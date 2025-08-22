library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(viridis)
library(spOccupancy)
library(vegan)
library(tidyr)

# =============================================
# CONFIGURACIÓN INICIAL Y CARGA DE DATOS
# =============================================

# Función para cargar datos (todos los módulos los usarán)
cargar_datos <- function() {
  # Nota: Los usuarios deberán colocar sus archivos en el directorio 'data/'
  # dentro del directorio de la aplicación
  
  # Cargar datos de aves
  datos <- read_excel("data/DarwinCore Final - Avifauna BPUN 547.xlsx",
                      sheet = "Plantilla", skip = 1) %>%
    clean_names()
  
  # Cargar shapefile del campus
  campus <- st_read("data/campus_unal.shp", quiet = TRUE) %>%
    st_transform(4326) %>%
    filter(!st_is_empty(geometry)) %>%
    st_make_valid()
  
  # Procesar datos de aves
  aves.datos <- datos %>%
    select(
      numero_de_individuos,
      ano,
      mes,
      dia,
      latitud_decimal,
      longitud_decimal,
      epiteto_especifico,
      familia
    ) %>%
    mutate(
      longitud = as.numeric(longitud_decimal),
      latitud = as.numeric(latitud_decimal),
      epiteto_especifico = ifelse(
        is.na(epiteto_especifico) | epiteto_especifico == "",
        "Sin identificar",
        trimws(epiteto_especifico)
      )
    ) %>%
    filter(!is.na(latitud), !is.na(longitud))
  
  # Convertir a objeto espacial
  aves.sf <- aves.datos %>%
    st_as_sf(coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)
  
  # Filtrar puntos dentro del campus
  aves.sf <- aves.sf[st_within(aves.sf, campus, sparse = FALSE), ]
  
  return(list(
    aves.datos = aves.datos,
    aves.sf = aves.sf,
    campus = campus
  ))
}

# Cargar datos al iniciar la aplicación
datos_lista <- cargar_datos()
aves.datos <- datos_lista$aves.datos
aves.sf <- datos_lista$aves.sf
campus <- datos_lista$campus

# =============================================
# DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
# =============================================

ui <- navbarPage(
  title = "Análisis de Avifauna - UNAL Sede La Paz",
  theme = shinythemes::shinytheme("flatly"),
  
  # Pestaña 1: Mapa de Observaciones
  tabPanel(
    "Mapa de Observaciones",
    icon = icon("map-marker-alt"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Filtros de Observaciones"),
        selectInput("familia1", "Selecciona familia:",
                    choices = c("Todas", sort(unique(aves.datos$familia)))),
        selectInput("especie1", "Selecciona especie:",
                    choices = c("Todas", sort(unique(aves.datos$epiteto_especifico)))),
        selectInput("mes1", "Selecciona mes:",
                    choices = c("Todos", sort(unique(aves.datos$mes)))),
        selectInput("anio1", "Selecciona año:",
                    choices = c("Todos", sort(unique(aves.datos$ano)))),
        checkboxInput("heatmap", "Mostrar mapa de calor", value = FALSE),
        hr(),
        helpText("Esta pestaña muestra observaciones individuales de aves con opción de mapa de calor.")
      ),
      mainPanel(
        leafletOutput("mapaObservaciones", height = 600)
      )
    )
  ),
  
  # Pestaña 2: Mapa de Abundancia
  tabPanel(
    "Mapa de Abundancia",
    icon = icon("layer-group"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Selección de Especie"),
        selectInput(
          "especie2",
          "Selecciona especie:",
          choices = sort(unique(aves.datos$epiteto_especifico)),
          selected = sort(unique(aves.datos$epiteto_especifico))[1]
        ),
        hr(),
        helpText("Esta pestaña muestra la abundancia de una especie específica en una cuadrícula sobre el campus.")
      ),
      mainPanel(
        leafletOutput("mapaAbundancia", height = 600)
      )
    )
  ),
  
  # Pestaña 3: Modelo de Ocupación
  tabPanel(
    "Modelo de Ocupación",
    icon = icon("chart-line"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parámetros del Modelo"),
        selectInput("familia3", "Selecciona familia:",
                    choices = c("Todas", sort(unique(aves.datos$familia)))),
        selectInput("especie3", "Selecciona especie:",
                    choices = c("Selecciona una familia primero")),
        selectInput("mes3", "Selecciona mes:",
                    choices = c("Todos", sort(unique(aves.datos$mes)))),
        actionButton("calcular", "Calcular Probabilidad", class = "btn-primary"),
        hr(),
        h4("Resultados del Modelo:"),
        verbatimTextOutput("modelo_summary")
      ),
      mainPanel(
        leafletOutput("mapaOcupacion", height = 400),
        plotOutput("prob_plot", height = 200)
      )
    )
  ),
  
  # Pestaña 4: Riqueza y Diversidad
  tabPanel(
    "Riqueza y Diversidad",
    icon = icon("seedling"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Opciones de Visualización"),
        selectInput("metrica", "Selecciona métrica:",
                    choices = c("Riqueza" = "Riqueza", 
                                "Diversidad de Shannon" = "Diversidad_Shannon")),
        hr(),
        helpText("Esta pestaña muestra métricas de biodiversidad calculadas para una cuadrícula sobre el campus.")
      ),
      mainPanel(
        leafletOutput("mapaRiqueza", height = 600)
      )
    )
  ),
  
  # Pestaña de Instrucciones
  tabPanel(
    "Instrucciones",
    icon = icon("info-circle"),
    fluidPage(
      h2("Instrucciones de Uso"),
      p("Esta aplicación fue creada para permitir analizar datos de avifauna del campus UNAL Sede La Paz."),
      h3("Requisitos:"),
      p("Para usar esta aplicación, necesitas tener los siguientes archivos en la carpeta 'data/':"),
      tags$ul(
        tags$li("DarwinCore Final - Avifauna BPUN 547.xlsx - Base de datos de observaciones de aves"),
        tags$li("campus_unal.shp - Shapefile del campus (y archivos asociados .shx, .dbf, etc.)")
      ),
      h3("Funcionalidades:"),
      tags$ul(
        tags$li(tags$b("Mapa de Observaciones:"), " Visualiza observaciones individuales con filtros y opción de mapa de calor."),
        tags$li(tags$b("Mapa de Abundancia:"), " Muestra la distribución espacial de abundancia por especie."),
        tags$li(tags$b("Modelo de Ocupación:"), " Calcula probabilidades de ocupación y detección usando modelos bayesianos."),
        tags$li(tags$b("Riqueza y Diversidad:"), " Visualiza métricas de biodiversidad en una cuadrícula.")
      ),
      h3("Para investigadores:"),
      p("Puedes modificar el código fuente para adaptarlo a tus necesidades específicas."),
      p("El código está disponible en: ", 
        tags$a(href = "https://github.com/tu_usuario/tu_repositorio", "GitHub"))
    )
  )
)

# =============================================
# DEFINICIÓN DEL SERVIDOR (SERVER)
# =============================================

server <- function(input, output, session) {
  
  # =============================================
  # Pestaña 1: Mapa de Observaciones
  # =============================================
  
  # Actualizar opciones de especies según familia seleccionada
  observeEvent(input$familia1, {
    if(input$familia1 == "Todas") {
      especies_filtradas <- sort(unique(aves.datos$epiteto_especifico))
    } else {
      especies_filtradas <- aves.datos %>%
        filter(familia == input$familia1) %>%
        pull(epiteto_especifico) %>%
        unique() %>%
        sort()
    }
    updateSelectInput(session, "especie1", choices = c("Todas", especies_filtradas))
  })
  
  # Filtrado reactivo para pestaña 1
  datos_filtrados1 <- reactive({
    df <- aves.sf
    
    if (input$familia1 != "Todas") {
      df <- df %>% filter(familia == input$familia1)
    }
    if (input$especie1 != "Todas") {
      df <- df %>% filter(epiteto_especifico == input$especie1)
    }
    if (input$mes1 != "Todos") {
      df <- df %>% filter(mes == as.numeric(input$mes1))
    }
    if (input$anio1 != "Todos") {
      df <- df %>% filter(ano == as.numeric(input$anio1))
    }
    
    df
  })
  
  # Mapa base para pestaña 1
  output$mapaObservaciones <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = campus,
                  fillColor = "transparent",
                  color = "red",
                  weight = 2) %>%
      setView(lng = -73.1985, lat = 10.3939, zoom = 17)
  })
  
  # Actualización dinámica del mapa de observaciones
  observe({
    aves <- datos_filtrados1()
    
    leafletProxy("mapaObservaciones") %>%
      clearMarkers() %>%
      clearHeatmap()
    
    if (nrow(aves) == 0) return()
    
    if (input$heatmap) {
      leafletProxy("mapaObservaciones") %>%
        addHeatmap(
          lng = aves$longitud,
          lat = aves$latitud,
          intensity = (aves$numero_de_individuos)*1.5,
          radius = 50,
          blur = 20,
          max = max(aves$numero_de_individuos, na.rm = TRUE),
          gradient = c(
            "0.0" = "transparent",
            "0.2" = "skyblue",
            "0.4" = "limegreen",
            "0.6" = "yellow",
            "0.8" = "orange",
            "1.0" = "red"
          )
        )
    } else {
      leafletProxy("mapaObservaciones") %>%
        addCircleMarkers(
          lng = aves$longitud,
          lat = aves$latitud,
          radius = 5,
          color = "orange",
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = paste0(
            "<b>Especie:</b> ", aves$epiteto_especifico, "<br>",
            "<b>Familia:</b> ", aves$familia, "<br>",
            "<b>Fecha:</b> ", aves$dia, "/", aves$mes, "/", aves$ano
          )
        )
    }
  })
  
  # =============================================
  # Pestaña 2: Mapa de Abundancia
  # =============================================
  
  # Preprocesar datos para el mapa de abundancia (solo una vez)
  abundancia_data <- reactive({
    # Transformar campus a UTM
    campus_utm <- st_transform(campus, 32618) %>% st_make_valid()
    
    # Crear cuadrícula
    grid <- st_make_grid(campus_utm, cellsize = 100, square = TRUE)
    grid_sf <- st_sf(ID = 1:length(grid), geometry = grid)
    grid_campus <- st_intersection(grid_sf, campus_utm)
    
    # Convertir aves a UTM y unir con cuadrícula
    aves_sf_utm <- st_transform(aves.sf, 32618)
    aves_join <- st_join(aves_sf_utm, grid_campus, left = FALSE)
    
    # Calcular abundancia
    abundancia_tabla <- aves_join %>%
      st_drop_geometry() %>%
      group_by(ID, epiteto_especifico) %>%
      summarise(Total_individuos = sum(numero_de_individuos, na.rm = TRUE), .groups = "drop")
    
    # Unir con cuadrícula y transformar a WGS84
    abundancia <- abundancia_tabla %>%
      left_join(grid_campus, by = "ID") %>%
      st_as_sf() %>%
      st_transform(4326)
    
    return(abundancia)
  })
  
  # Filtrar datos por especie seleccionada
  datos_filtrados2 <- reactive({
    abundancia_data() %>% filter(epiteto_especifico == input$especie2)
  })
  
  # Mapa de abundancia
  output$mapaAbundancia <- renderLeaflet({
    datos <- datos_filtrados2()
    
    if (nrow(datos) == 0) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               addPolygons(data = campus, fill = NA, color = "black", weight = 1.5) %>%
               setView(lng = -73.1985, lat = 10.3939, zoom = 16))
    }
    
    pal <- colorNumeric("viridis", domain = datos$Total_individuos)
    
    leaflet(datos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Total_individuos),
        color = "grey40",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0("Celda: ", ID, "<br>Individuos: ", Total_individuos) %>% lapply(htmltools::HTML)
      ) %>%
      addPolygons(
        data = campus,
        fill = NA, color = "black", weight = 1.5
      ) %>%
      addLegend(
        pal = pal,
        values = datos$Total_individuos,
        title = paste("Individuos de", input$especie2)
      )
  })
  
  # =============================================
  # Pestaña 3: Modelo de Ocupación
  # =============================================
  
  # Actualizar opciones de especies según familia seleccionada
  observeEvent(input$familia3, {
    if(input$familia3 == "Todas") {
      especies <- sort(unique(aves.datos$epiteto_especifico))
    } else {
      especies <- sort(unique(aves.datos$epiteto_especifico[aves.datos$familia == input$familia3]))
    }
    updateSelectInput(session, "especie3", choices = especies)
  })
  
  # Filtrar datos para pestaña 3
  datos_filtrados3 <- reactive({
    df <- aves.sf
    
    if(input$familia3 != "Todas") {
      df <- df %>% filter(familia == input$familia3)
    }
    if(input$especie3 != "Selecciona una familia primero") {
      df <- df %>% filter(epiteto_especifico == input$especie3)
    }
    if(input$mes3 != "Todos") {
      df <- df %>% filter(mes == as.numeric(input$mes3))
    }
    
    df
  })
  
  # Función para preparar datos de ocupación
  preparar_datos_ocupacion <- function(especie_seleccionada) {
    # Crear base completa con todas las combinaciones
    base_completa <- expand.grid(
      sitio = unique(paste(aves.datos$latitud, aves.datos$longitud, sep="_")),
      visita = unique(paste(aves.datos$mes, aves.datos$dia, sep="_")),
      especie = especie_seleccionada,
      stringsAsFactors = FALSE
    )
    
    # Unir con datos reales
    datos_ocupacion <- aves.datos %>%
      mutate(
        sitio = paste(latitud, longitud, sep="_"),
        visita = paste(mes, dia, sep="_"),
        deteccion = ifelse(numero_de_individuos > 0, 1, 0)
      ) %>%
      right_join(base_completa, by = c("sitio", "visita", "epiteto_especifico" = "especie")) %>%
      mutate(deteccion = ifelse(is.na(deteccion), 0, deteccion))
    
    # Convertir a formato para spOccupancy
    matriz_deteccion <- datos_ocupacion %>%
      select(sitio, visita, deteccion) %>%
      tidyr::pivot_wider(names_from = visita, values_from = deteccion, values_fill = 0) %>%
      arrange(sitio)
    
    # Extraer coordenadas
    coords <- matriz_deteccion %>%
      separate(sitio, into = c("lat", "lon"), sep = "_", convert = TRUE) %>%
      select(lon, lat) %>%
      as.matrix()
    
    # Matriz de detección (sin columna de sitio)
    y <- as.matrix(matriz_deteccion[, -1])
    
    list(y = y, coords = coords)
  }
  
  # Mapa base para pestaña 3
  output$mapaOcupacion <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = campus,
                  color = "blue",
                  weight = 2,
                  fillOpacity = 0.1) %>%
      setView(lng = mean(aves.datos$longitud, na.rm = TRUE), 
              lat = mean(aves.datos$latitud, na.rm = TRUE), 
              zoom = 16)
  })
  
  # Modelo de ocupación
  modelo <- eventReactive(input$calcular, {
    req(input$especie3)
    
    datos_ocup <- preparar_datos_ocupacion(input$especie3)
    
    if(nrow(datos_ocup$y) < 2 || sum(datos_ocup$y) == 0) {
      return(NULL)
    }
    
    tryCatch({
      PGOcc(
        occ.formula = ~1,
        det.formula = ~1,
        data = datos_ocup,
        n.samples = 1000,
        n.thin = 1,
        n.burn = 500,
        n.chains = 1,
        verbose = FALSE
      )
    }, error = function(e) {
      message("Error ajustando modelo: ", e$message)
      return(NULL)
    })
  })
  
  # Actualizar mapa con resultados del modelo
  observe({
    df <- datos_filtrados3()
    
    leafletProxy("mapaOcupacion") %>%
      clearMarkers()
    
    if(nrow(df) > 0) {
      leafletProxy("mapaOcupacion") %>%
        addCircleMarkers(
          data = df,
          radius = 5,
          color = ifelse(df$numero_de_individuos > 0, "red", "gray"),
          fillOpacity = 0.7,
          popup = paste(
            "<b>Especie:</b>", df$epiteto_especifico, "<br>",
            "<b>Fecha:</b>", df$dia, "/", df$mes, "/", df$ano, "<br>",
            "<b>Individuos:</b>", df$numero_de_individuos
          )
        )
    }
  })
  
  # Resumen del modelo
  output$modelo_summary <- renderPrint({
    mod <- modelo()
    
    if(is.null(mod)) {
      cat("No se pudo ajustar el modelo. Razones posibles:\n")
      cat("- No hay suficientes datos para la especie seleccionada\n")
      cat("- No hay detecciones positivas\n")
    } else {
      cat("Probabilidad de ocupación (psi):", round(mean(mod$psi.samples), 3), "\n")
      cat("Probabilidad de detección (p):", round(mean(mod$p.samples), 3), "\n")
      cat("\nResumen del modelo:\n")
      summary(mod)
    }
  })
  
  # Gráfico de probabilidades
  output$prob_plot <- renderPlot({
    mod <- modelo()
    
    if(!is.null(mod)) {
      par(mfrow = c(1, 2))
      hist(mod$psi.samples, main = "Probabilidad de Ocupación", 
           xlab = "psi", col = "lightblue")
      hist(mod$p.samples, main = "Probabilidad de Detección", 
           xlab = "p", col = "lightgreen")
    }
  })
  
  # =============================================
  # Pestaña 4: Riqueza y Diversidad
  # =============================================
  
  # Preprocesar datos de riqueza y diversidad (solo una vez)
  riqueza_data <- reactive({
    # Transformar a UTM
    campus_utm <- st_transform(campus, 32618)
    aves_sf_utm <- st_transform(aves.sf, 32618)
    
    # Crear cuadrícula
    grid <- st_make_grid(campus_utm, cellsize = 100, square = TRUE)
    grid_sf <- st_sf(ID = 1:length(grid), geometry = grid)
    grid_campus <- st_intersection(grid_sf, campus_utm)
    
    # Unir aves con cuadrícula
    aves_join <- st_join(aves_sf_utm, grid_campus, left = FALSE)
    
    # Crear matriz de abundancia (celdas x especies)
    tabla_abundancia <- aves_join %>%
      st_drop_geometry() %>%
      group_by(ID, epiteto_especifico) %>%
      summarise(Total_individuos = sum(numero_de_individuos, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(
        names_from = epiteto_especifico,
        values_from = Total_individuos,
        values_fill = 0
      )
    
    # Calcular riqueza y diversidad con vegan
    tabla_abundancia <- tabla_abundancia %>%
      mutate(
        Riqueza = specnumber(select(., -ID)),
        Diversidad_Shannon = diversity(select(., -ID), index = "shannon")
      )
    
    # Unir tabla a cuadrícula y transformar a WGS84
    riqueza_sf <- grid_campus %>%
      left_join(tabla_abundancia, by = "ID") %>%
      st_transform(4326)
    
    return(riqueza_sf)
  })
  
  # Mapa de riqueza/diversidad
  output$mapaRiqueza <- renderLeaflet({
    datos <- riqueza_data()
    metrica <- input$metrica
    
    if (all(is.na(datos[[metrica]]))) {
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               addPolygons(data = campus, fill = NA, color = "black", weight = 1.5) %>%
               setView(lng = -73.1985, lat = 10.3939, zoom = 16))
    }
    
    pal <- colorNumeric("viridis", domain = datos[[metrica]])
    
    leaflet(datos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(get(metrica)),
        weight = 1,
        color = "grey40",
        fillOpacity = 0.7,
        label = ~paste("ID:", ID, "<br>", metrica, ":", round(get(metrica), 2))
      ) %>%
      addPolygons(
        data = campus,
        fill = NA, color = "black", weight = 1.5
      ) %>%
      addLegend(
        pal = pal,
        values = datos[[metrica]],
        title = metrica
      )
  })
}

# =============================================
# EJECUTAR LA APLICACIÓN
# =============================================

shinyApp(ui, server)