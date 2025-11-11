# --- FORZAR DIRECTORIO DE TRABAJO ---
# Esto asegura que R sepa dónde buscar los archivos CSV y GeoJSON
tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) {
  message("Directorio de trabajo establecido (o ya era correcto).")
})
# -----------------------------------

# --- 0. Dependencias ---
library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(memoise)
library(rstudioapi) # Asegúrate de tenerlo instalado
library(leaflet)
library(sf)
library(stringr)
library(bsicons)

# --- 1. Definiciones Globales y Carga de Datos ---

# Columnas clave
COL_YEAR <- "Año"
COL_SECTOR <- "Sector IES"
COL_CARACTER <- "Caracter IES"
COL_DEPTO <- "Departamento de domicilio de la IES"
COL_NIVEL <- "Nivel Académico"
COL_MATRIC <- "Matriculados"
COL_GRAD <- "Graduados"
COL_IES <- "Institución de Educación Superior (IES)"
COL_SEXO <- "Sexo"

# Carga de datos del CSV (cacheada)
load_data <- memoise(function() {
  tryCatch({
    df <- read.csv("MEN_SNIES_test.csv", stringsAsFactors = TRUE, fileEncoding = "UTF-8-BOM")
    
    # Limpieza de nombres de columnas
    colnames(df)[colnames(df) == "Institución.de.Educación.Superior..IES."] <- COL_IES
    colnames(df)[colnames(df) == "Departamento.de.domicilio.de.la.IES"] <- COL_DEPTO
    colnames(df)[colnames(df) == "Nivel.Académico"] <- COL_NIVEL
    colnames(df)[colnames(df) == "Sector.IES"] <- COL_SECTOR
    colnames(df)[colnames(df) == "Caracter.IES"] <- COL_CARACTER
    
    df[[COL_MATRIC]] <- as.numeric(as.character(df[[COL_MATRIC]]))
    df[[COL_MATRIC]][is.na(df[[COL_MATRIC]])] <- 0
    df[[COL_GRAD]] <- as.numeric(df[[COL_GRAD]])
    df[[COL_YEAR]] <- as.numeric(df[[COL_YEAR]])
    
    return(df)
  }, error = function(e) {
    stop(paste("Error fatal al cargar 'MEN_SNIES_test.csv': ", e$message))
  })
})

data_snies <- load_data()

# --- Carga de Datos Geoespaciales ---

# Helper function (VERSIÓN FINAL Y ROBUSTA)
normalize_depto_names <- function(depto_name) {
  name <- stringr::str_to_upper(depto_name)
  # 1. Quitar acentos (ej. BOGOTÁ -> BOGOTA)
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  # 2. Quitar puntuación (ej. BOGOTA D.C. -> BOGOTA D C)
  name <- stringr::str_replace_all(name, "[^A-Z ]", "")
  # 3. Quitar prefijos comunes (ej. LA GUAJIRA -> GUAJIRA)
  name <- stringr::str_replace_all(name, "^LA ", "")
  name <- stringr::str_replace_all(name, "^EL ", "")
  # 4. Quitar sufijos comunes (ej. BOGOTA D C -> BOGOTA)
  name <- stringr::str_replace_all(name, " D C$", "")
  # 5. Quitar espacios dobles
  name <- stringr::str_squish(name)
  
  # 6. Recodificaciones manuales
  dplyr::recode(name,
                "VALLE" = "VALLE DEL CAUCA",
                "ARCHIPELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA" = "SAN ANDRES Y PROVIDENCIA",
                "BOGOTA" = "BOGOTÁ D.C.",  # POSIBLE FALTA
                .default = name  # Mantener otros nombres sin cambios
  )
}

# Carga del GeoJSON local (cacheado)
load_geojson_data <- memoise(function() {
  local_file <- "colombia.geo.json" 
  tryCatch({
    colombia_sf <- sf::st_read(local_file, quiet = TRUE)
    
    colombia_sf <- colombia_sf %>%
      mutate(
        Depto_Join_Key = normalize_depto_names(NOMBRE_DPT) # Asegúrate que tu columna se llama NOMBRE_DPT
      )
    
    return(colombia_sf)
  }, error = function(e) {
    stop(paste("Error al cargar 'colombia.geo.json'.",
               "Asegúrate de que el archivo esté en la misma carpeta que app.R. Error:", e$message))
  })
})

geojson_data <- load_geojson_data()

# Valores de filtros
choice_sektor <- c("Todos", as.character(unique(data_snies[[COL_SECTOR]])))
choice_nivel <- c("Todos", as.character(unique(data_snies[[COL_NIVEL]])))
choice_depto <- c("Todos", as.character(unique(data_snies[[COL_DEPTO]])))
min_year <- min(data_snies[[COL_YEAR]], na.rm = TRUE)
max_year <- max(data_snies[[COL_YEAR]], na.rm = TRUE)

# --- 2. Interfaz de Usuario (UI) ---
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "pulse",
    primary = "#007bff",
    secondary = "#6c757d"
  ),
  title = "Dashboard SNIES - Educación Superior",
  
  sidebar = sidebar(
    title = "Filtros Principales",
    sliderInput("range_year", "Rango de Años:", min = min_year, max = max_year, value = c(min_year, max_year), sep = ""),
    selectInput("filter_depto", "Departamento:", choices = choice_depto, selected = "Todos"),
    selectInput("filter_nivel", "Nivel Académico:", choices = choice_nivel, selected = "Todos"),
    selectInput("filter_sector", "Sector IES:", choices = choice_sektor, selected = "Todos"),
    selectInput("filter_caracter", "Caracter IES:", choices = c("Todos" = "Todos"), selected = "Todos"),
    bslib::tooltip(
      selectInput("filter_metodologia", "Metodología:", 
                  choices = c("Todos", as.character(unique(data_snies$Metodología))),
                  selected = "Todos"),
      "Filtrar por modalidad del programa"
    )
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "Total Matriculados", value = textOutput("kpi_matriculados"), showcase = bsicons::bs_icon("people-fill"), theme = "primary"),
    value_box(title = "Total Graduados", value = textOutput("kpi_graduados"), showcase = bsicons::bs_icon("mortarboard-fill"), theme = "secondary"),
    value_box(title = "Instituciones (IES) Únicas", value = textOutput("kpi_ies"), showcase = bsicons::bs_icon("building"), theme = "info")
  ),
  
  navset_tab(
    nav_panel(
      title = "Resumen y Tendencias",
      layout_columns(
        col_widths = c(12, 12),
        row_heights = c(1, 2),
        card(
          card_header("Evolución de Matriculados y Graduados (por Año)"),
          card_body(shinycssloaders::withSpinner(plotlyOutput("plot_tendencia"), type = 6, color = "#007bff"))
        ),
        card(
          card_header("Distribución por Nivel Académico y Sexo"),
          card_body(shinycssloaders::withSpinner(plotlyOutput("plot_distribucion_sexo"), type = 6, color = "#007bff"))
        )
      )
    ),
    nav_panel(
      title = "Análisis Geográfico",
      card(
        card_header("Distribución Geográfica de IES"),
        card_body(
          p("El mapa muestra el número de Instituciones (IES) únicas activas según los filtros (excepto Departamento)."),
          shinycssloaders::withSpinner(leafletOutput("mapa_colombia", height = "600px"), type = 6, color = "#007bff")
        )
      )
    ),
    nav_panel(
      title = "Análisis Detallado",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Matriculados por Sector y Caracter IES (Treemap)"),
          card_body(shinycssloaders::withSpinner(plotlyOutput("plot_treemap_sector"), type = 6, color = "#007bff"))
        ),
        card(
          card_header("Matriculados vs. Graduados (por IES)"),
          card_body(
            shinycssloaders::withSpinner(plotlyOutput("plot_scatter_ies"), type = 6, color = "#007bff"),
            p(class = "text-muted", "Cada punto representa una institución. El tamaño indica el total de programas.")
          )
        )
      )
    ),
    nav_panel(
      title = "Datos y Reportes",
      card(
        card_header("Datos Filtrados"),
        card_body(
          downloadButton("download_report_csv", "Descargar CSV Filtrado"),
          hr(),
          shinycssloaders::withSpinner(DT::dataTableOutput("tabla_datos"), type = 6, color = "#007bff")
        )
      )
    )
  )
)

# --- 3. Lógica del Servidor ---
server <- function(input, output, session) {
  
  # --- A. Lógica de Filtros en Cascada ---
  caracter_options <- reactiveVal(c("Todos" = "Todos"))
  observeEvent(input$filter_sector, {
    if (input$filter_sector == "Todos") {
      new_options <- c("Todos", as.character(unique(data_snies[[COL_CARACTER]])))
    } else {
      new_options <- data_snies %>%
        filter(.data[[COL_SECTOR]] == input$filter_sector) %>%
        pull(.data[[COL_CARACTER]]) %>%
        unique() %>%
        as.character()
      new_options <- c("Todos", new_options)
    }
    caracter_options(new_options)
    updateSelectInput(session, "filter_caracter", choices = caracter_options(), selected = "Todos")
  })
  
  # --- B. Reactividad Principal (Datos Filtrados para Gráficos y Tabla) ---
  filtered_data <- reactive({
    df_filtered <- data_snies
    df_filtered <- df_filtered %>%
      filter(.data[[COL_YEAR]] >= input$range_year[1] & .data[[COL_YEAR]] <= input$range_year[2])
    
    if (input$filter_depto != "Todos") {
      df_filtered <- df_filtered %>% filter(.data[[COL_DEPTO]] == input$filter_depto)
    }
    if (input$filter_nivel != "Todos") {
      df_filtered <- df_filtered %>% filter(.data[[COL_NIVEL]] == input$filter_nivel)
    }
    if (input$filter_sector != "Todos") {
      df_filtered <- df_filtered %>% filter(.data[[COL_SECTOR]] == input$filter_sector)
    }
    if (input$filter_caracter != "Todos") {
      df_filtered <- df_filtered %>% filter(.data[[COL_CARACTER]] == input$filter_caracter)
    }
    if (input$filter_metodologia != "Todos") {
      df_filtered <- df_filtered %>% filter(Metodología == input$filter_metodologia)
    }
    return(df_filtered)
  })
  
  data_reactive <- reactive(filtered_data()) %>% debounce(500)  
  
  # --- C. Salidas de KPIs (Value Boxes) ---
  format_kpi <- function(num) {
    if (num >= 1e6) { paste0(round(num / 1e6, 1), " M") } 
    else if (num >= 1e3) { paste0(round(num / 1e3, 0), " K") } 
    else { as.character(num) }
  }
  output$kpi_matriculados <- renderText({ format_kpi(sum(data_reactive()[[COL_MATRIC]], na.rm = TRUE)) })
  output$kpi_graduados <- renderText({ format_kpi(sum(data_reactive()[[COL_GRAD]], na.rm = TRUE)) })
  output$kpi_ies <- renderText({ n_distinct(data_reactive()[[COL_IES]]) })
  
  # --- D. Salidas de Gráficos (Plotly) ---
  
  # Gráfico 1: Tendencia
  output$plot_tendencia <- renderPlotly({
    validate(
      need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados."),
      need(sum(data_reactive()[[COL_MATRIC]], na.rm = TRUE) > 0, 
           "No hay datos de matriculados en el rango seleccionado.")
    )
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_YEAR]]) %>%
      summarise(Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE), Graduados = sum(.data[[COL_GRAD]], na.rm = TRUE)) %>%
      tidyr::gather(key = "Metrica", value = "Total", -all_of(COL_YEAR))
    
    p <- ggplot(plot_data, aes(x = .data[[COL_YEAR]], y = Total, color = Metrica, text = paste("Año:", .data[[COL_YEAR]], "<br>Total:", Total))) +
      geom_line(linewidth = 1) + geom_point() + scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(title = NULL, x = "Año", y = "Total Personas", color = "Métrica") + theme_minimal() + theme(legend.position = "top")
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  }) %>% bindCache(data_reactive())
  
  # Gráfico 2: Distribución Sexo
  output$plot_distribucion_sexo <- renderPlotly({
    validate(need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados."))
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_NIVEL]], .data[[COL_SEXO]]) %>%
      summarise(Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(plot_data, aes(x = .data[[COL_NIVEL]], y = Matriculados, fill = .data[[COL_SEXO]],
                               text = paste("Nivel:", .data[[COL_NIVEL]], "<br>Sexo:", .data[[COL_SEXO]], "<br>Matriculados:", Matriculados))) +
      geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
      labs(title = NULL, x = "Nivel Académico", y = "Total Matriculados", fill = "Sexo") + theme_minimal()
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  }) %>% bindCache(data_reactive())
  
  # Gráfico 3: Treemap
  output$plot_treemap_sector <- renderPlotly({
    validate(need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados."))
    df_children <- data_reactive() %>%
      group_by(.data[[COL_SECTOR]], .data[[COL_CARACTER]]) %>%
      summarise(Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE), .groups = 'drop') %>%
      filter(Matriculados > 0) %>%
      rename(label = .data[[COL_CARACTER]], parent = .data[[COL_SECTOR]], value = Matriculados)
    df_parents <- df_children %>% group_by(parent) %>% summarise(value = sum(value), .groups = 'drop') %>% rename(label = parent) %>% mutate(parent = "Total")
    df_root <- data.frame(label = "Total", parent = "", value = sum(df_parents$value))
    plot_data <- bind_rows(df_root, df_parents, df_children)
    
    plot_ly(data = plot_data, type = "treemap", labels = ~label, parents = ~parent, values = ~value,
            branchvalues = "total", textinfo = "label+value+percent parent", hoverinfo = "text",
            text = ~paste("Nivel:", label, "<br>Padre:", parent, "<br>Matriculados:", scales::comma(value, accuracy = 1))) %>%
      layout(margin = list(l=0, r=0, b=0, t=0)) %>% config(displaylogo = FALSE)
  }) %>% bindCache(data_reactive())
  
  # Gráfico 4: Scatter
  output$plot_scatter_ies <- renderPlotly({
    validate(need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados."))
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_IES]]) %>%
      summarise(
        Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE),
        Graduados = sum(.data[[COL_GRAD]], na.rm = TRUE),
        Programas = n_distinct(Programa.Académico), # Usa Programa.Académico por la limpieza de read.csv
        .groups = 'drop'
      ) %>% filter(Matriculados > 0 | Graduados > 0)
    
    p <- ggplot(plot_data, aes(x = Matriculados, y = Graduados, size = Programas,
                               text = paste("IES:", .data[[COL_IES]], "<br>Matriculados:", scales::comma(Matriculados),
                                            "<br>Graduados:", scales::comma(Graduados), "<br>Programas:", Programas))) +
      geom_point(alpha = 0.6) + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
      labs(title = NULL, x = "Total Matriculados", y = "Total Graduados") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  }) %>% bindCache(data_reactive())
  
  # --- E. Lógica del Mapa (Corregida) ---
  
  # 1. Reactive específico para el mapa (ignora filtro de depto)
  map_filtered_data <- reactive({
    df_filtered <- data_snies
    df_filtered <- df_filtered %>%
      filter(.data[[COL_YEAR]] >= input$range_year[1] & .data[[COL_YEAR]] <= input$range_year[2])
    
    if (input$filter_nivel != "Todos") { df_filtered <- df_filtered %>% filter(.data[[COL_NIVEL]] == input$filter_nivel) }
    if (input$filter_sector != "Todos") { df_filtered <- df_filtered %>% filter(.data[[COL_SECTOR]] == input$filter_sector) }
    if (input$filter_caracter != "Todos") { df_filtered <- df_filtered %>% filter(.data[[COL_CARACTER]] == input$filter_caracter) }
    if (input$filter_metodologia != "Todos") { df_filtered <- df_filtered %>% filter(Metodología == input$filter_metodologia) }
    
    return(df_filtered)
  }) %>% debounce(500)
  
  # 2. Reactive que une los datos con el mapa
  map_data_reactive <- reactive({
    
    # He quitado el 'validate()' de aquí
    
    df_filtered <- map_filtered_data()
    
    # Si los filtros no devuelven datos, devuelve NULL
    if (nrow(df_filtered) == 0) {
      return(NULL) 
    }
    
    # El resto de la función es igual
    data_summary <- df_filtered %>%
      group_by(.data[[COL_DEPTO]]) %>%
      summarise(
        IES_Unicas = n_distinct(.data[[COL_IES]]),
        Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE),
        Graduados = sum(.data[[COL_GRAD]], na.rm = TRUE),
        .groups = 'drop'
      )
    data_summary <- data_summary %>% mutate(Depto_Join_Key = normalize_depto_names(.data[[COL_DEPTO]]))
    map_data <- geojson_data %>% left_join(data_summary, by = "Depto_Join_Key")
    map_data <- map_data %>%
      mutate(
        IES_Unicas = ifelse(is.na(IES_Unicas), 0, IES_Unicas),
        Matriculados = ifelse(is.na(Matriculados), 0, Matriculados)
      )
    return(map_data)
  })
  
  # 3. Renderizar el lienzo base del mapa
  output$mapa_colombia <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.297333, lat = 4.570868, zoom = 5)
  })
  
  # 4. Observador para ACTUALIZAR los colores del mapa
  observeEvent(map_data_reactive(), {
    
    map_data <- map_data_reactive()
    
    # ¡ESTA ES LA LÍNEA NUEVA E IMPORTANTE!
    # Si los datos son NULL (por el filtro) o no hay IES, limpia el mapa y sal.
    if(is.null(map_data) || nrow(map_data) == 0 || all(map_data$IES_Unicas == 0)) {
      leafletProxy("mapa_colombia") %>% clearShapes() %>% clearControls()
      return()
    }
    
    # El resto de la función es igual
    pal <- colorNumeric(
      palette = "Blues",  # Paleta Azul
      domain = map_data$IES_Unicas
    )
    labels <- sprintf(
      "<strong>%s</strong><br/>IES Únicas: %s<br/>Matriculados: %s",
      map_data$NOMBRE_DPT, # (Nombre de columna de tu GeoJSON)
      map_data$IES_Unicas,
      scales::comma(map_data$Matriculados, accuracy = 1)
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("mapa_colombia", data = map_data) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor = ~pal(IES_Unicas),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~IES_Unicas, opacity = 0.7, title = "IES Únicas", position = "bottomright")
  }, ignoreNULL = FALSE)
  
  # 5. Observador para el ZOOM del mapa (Corregido para jsonlite)
  observeEvent(input$filter_depto, {
    if (input$filter_depto == "Todos") {
      leafletProxy("mapa_colombia") %>%
        setView(lng = -74.297333, lat = 4.570868, zoom = 5)
    } else {
      tryCatch({
        selected_key <- normalize_depto_names(input$filter_depto)
        selected_polygon <- geojson_data %>% filter(Depto_Join_Key == selected_key)
        
        if (nrow(selected_polygon) > 0) {
          bbox <- sf::st_bbox(selected_polygon)
          leafletProxy("mapa_colombia") %>%
            fitBounds(
              lng1 = bbox[["xmin"]], # Corrección para jsonlite
              lat1 = bbox[["ymin"]], # Corrección para jsonlite
              lng2 = bbox[["xmax"]], # Corrección para jsonlite
              lat2 = bbox[["ymax"]]  # Corrección para jsonlite
            )
        }
      }, error = function(e) {
        showNotification(paste("No se pudo hacer zoom:", e$message), type = "warning")
      })
    }
  })
  
  # --- F. Salida de Tabla (DT) ---
  output$tabla_datos <- DT::renderDataTable({
    data_display <- data_reactive() %>%
      select(all_of(COL_IES), `Programa Académico`, all_of(COL_NIVEL), Metodología, 
             all_of(COL_YEAR), Semestre, all_of(COL_MATRIC), all_of(COL_GRAD))
    
    DT::datatable(
      data_display,
      rownames = FALSE, filter = 'top', extensions = c('Buttons', 'Responsive'),
      options = list(
        pageLength = 10, autoWidth = TRUE, dom = 'Bfrtip', buttons = list(list(extend = 'colvis', text = 'Seleccionar Columnas')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json')
      )
    )
  })
  
  # --- G. Lógica de Descarga de Reportes ---
  output$download_report_csv <- downloadHandler(
    filename = function() { paste0("reporte_snies_filtrado_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(data_reactive(), file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
}

# --- 4. Ejecutar la Aplicación ---
shinyApp(ui, server)