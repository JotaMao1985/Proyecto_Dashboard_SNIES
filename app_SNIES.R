# --- 0. Dependencias ---
# (Asegúrate de instalar estos paquetes: install.packages(c("shiny", "bslib", "pandas", "plotly", "DT", "shinycssloaders", "memoise")))

library(shiny)
library(bslib)          # Para el framework visual moderno (Bootstrap 5)
library(dplyr)          # Para manipulación de datos
library(plotly)         # Para gráficos interactivos
library(DT)             # Para tablas interactivas
library(shinycssloaders)  # Para spinners de carga
library(memoise)        # Para caching de la carga de datos


# --- 1. Definiciones Globales y Carga de Datos ---

# Columnas clave (para evitar errores tipográficos)
COL_YEAR <- "Año"
COL_SECTOR <- "Sector IES"
COL_CARACTER <- "Caracter IES"
COL_DEPTO <- "Departamento de domicilio de la IES"
COL_NIVEL <- "Nivel Académico"
COL_MATRIC <- "Matriculados"
COL_GRAD <- "Graduados"
COL_IES <- "Institución de Educación Superior (IES)"
COL_SEXO <- "Sexo"

# Usamos memoise para cachear la carga del CSV.
# Esto asegura que el archivo se lea de disco solo una vez,
# sin importar cuántos usuarios se conecten.
load_data <- memoise(function() {
  # Usamos tryCatch para un manejo de errores robusto en la carga inicial
  tryCatch({
    df <- read.csv("MEN_SNIES_test.csv", stringsAsFactors = TRUE, fileEncoding = "UTF-8-BOM")
    
    # Limpieza básica de nombres para facilitar el uso
    # (En un proyecto real, esto sería más extenso)
    colnames(df)[colnames(df) == "Institución.de.Educación.Superior..IES."] <- COL_IES
    colnames(df)[colnames(df) == "Departamento.de.domicilio.de.la.IES"] <- COL_DEPTO
    colnames(df)[colnames(df) == "Nivel.Académico"] <- COL_NIVEL
    colnames(df)[colnames(df) == "Sector.IES"] <- COL_SECTOR
    colnames(df)[colnames(df) == "Caracter.IES"] <- COL_CARACTER
    
    # Asegurar tipos de datos correctos
    df[[COL_MATRIC]] <- as.numeric(df[[COL_MATRIC]])
    df[[COL_GRAD]] <- as.numeric(df[[COL_GRAD]])
    df[[COL_YEAR]] <- as.numeric(df[[COL_YEAR]])
    
    return(df)
  }, error = function(e) {
    # Si el archivo no carga, detenemos la app con un mensaje claro
    stop(paste("Error fatal al cargar los datos: ", e$message))
  })
})

# Cargar los datos al iniciar la app
data_snies <- load_data()

# Obtener valores únicos para los filtros
choice_sektor <- c("Todos", as.character(unique(data_snies[[COL_SECTOR]])))
choice_nivel <- c("Todos", as.character(unique(data_snies[[COL_NIVEL]])))
choice_depto <- c("Todos", as.character(unique(data_snies[[COL_DEPTO]])))
min_year <- min(data_snies[[COL_YEAR]], na.rm = TRUE)
max_year <- max(data_snies[[COL_YEAR]], na.rm = TRUE)

# --- 2. Interfaz de Usuario (UI) ---
# Usamos bslib::page_sidebar para un layout moderno y responsive
ui <- page_sidebar(
  
  # Definición del Tema (paleta de colores accesible)
  theme = bs_theme(
    version = 5,
    bootswatch = "pulse", # Un tema limpio y profesional
    primary = "#007bff",
    secondary = "#6c757d",
    bg = "#FFFFFF",
    fg = "#212529"
  ),
  
  title = "Dashboard SNIES - Educación Superior",
  
  # --- Barra Lateral (Filtros) ---
  sidebar = sidebar(
    title = "Filtros Principales",
    
    sliderInput(
      "range_year",
      "Rango de Años:",
      min = min_year,
      max = max_year,
      value = c(min_year, max_year),
      sep = ""
    ),
    
    selectInput(
      "filter_depto",
      "Departamento:",
      choices = choice_depto,
      selected = "Todos"
    ),
    
    selectInput(
      "filter_nivel",
      "Nivel Académico:",
      choices = choice_nivel,
      selected = "Todos"
    ),
    
    # Filtro 1 (Cascade)
    selectInput(
      "filter_sector",
      "Sector IES:",
      choices = choice_sektor,
      selected = "Todos"
    ),
    
    # Filtro 2 (Cascade) - Sus opciones dependen del Filtro 1
    selectInput(
      "filter_caracter",
      "Caracter IES:",
      choices = c("Todos" = "Todos"), # Se llenará dinámicamente
      selected = "Todos"
    ),
    
    # Tooltip para UX
    bslib::tooltip(
      selectInput("filter_metodologia", "Metodología:", 
                  choices = c("Todos", as.character(unique(data_snies$Metodología))),
                  selected = "Todos"),
      "Filtrar por modalidad del programa"
    )
  ),
  
  # --- Cuerpo Principal (Pestañas) ---
  layout_columns(
    # Fila 1: KPIs Principales
    col_widths = c(4, 4, 4),
    
    # Usamos bslib::value_box, la versión moderna de valueBox
    value_box(
      title = "Total Matriculados",
      value = textOutput("kpi_matriculados"),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "primary"
    ),
    value_box(
      title = "Total Graduados",
      value = textOutput("kpi_graduados"),
      showcase = bsicons::bs_icon("mortarboard-fill"),
      theme = "secondary"
    ),
    value_box(
      title = "Instituciones (IES) Únicas",
      value = textOutput("kpi_ies"),
      showcase = bsicons::bs_icon("building"),
      theme = "info"
    )
  ),
  
  # Fila 2: Pestañas con Gráficos y Datos
  navset_tab(
    # --- Pestaña 1: Resumen y Tendencias ---
    nav_panel(
      title = "Resumen y Tendencias",
      layout_columns(
        col_widths = c(12, 12),
        row_heights = c(1, 2), # Asignar más altura a la segunda fila
        
        # Gráfico 1: Tendencia Temporal
        card(
          card_header("Evolución de Matriculados y Graduados (por Año)"),
          card_body(
            shinycssloaders::withSpinner(
              plotlyOutput("plot_tendencia"),
              type = 6, color = "#007bff"
            )
          )
        ),
        
        # Gráfico 2: Distribución
        card(
          card_header("Distribución por Nivel Académico y Sexo"),
          card_body(
            shinycssloaders::withSpinner(
              plotlyOutput("plot_distribucion_sexo"),
              type = 6, color = "#007bff"
            )
          )
        )
      )
    ),
    
    # --- Pestaña 2: Análisis Detallado ---
    nav_panel(
      title = "Análisis Detallado",
      layout_columns(
        col_widths = c(6, 6),
        
        # Gráfico 3: Comparación (Treemap)
        card(
          card_header("Matriculados por Sector y Caracter IES"),
          card_body(
            shinycssloaders::withSpinner(
              plotlyOutput("plot_treemap_sector"),
              type = 6, color = "#007bff"
            )
          )
        ),
        
        # Gráfico 4: Correlación
        card(
          card_header("Matriculados vs. Graduados (por IES)"),
          card_body(
            shinycssloaders::withSpinner(
              plotlyOutput("plot_scatter_ies"),
              type = 6, color = "#007bff"
            ),
            p(class = "text-muted", 
              "Cada punto representa una institución. El tamaño indica el total de programas.")
          )
        )
      )
    ),
    
    # --- Pestaña 3: Datos y Reportes ---
    nav_panel(
      title = "Datos y Reportes",
      card(
        card_header("Datos Filtrados"),
        card_body(
          p("A continuación, se muestra la tabla con los datos según los filtros aplicados."),
          downloadButton("download_report_csv", "Descargar CSV Filtrado"),
          downloadButton("download_report_html", "Descargar Reporte HTML"),
          hr(),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("tabla_datos"),
            type = 6, color = "#007bff"
          )
        )
      )
    )
  )
)

# --- 3. Lógica del Servidor ---
server <- function(input, output, session) {
  
  # --- A. Lógica de Filtros en Cascada ---
  
  # Usamos reactiveVal para almacenar las opciones del filtro dependiente
  caracter_options <- reactiveVal(c("Todos" = "Todos"))
  
  observeEvent(input$filter_sector, {
    if (input$filter_sector == "Todos") {
      # Si se seleccionan "Todos", mostrar todos los caracteres
      new_options <- c("Todos", as.character(unique(data_snies[[COL_CARACTER]])))
    } else {
      # Filtrar 'Caracter' basado en 'Sector'
      new_options <- data_snies %>%
        filter(.data[[COL_SECTOR]] == input$filter_sector) %>%
        pull(.data[[COL_CARACTER]]) %>%
        unique() %>%
        as.character()
      new_options <- c("Todos", new_options)
    }
    caracter_options(new_options)
    
    # Actualizar el selectInput en la UI
    updateSelectInput(session, "filter_caracter", 
                      choices = caracter_options(), 
                      selected = "Todos")
  })
  
  
  # --- B. Reactividad Principal (Datos Filtrados) ---
  
  # Este es el "cerebro" del dashboard.
  # Filtra los datos basado en *todos* los inputs.
  filtered_data <- reactive({
    
    df_filtered <- data_snies
    
    # 1. Filtro de Año
    df_filtered <- df_filtered %>%
      filter(.data[[COL_YEAR]] >= input$range_year[1] & 
               .data[[COL_YEAR]] <= input$range_year[2])
    
    # 2. Filtros de Selección (con condición "Todos")
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
  
  # Aplicamos debounce para evitar cálculos excesivos mientras el usuario
  # mueve el slider rápidamente. Espera 500ms de inactividad.
  data_reactive <- filtered_data %>% debounce(500)
  
  
  # --- C. Salidas de KPIs (Value Boxes) ---
  
  # Función auxiliar para formatear números
  format_kpi <- function(num) {
    if (num >= 1e6) {
      paste0(round(num / 1e6, 1), " M")
    } else if (num >= 1e3) {
      paste0(round(num / 1e3, 0), " K")
    } else {
      as.character(num)
    }
  }
  
  output$kpi_matriculados <- renderText({
    # Usamos data_reactive() que es la versión "debounded"
    total <- sum(data_reactive()[[COL_MATRIC]], na.rm = TRUE)
    format_kpi(total)
  })
  
  output$kpi_graduados <- renderText({
    total <- sum(data_reactive()[[COL_GRAD]], na.rm = TRUE)
    format_kpi(total)
  })
  
  output$kpi_ies <- renderText({
    n_distinct(data_reactive()[[COL_IES]])
  })
  
  # --- D. Salidas de Gráficos (Plotly) ---
  
  # Gráfico 1: Tendencia Temporal
  output$plot_tendencia <- renderPlotly({
    # Validar que hay datos
    validate(
      need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados.")
    )
    
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_YEAR]]) %>%
      summarise(
        Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE),
        Graduados = sum(.data[[COL_GRAD]], na.rm = TRUE)
      ) %>%
      tidyr::gather(key = "Metrica", value = "Total", -all_of(COL_YEAR))
    
    p <- ggplot(plot_data, aes(x = .data[[COL_YEAR]], y = Total, color = Metrica, 
                               text = paste("Año:", .data[[COL_YEAR]], "<br>Total:", Total))) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(title = NULL, x = "Año", y = "Total Personas", color = "Métrica") +
      theme_minimal() +
      theme(legend.position = "top")
    
    # Convertir a plotly y aplicar caching
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE)
    
  }) %>% bindCache(data_reactive()) # Caching basado en los datos reactivos
  
  # Gráfico 2: Distribución por Nivel y Sexo
  output$plot_distribucion_sexo <- renderPlotly({
    validate(
      need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados.")
    )
    
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_NIVEL]], .data[[COL_SEXO]]) %>%
      summarise(Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(plot_data, aes(x = .data[[COL_NIVEL]], y = Matriculados, fill = .data[[COL_SEXO]],
                               text = paste("Nivel:", .data[[COL_NIVEL]], 
                                            "<br>Sexo:", .data[[COL_SEXO]], 
                                            "<br>Matriculados:", Matriculados))) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = NULL, x = "Nivel Académico", y = "Total Matriculados", fill = "Sexo") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE)
    
  }) %>% bindCache(data_reactive())
  
  # Gráfico 3: Treemap por Sector
  output$plot_treemap_sector <- renderPlotly({
    validate(
      need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados.")
    )
    
    # 1. Resumir por "hijo" (Caracter IES)
    # Estos son los rectángulos más pequeños
    df_children <- data_reactive() %>%
      group_by(.data[[COL_SECTOR]], .data[[COL_CARACTER]]) %>%
      summarise(Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE), .groups = 'drop') %>%
      filter(Matriculados > 0) %>%
      rename(label = .data[[COL_CARACTER]], 
             parent = .data[[COL_SECTOR]], 
             value = Matriculados)
    
    # 2. Resumir por "padre" (Sector IES)
    # Estos son los rectángulos grandes que agrupan a los hijos
    df_parents <- df_children %>%
      group_by(parent) %>%
      summarise(value = sum(value), .groups = 'drop') %>%
      rename(label = parent) %>%
      mutate(parent = "Total") # El "padre" de los sectores es el total
    
    # 3. Crear el nodo raíz (El total de todo)
    # (Necesario para que la jerarquía esté completa)
    df_root <- data.frame(
      label = "Total",
      parent = "", # La raíz no tiene padre
      value = sum(df_parents$value)
    )
    
    # 4. Combinar todos los niveles en un solo dataframe
    plot_data <- bind_rows(df_root, df_parents, df_children)
    
    # 5. Graficar con la estructura jerárquica correcta
    fig <- plot_ly(
      data = plot_data,
      type = "treemap",
      labels = ~ label,
      parents = ~ parent,
      values = ~ value,
      branchvalues = "total", # Asegura que los padres sumen el total de sus hijos
      textinfo = "label+value+percent parent",
      hoverinfo = "text",
      text = ~ paste("Nivel:", label, 
                     "<br>Padre:", parent, 
                     "<br>Matriculados:", scales::comma(value, accuracy = 1))
    ) %>%
      layout(margin = list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = FALSE)
    
    fig
    
  }) %>% bindCache(data_reactive())
  
  # Gráfico 4: Scatter Plot IES
  # Gráfico 4: Scatter Plot IES (CORREGIDO)
  output$plot_scatter_ies <- renderPlotly({
    validate(
      need(nrow(data_reactive()) > 0, "No hay datos para los filtros seleccionados.")
    )
    
    plot_data <- data_reactive() %>%
      group_by(.data[[COL_IES]]) %>%
      summarise(
        Matriculados = sum(.data[[COL_MATRIC]], na.rm = TRUE),
        Graduados = sum(.data[[COL_GRAD]], na.rm = TRUE),
        # CORRECCIÓN: Usar 'Programa.Académico' en lugar de '`Programa Académico`'
        Programas = n_distinct(Programa.Académico), 
        .groups = 'drop'
      ) %>%
      filter(Matriculados > 0 | Graduados > 0)
    
    p <- ggplot(plot_data, aes(x = Matriculados, y = Graduados, size = Programas,
                               text = paste("IES:", .data[[COL_IES]], 
                                            "<br>Matriculados:", scales::comma(Matriculados),
                                            "<br>Graduados:", scales::comma(Graduados),
                                            "<br>Programas:", Programas))) +
      # CORRECCIÓN: Se eliminó 'color = .data[[COL_IES]]' para mejorar rendimiento
      geom_point(alpha = 0.6) +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = NULL, x = "Total Matriculados", y = "Total Graduados") +
      theme_minimal() +
      theme(legend.position = "none") # Ocultar leyenda es ahora más importante
    
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE)
    
  }) %>% bindCache(data_reactive())
  
  # --- E. Salida de Tabla (DT) ---
  
  output$tabla_datos <- DT::renderDataTable({
    
    # Seleccionamos solo columnas relevantes para mostrar
    data_display <- data_reactive() %>%
      select(all_of(COL_IES), `Programa Académico`, all_of(COL_NIVEL), Metodología, 
             all_of(COL_YEAR), Semestre, all_of(COL_MATRIC), all_of(COL_GRAD))
    
    DT::datatable(
      data_display,
      rownames = FALSE,
      filter = 'top',
      extensions = c('Buttons', 'Responsive'),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip', # Habilita botones
        buttons = list(
          list(extend = 'colvis', text = 'Seleccionar Columnas')
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json')
      )
    )
  })
  
  # --- F. Lógica de Descarga de Reportes ---
  
  # 1. Descarga de CSV
  output$download_report_csv <- downloadHandler(
    filename = function() {
      paste0("reporte_snies_filtrado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_reactive(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # 2. Descarga de Reporte HTML (R Markdown)
  # (Esto requiere un archivo .Rmd en el mismo directorio)
  
  # --- INICIO: Contenido de 'report.Rmd' (Deberías crear este archivo) ---
  #
  # ---
  # title: "Reporte SNIES - Análisis de Educación Superior"
  # date: "`r Sys.Date()`"
  # output: html_document
  # params:
  #   data_report: NA
  #   filters_summary: "Sin filtros"
  # ---
  # 
  # ## Reporte Generado
  # 
  # Este reporte fue generado desde el Dashboard de Análisis SNIES con los siguientes filtros aplicados:
  # 
  # ```{r, echo=FALSE}
  # print(params$filters_summary)
  # ```
  # 
  # ### Resumen de Datos
  # 
  # ```{r, echo=FALSE, message=FALSE, warning=FALSE}
  # library(dplyr)
  # 
  # # Calcular KPIs
  # kpi_mat <- sum(params$data_report$Matriculados, na.rm = TRUE)
  # kpi_grad <- sum(params$data_report$Graduados, na.rm = TRUE)
  # kpi_ies <- n_distinct(params$data_report$`Institución de Educación Superior (IES)`)
  # 
  # cat(paste0("- Total Matriculados: ", format(kpi_mat, big.mark = ","), "\n"))
  # cat(paste0("- Total Graduados: ", format(kpi_grad, big.mark = ","), "\n"))
  # cat(paste0("- Total IES: ", kpi_ies, "\n"))
  # ```
  # 
  # ### Tabla de Datos Completa
  # 
  # ```{r, echo=FALSE, message=FALSE, warning=FALSE}
  # DT::datatable(params$data_report, options = list(pageLength = 20))
  # ```
  #
  # --- FIN: Contenido de 'report.Rmd' ---
  
  output$download_report_html <- downloadHandler(
    filename = function() {
      paste0("reporte_snies_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Asegurarse de que 'rmarkdown' y 'knitr' estén instalados
      if (!require("rmarkdown")) {
        stop("El paquete 'rmarkdown' es necesario para generar reportes HTML.")
      }
      
      # Crear un resumen de los filtros para el reporte
      filter_summary_text <- paste(
        "Rango de Años:", paste(input$range_year, collapse = " - "),
        "\nDepartamento:", input$filter_depto,
        "\nNivel Académico:", input$filter_nivel,
        "\nSector IES:", input$filter_sector,
        "\nCaracter IES:", input$filter_caracter,
        "\nMetodología:", input$filter_metodologia,
        sep = "\n"
      )
      
      # Crear un archivo Rmd temporal (o usar uno existente)
      # Por simplicidad, aquí lo creamos al vuelo.
      # Es MEJOR PRÁCTICA tener 'report.Rmd' como un archivo separado.
      
      # --- Simulación de 'report.Rmd' (creado al vuelo) ---
      # (Si 'report.Rmd' existe, omitir este bloque y usar la ruta)
      temp_report_path <- file.path(tempdir(), "report.Rmd")
      writeLines(c(
        "---",
        "title: 'Reporte SNIES - Análisis de Educación Superior'",
        "output: html_document",
        "params:",
        "  data_report: NA",
        "  filters_summary: 'Sin filtros'",
        "---",
        "## Reporte Generado",
        "Filtros aplicados:",
        "```{r, echo=FALSE}",
        "cat(params$filters_summary)",
        "```",
        "## Resumen",
        "```{r, echo=FALSE}",
        "kpi_mat <- sum(params$data_report$Matriculados, na.rm = TRUE)",
        "kpi_grad <- sum(params$data_report$Graduados, na.rm = TRUE)",
        "cat(paste0('- Total Matriculados: ', kpi_mat, '\n'))",
        "cat(paste0('- Total Graduados: ', kpi_grad, '\n'))",
        "```",
        "## Datos",
        "```{r, echo=FALSE}",
        "DT::datatable(params$data_report)",
        "```"
      ), temp_report_path)
      # --- Fin simulación ---
      
      # Renderizar el R Markdown
      tryCatch({
        rmarkdown::render(
          input = temp_report_path, # Usar 'report.Rmd' si existe
          output_file = file,
          params = list(
            data_report = data_reactive(),
            filters_summary = filter_summary_text
          ),
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        showNotification(
          paste("Error al generar el reporte:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  )
}

# --- 4. Ejecutar la Aplicación ---
shinyApp(ui, server)