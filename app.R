Sys.setlocale("LC_ALL", "en_US.UTF-8") # or your preferred UTF-8 locale
#Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(encoding = "UTF-8")
# ============================================================================
# Dashboard de Administración Pública de Haití
# Integración completa con diseño profesional ejecutivo
# ============================================================================
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(scales)
library(gridExtra)
library(plotly)
library(stringi)

# ============================================================================
# CONFIGURACIÓN DE DATOS
# ============================================================================

#EXCEL_PATH <- "C:/Users/Peni-/DataspellProjects/shiny/src/01_shiny_ui"
EXCEL_FILE <- "haiti.xlsx"
#EXCEL_FILE <- file.path(EXCEL_PATH, EXCEL_SHEET)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

get_variable_info <- function() {
  list(
    "Demografia" = list(
      poblacion = "Población total",
      matricula_primaria_pct = "Matrícula primaria (%)",
      tasa_alfabetizacion_pct = "Tasa de alfabetización (%)"
    ),
    "Economia" = list(
      pib_per_capita = "PIB per cápita",
      tasa_desempleo = "Tasa de desempleo (%)",
      tasa_inflacion = "Tasa de inflación (%)",
      ingresos_fiscales_pct_pib = "Ingresos fiscales (% PIB)",
      gasto_publico_pct_pib = "Gasto público (% PIB)",
      deficit_presupuestario_pct_pib = "Déficit presupuestario (% PIB)",
      ayuda_extranjera_pct_pib = "Ayuda extranjera (% PIB)",
      empleo_informal_pct = "Empleo informal (%)"
    ),
    "Infraestructura" = list(
      acceso_agua_pct = "Acceso a agua (%)",
      acceso_electricidad_pct = "Acceso a electricidad (%)",
      penetracion_internet_pct = "Penetración de internet (%)",
      indice_calidad_carreteras = "Índice calidad carreteras"
    ),
    "Indices sociales" = list(
      indice_corrupcion = "Índice de corrupción",
      indice_paridad_genero = "Índice paridad de género",
      satisfaccion_ciudadana = "Satisfacción ciudadana"
    )
  )
}

get_variable_category <- function(var_name) {
  vi <- get_variable_info()
  for (cat in names(vi)) {
    if (var_name %in% names(vi[[cat]])) return(cat)
  }
  "Otras variables"
}

get_variable_label <- function(var_name) {
  vi <- get_variable_info()
  for (cat in names(vi)) {
    if (var_name %in% names(vi[[cat]])) return(vi[[cat]][[var_name]])
  }
  var_name
}

calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  tb <- tabulate(match(x, ux))
  modes <- ux[tb == max(tb)]
  suppressWarnings(min(modes))
}

fmt_num <- function(x, dec = 2) {
  format(round(x, dec), big.mark = ",", nsmall = dec, scientific = FALSE)
}

fmt_int <- function(x) {
  format(round(x, 0), big.mark = ",", scientific = FALSE)
}

# ============================================================================
# INTERFAZ DE USUARIO
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(icon("chart-line"), "Dashboard Haití"),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Visión Nacional", tabName = "vision_nacional", icon = icon("flag")),
      menuItem("Matriz de Correlaciones", tabName = "correlaciones", icon = icon("chart-line")),
      menuItem("Impacto Hipotético PIB", tabName = "impacto_pib", icon = icon("dollar-sign")),
      menuItem("Detalles Técnicos", tabName = "detalles", icon = icon("table")),
      menuItem("Dispersión de Datos", tabName = "dispersion", icon = icon("bar-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Diseño ejecutivo profesional */
        .content-wrapper, .right-side {
          background-color: #ecf0f5;
        }
        
        /* Value Boxes mejorados */
        .small-box {
          min-height: 120px;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
          transition: all 0.3s ease;
          cursor: pointer;
        }
        
        .small-box:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 16px rgba(0,0,0,0.25);
        }
        
        .small-box .icon-large {
          display: none;
        }
        
        .small-box h3 {
          font-size: 28px !important;
          font-weight: bold;
          margin: 10px 0 5px 0 !important;
          color: #ffffff !important;
          text-shadow: 1px 1px 3px rgba(0,0,0,0.3);
        }
        
        .small-box p {
          font-size: 14px !important;
          margin: 0 !important;
          color: #ffffff !important;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.3);
          white-space: normal;
          font-weight: 500;
        }
        
        .small-box .inner {
          padding: 15px;
        }
        
        /* Títulos de categoría */
        .category-title {
          color: #3c8dbc;
          font-size: 22px;
          font-weight: 700;
          margin: 25px 0 15px 0;
          padding-bottom: 10px;
          border-bottom: 3px solid #3c8dbc;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        /* Gráfica regional con animación */
        .grafica-regional {
          margin-top: 20px;
          padding: 20px;
          background: white;
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          animation: fadeIn 0.5s ease-in;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(-20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        /* Box mejorados */
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        .box-solid > .box-header {
          border-radius: 10px 10px 0 0;
        }
        
        /* Select y sliders mejorados */
        .selectize-input {
          border-radius: 8px;
          border: 2px solid #3c8dbc;
          font-size: 15px;
        }
        
        .irs-bar {
          background: linear-gradient(to bottom, #667eea 0%, #764ba2 100%);
        }
        
        .irs-from, .irs-to, .irs-single {
          background: #667eea;
        }
        
        /* Botones mejorados */
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          border-radius: 8px;
          padding: 10px 25px;
          font-weight: 600;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          transform: scale(1.05);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.6);
        }
        
        /* DataTables */
        .dataTables_wrapper {
          padding: 15px;
        }
        
        table.dataTable thead th {
          background: linear-gradient(135deg, #3c8dbc 0%, #2c699a 100%);
          color: white;
          font-weight: 600;
        }
        
        /* Instrucciones */
        .info-box {
          background: #d9edf7;
          border-left: 4px solid #31b0d5;
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 8px;
        }
        
        .info-box strong {
          color: #31708f;
        }
      "))
    ),
    
    tabItems(
      # ========== VISIÓN NACIONAL ==========
      tabItem(
        tabName = "vision_nacional",
        
        h2("Visión Nacional - República de Haití", style = "color: #2c3e50; font-weight: 700;"),
        
        div(
          class = "info-box",
          p(
            tags$strong("📊 Instrucciones:"),
            " Haga clic en cualquier tarjeta para ver el desglose por región.",
            tags$br(),
            "La población se muestra como ", tags$strong("suma"),
            "; las demás variables como ", tags$strong("promedios"), "src/01_shiny_ui"
          )
        ),
        hr(),
        
        # KPIs por categoría
        uiOutput("kpi_boxes")
      ),
      
      # ========== MATRIZ DE CORRELACIONES ==========
      tabItem(
        tabName = "correlaciones",
        h2("Matriz de Correlaciones", style = "color: #2c3e50; font-weight: 700;"),
        p("Análisis de correlaciones entre todas las variables numéricas del dataset.", 
          style = "font-size: 16px; color: #555;"),
        
        fluidRow(
          box(
            width = 4,
            title = "Filtros de Correlación",
            status = "primary",
            solidHeader = TRUE,
            sliderInput("corr_threshold", 
                        "Umbral mínimo |correlación|:",
                        min = 0, max = 1, value = 0, step = 0.05),
            selectInput("corr_category", 
                        "Filtrar por categoría:",
                        choices = c("Todas" = "all"))
          ),
          box(
            width = 8,
            title = "Heatmap de Correlaciones",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("correlation_heatmap", height = "540px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Tabla de Pares de Correlaciones (únicos)",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("correlation_table")
          )
        )
      ),
      
      # ========== IMPACTO HIPOTÉTICO PIB ==========
      tabItem(
        tabName = "impacto_pib",
        h2("Análisis de Impacto Hipotético: PIB per cápita", 
           style = "color: #2c3e50; font-weight: 700;"),
        
        div(
          style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #f0ad4e;",
          p(
            style = "margin: 0; color: #856404;",
            tags$strong("⚠️ Nota importante:"), 
            " Este es un ejercicio exploratorio basado en correlaciones estadísticas. ",
            "Las estimaciones NO implican relaciones causales y deben interpretarse con precaución."
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Ajuste de Escenario",
            status = "primary",
            solidHeader = TRUE,
            sliderInput("pib_change", 
                        "Cambio porcentual en PIB per cápita:",
                        min = -50, max = 50, value = 0, step = 5,
                        post = "%",
                        width = "100%")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Tabla de Impacto Estimado",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("impact_table")
          ),
          box(
            width = 6,
            title = "Visualización de Cambios Estimados",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("impact_chart", height = "420px")
          )
        )
      ),
      
      # ========== DETALLES TÉCNICOS ==========
      tabItem(
        tabName = "detalles",
        h2("Detalles Técnicos de Variables", style = "color: #2c3e50; font-weight: 700;"),
        p("Estadísticas descriptivas completas para todas las variables numéricas.",
          style = "font-size: 16px; color: #555;"),
        
        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Filtros",
            selectInput("tech_category_filter", 
                        "Filtrar por categoría:",
                        choices = c("Todas" = "all"))
          ),
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Exportar",
            downloadButton("download_tech_details", 
                           "Descargar Tabla (CSV)",
                           class = "btn-primary btn-lg btn-block")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Tabla de Estadísticas Descriptivas",
            status = "primary",
            solidHeader = TRUE,
            p("Incluye: Media, Mediana, Moda, Desviación estándar, Mínimo, Máximo, N válidos, N NA"),
            DTOutput("technical_details_table")
          )
        )
      ),
      
      # ========== DISPERSIÓN DE DATOS ==========
      tabItem(
        tabName = "dispersion",
        h2("Análisis de Dispersión", style = "color: #2c3e50; font-weight: 700;"),
        
        tabsetPanel(
          type = "tabs",
          
          tabPanel(
            "Explorador Personalizado",
            br(),
            fluidRow(
              column(4, 
                     selectInput("scatter_x", "Variable Eje X:", choices = NULL)
              ),
              column(4, 
                     selectInput("scatter_y", "Variable Eje Y:", choices = NULL)
              ),
              column(4, 
                     checkboxInput("color_by_region", 
                                   "Diferenciar por región", 
                                   value = TRUE)
              )
            ),
            plotlyOutput("custom_scatter", height = "550px")
          ),
          
          tabPanel(
            "Galería de Pares Clave",
            br(),
            p("Visualización de relaciones importantes entre variables seleccionadas.",
              style = "font-size: 16px; color: #555;"),
            plotOutput("scatter_gallery", height = "900px")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVIDOR
# ============================================================================

server <- function(input, output, session) {
  
  # ========== CARGA DE DATOS ==========
  datos <- reactive({
    tryCatch({
      df <- read_excel(EXCEL_FILE, sheet = 1)
      # ========== Clean column names ==========
      names(df) <- tolower(names(df))  # lowercase
      names(df) <- stri_trans_general(names(df), "Latin-ASCII") # convert accents to ASCII
      names(df) <- gsub("[^a-z0-9_]", "_", names(df))           # replace non-alphanumeric chars with "_"

      # ========== Clean character columns ==========
      df <- df %>%
        mutate(across(where(is.character),
                      ~stri_trans_general(., "Latin-ASCII")))  # convert accents to ASCII
      df <- df %>%
      mutate(across(where(is.character),
                ~stri_enc_toutf8(.) %>%
                  stri_trans_general("Latin-ASCII") %>%
                  stri_replace_all_regex("[^\\x20-\\x7E]", " ")))
      # ========== Check required columns ==========
      if (!all(c("id", "region") %in% names(df))) {
        showNotification(
          "El archivo no contiene las columnas requeridas: 'id' y 'region'",
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Convertir a numérico
      numeric_cols <- setdiff(names(df), c("id", "region"))
      for (col in numeric_cols) {
        df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
      }
      
      showNotification(
        paste("Datos cargados correctamente:", nrow(df), "registros,", 
              ncol(df), "columnas"),
        type = "message",
        duration = 3
      )
      
      df
    }, error = function(e) {
      showNotification(
        paste("Error al cargar datos:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })
  
  numeric_vars <- reactive({
    req(datos())
    df <- datos()
    vars <- setdiff(names(df), c("id", "region"))
    vars[sapply(df[vars], is.numeric)]
  })
  
  national_summary <- reactive({
    req(datos(), numeric_vars())
    df <- datos()
    vars <- numeric_vars()
    
    result <- data.frame(
      variable = vars,
      valor = sapply(vars, function(v) {
        if (v == "poblacion") {
          sum(df[[v]], na.rm = TRUE)
        } else {
          mean(df[[v]], na.rm = TRUE)
        }
      }),
      stringsAsFactors = FALSE
    )
    result
  })

  regional_summary <- reactive({
    req(datos(), numeric_vars())
    df <- datos()
    vars <- numeric_vars()
    
    df %>%
      group_by(region) %>%
      summarise(
        across(
          all_of(vars),
          ~ if(cur_column() == "poblacion") sum(.x, na.rm = TRUE) else mean(.x, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      )
  })
  
  # ========== VISIÓN NACIONAL ==========
  
  selected_var <- reactiveVal(NULL)
  
  output$mostrar_grafica <- reactive({
    !is.null(selected_var())
  })
  outputOptions(output, "mostrar_grafica", suspendWhenHidden = FALSE)
  
  output$kpi_boxes <- renderUI({
    req(national_summary())

    ns <- national_summary()
    vi <- get_variable_info()

    all_elements <- list()

    for (cat_name in names(vi)) {
      # Título de categoría
      all_elements[[length(all_elements) + 1]] <- div(
        class = "category-title",
        cat_name
      )

      # Variables de esta categoría
      cat_vars <- names(vi[[cat_name]])

      # Crear valueBoxes (4 por fila)
      boxes <- lapply(cat_vars, function(var_name) {
        if (!var_name %in% ns$variable) {
          return(column(
            width = 4,
            valueBox(
              value = "N/D",
              subtitle = vi[[cat_name]][[var_name]],
              icon = icon("exclamation-triangle"),
              color = "red"
            )
          ))
        }

        val <- ns$valor[ns$variable == var_name]
        label <- vi[[cat_name]][[var_name]]

        display_val <- if (var_name == "poblacion") {
          fmt_int(val)
        } else {
          fmt_num(val, 2)
        }

        # Colores según categoría
        box_color <- if (grepl("Demografia", cat_name)) {
          "blue"
        } else if (grepl("Economia", cat_name)) {
          "green"
        } else if (grepl("Infraestructura", cat_name)) {
          "yellow"
        } else {
          "purple"
        }

        column(
          width = 3,
          div(
            style = "width: 100%;",
            onclick = sprintf(
        "Shiny.setInputValue('kpi_clicked', {var: '%s', nonce: Date.now()}, {priority: 'event'})", var_name),
            valueBox(
              value = display_val,
              subtitle = label,
              icon = icon("chart-bar"),
              color = box_color,
              width = NULL  # ensure no fixed width inside
            )
          )
        )
      })

      # Agregar fila de boxes
      all_elements[[length(all_elements) + 1]] <- fluidRow(boxes)
      all_elements[[length(all_elements) + 1]] <- br()
    }

    do.call(tagList, all_elements)
  })



  output$regional_chart_title <- renderText({
    req(selected_var())
    var <- selected_var()
    label <- get_variable_label(var)
    tipo <- if (var == "poblacion") "Suma" else "Promedio"
    paste(label, "—", tipo, "por Región")
  })
  
  output$regional_chart <- renderPlotly({
    req(selected_var(), datos())

    var <- selected_var()
    df <- datos()

    # Crear resumen temporal según variable
    if (var == "poblacion") {
      reg_data <- df %>%
        group_by(region) %>%
        summarise(value = sum(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      reg_data <- df %>%
        group_by(region) %>%
        summarise(value = round(mean(.data[[var]], na.rm = TRUE), 2), .groups = "drop")

    }

    reg_data <- reg_data %>% arrange(desc(value))

    p <- ggplot(reg_data, aes(x = reorder(region, value), y = value,
                              text = paste0(region, ": ", round(value, 2)))) +
      geom_col(fill = "#3c8dbc", alpha = 0.8) +
      coord_flip() +
      labs(
        x = "",
        y = get_variable_label(var)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE, hovermode = "closest")
  })

  # H_00 PATCH: Agregado un observador para aparecer despues de click.
  # Handle clicks reliably (kpi_clicked is an object: list(var = "...", nonce = <number>))
  observeEvent(input$kpi_clicked, {
    req(input$kpi_clicked)

    # If you sent an object from JS, extract the var name; otherwise fall back to raw value
    clicked_var <- NULL
    if (is.list(input$kpi_clicked) && !is.null(input$kpi_clicked$var)) {
      clicked_var <- input$kpi_clicked$var
    } else {
      clicked_var <- input$kpi_clicked
    }

    # Update selected_var for other logic if needed
    selected_var(clicked_var)

    # Show modal immediately on click — always runs because the JS includes a nonce
    showModal(
      modalDialog(
        title = paste0(get_variable_label(clicked_var),
                       " — ",
                       ifelse(clicked_var == "poblacion", "Suma", "Promedio"),
                       " por Región"),
        plotlyOutput("regional_chart", height = "450px"),
        easyClose = TRUE,
        size = "l"
      )
    )
  })

  # ========== MATRIZ DE CORRELACIONES ==========
  
  cor_matrix <- reactive({
    req(datos(), numeric_vars())
    df <- datos()
    vars <- numeric_vars()
    cor(df[vars], use = "pairwise.complete.obs")
  })
  
  observe({
    cats <- c("Todas" = "all", names(get_variable_info()))
    updateSelectInput(session, "corr_category", choices = cats)
    updateSelectInput(session, "tech_category_filter", choices = cats)
  })
  
  output$correlation_heatmap <- renderPlot({
    req(cor_matrix())
    
    cm <- cor_matrix()
    cor_df <- as.data.frame(as.table(cm))
    names(cor_df) <- c("Var1", "Var2", "Correlation")
    
    ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(
        low = "#2166ac", mid = "white", high = "#b2182b",
        midpoint = 0, limits = c(-1, 1),
        name = "Correlación"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_blank(),
        panel.grid = element_blank()
      ) +
      coord_fixed()
  })
  
  output$correlation_table <- renderDT({
    req(cor_matrix())
    
    cm <- cor_matrix()
    pairs <- expand.grid(
      variable_x = rownames(cm),
      variable_y = colnames(cm),
      stringsAsFactors = FALSE
    )
    pairs$correlacion <- mapply(function(x, y) cm[x, y], 
                                pairs$variable_x, pairs$variable_y)
    
    pairs <- pairs %>%
      filter(variable_x != variable_y) %>%
      mutate(
        pair_id = paste(pmin(variable_x, variable_y), 
                        pmax(variable_x, variable_y), sep = "_")
      ) %>%
      distinct(pair_id, .keep_all = TRUE) %>%
      select(-pair_id)
    
    if (input$corr_threshold > 0) {
      pairs <- pairs %>%
        filter(abs(correlacion) >= input$corr_threshold)
    }
    
    if (input$corr_category != "all") {
      vi <- get_variable_info()
      cat_vars <- names(vi[[input$corr_category]])
      pairs <- pairs %>%
        filter(variable_x %in% cat_vars | variable_y %in% cat_vars)
    }
    
    pairs <- pairs %>%
      arrange(desc(abs(correlacion))) %>%
      mutate(correlacion = round(correlacion, 3))
    
    datatable(
      pairs,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })
  
  # ========== IMPACTO HIPOTÉTICO PIB ==========
  
  output$impact_table <- renderDT({
    req(datos(), numeric_vars())
    
    df <- datos()
    vars <- setdiff(numeric_vars(), "poblacion")
    
    if (!"pib_per_capita" %in% vars) {
      return(datatable(
        data.frame(Mensaje = "Variable 'pib_per_capita' no encontrada en el dataset")
      ))
    }
    
    pib_avg <- mean(df$pib_per_capita, na.rm = TRUE)
    pib_sd <- sd(df$pib_per_capita, na.rm = TRUE)
    
    delta_pct <- input$pib_change / 100
    delta_z_pib <- ifelse(pib_sd == 0, 0, delta_pct * (pib_avg / pib_sd))
    
    results <- lapply(vars, function(v) {
      actual <- mean(df[[v]], na.rm = TRUE)
      v_sd <- sd(df[[v]], na.rm = TRUE)
      rho <- suppressWarnings(
        cor(df$pib_per_capita, df[[v]], use = "pairwise.complete.obs")
      )
      if (is.na(rho)) rho <- 0
      
      delta_v <- rho * delta_z_pib * v_sd
      nuevo <- actual + delta_v
      
      data.frame(
        Variable = get_variable_label(v),
        Valor_Actual = round(actual, 2),
        Valor_Estimado = round(nuevo, 2),
        Cambio_Absoluto = round(delta_v, 2),
        Cambio_Porcentual = round(ifelse(actual == 0, NA, 100 * delta_v / actual), 2),
        Correlacion_PIB = round(rho, 3)
      )
    })
    
    result_df <- bind_rows(results)
    
    datatable(
      result_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Cambio_Porcentual',
        backgroundColor = styleInterval(
          c(-5, 0, 5),
          c('#ffcccc', '#ffffcc', '#ccffcc', '#99ff99')
        )
      )
  })
  
  output$impact_chart <- renderPlotly({
    req(datos(), numeric_vars())
    
    df <- datos()
    vars <- setdiff(numeric_vars(), "poblacion")
    
    if (!"pib_per_capita" %in% vars) return(NULL)
    
    pib_avg <- mean(df$pib_per_capita, na.rm = TRUE)
    pib_sd <- sd(df$pib_per_capita, na.rm = TRUE)
    delta_pct <- input$pib_change / 100
    delta_z_pib <- ifelse(pib_sd == 0, 0, delta_pct * (pib_avg / pib_sd))
    
    results <- lapply(vars, function(v) {
      v_sd <- sd(df[[v]], na.rm = TRUE)
      rho <- suppressWarnings(
        cor(df$pib_per_capita, df[[v]], use = "pairwise.complete.obs")
      )
      if (is.na(rho)) rho <- 0
      delta_v <- rho * delta_z_pib * v_sd
      
      data.frame(
        Variable = get_variable_label(v),
        Variacion = delta_v,
        Positivo = delta_v >= 0
      )
    })
    
    plot_df <- bind_rows(results) %>%
      arrange(Variacion)
    
    p <- ggplot(plot_df, aes(x = reorder(Variable, Variacion), 
                             y = Variacion, fill = Positivo,
                             text = paste0(Variable, ": ", round(Variacion, 2)))) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                        guide = "none") +
      coord_flip() +
      labs(x = "", y = "Variación Estimada") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_text(size = 11, face = "bold"),
        panel.grid.major.y = element_blank()
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  # ========== DETALLES TÉCNICOS ==========
  
  output$technical_details_table <- renderDT({
    req(datos(), numeric_vars())
    
    df <- datos()
    vars <- numeric_vars()
    
    tech_data <- lapply(vars, function(v) {
      x <- df[[v]]
      data.frame(
        Variable = v,
        Etiqueta = get_variable_label(v),
        Categoria = get_variable_category(v),
        Media = mean(x, na.rm = TRUE),
        Mediana = median(x, na.rm = TRUE),
        Moda = calculate_mode(x),
        Desv_Est = sd(x, na.rm = TRUE),
        Minimo = suppressWarnings(min(x, na.rm = TRUE)),
        Maximo = suppressWarnings(max(x, na.rm = TRUE)),
        N_Validos = sum(!is.na(x)),
        N_NA = sum(is.na(x))
      )
    })
    
    result_df <- bind_rows(tech_data) %>%
      mutate(across(c(Media, Mediana, Moda, Desv_Est, Minimo, Maximo),
                    ~round(., 2)))
    
    if (!is.null(input$tech_category_filter) && 
        input$tech_category_filter != "all") {
      result_df <- result_df %>%
        filter(Categoria == input$tech_category_filter)
    }
    
    datatable(
      result_df,
      filter = "top",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })
  
  output$download_tech_details <- downloadHandler(
    filename = function() {
      paste("detalles_tecnicos_haiti_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- datos()
      vars <- numeric_vars()
      
      tech_data <- lapply(vars, function(v) {
        x <- df[[v]]
        data.frame(
          Variable = v,
          Etiqueta = get_variable_label(v),
          Categoria = get_variable_category(v),
          Media = mean(x, na.rm = TRUE),
          Mediana = median(x, na.rm = TRUE),
          Moda = calculate_mode(x),
          Desv_Est = sd(x, na.rm = TRUE),
          Minimo = suppressWarnings(min(x, na.rm = TRUE)),
          Maximo = suppressWarnings(max(x, na.rm = TRUE)),
          N_Validos = sum(!is.na(x)),
          N_NA = sum(is.na(x))
        )
      })
      
      result_df <- bind_rows(tech_data)
      write.csv(result_df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ========== DISPERSIÓN DE DATOS ==========
  
  observe({
    vars <- numeric_vars()
    if (length(vars) >= 2) {
      updateSelectInput(session, "scatter_x", 
                        choices = vars, selected = vars[1])
      updateSelectInput(session, "scatter_y", 
                        choices = vars, selected = vars[2])
    }
  })
  
  output$custom_scatter <- renderPlotly({
    req(datos(), input$scatter_x, input$scatter_y)

    df <- datos()

    if (isTRUE(input$color_by_region)) {
      p <- ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        geom_point(aes(color = region, text = paste0("Región: ", region,
                                                     "<br>", input$scatter_x, ": ", round(get(input$scatter_x), 2),
                                                     "<br>", input$scatter_y, ": ", round(get(input$scatter_y), 2))),
                   size = 3, alpha = 0.7)
    } else {
      p <- ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        geom_point(size = 3, alpha = 0.7, color = "#3c8dbc")
    }

    p <- p +
      geom_smooth(method = "lm", se = TRUE,
                  color = "#e74c3c", linetype = "dashed",
                  fill = "#e74c3c", alpha = 0.2) +
      labs(
        x = get_variable_label(input$scatter_x),
        y = get_variable_label(input$scatter_y)
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )

    ggplotly(p, tooltip = "text")
  })
  
  output$scatter_gallery <- renderPlot({
    req(datos())
    
    df <- datos()
    
    pairs_list <- list(
      c("pib_per_capita", "tasa_desempleo"),
      c("pib_per_capita", "tasa_inflacion"),
      c("acceso_electricidad_pct", "satisfaccion_ciudadana"),
      c("tasa_alfabetizacion_pct", "pib_per_capita"),
      c("indice_corrupcion", "satisfaccion_ciudadana"),
      c("acceso_agua_pct", "acceso_electricidad_pct")
    )
    
    pairs_list <- Filter(function(p) all(p %in% names(df)), pairs_list)
    
    if (length(pairs_list) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No hay pares de variables disponibles",
                        size = 6) +
               theme_void())
    }
    
    plots <- lapply(pairs_list, function(p) {
      ggplot(df, aes_string(x = p[1], y = p[2])) +
        geom_point(aes(color = region), size = 2.5, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, 
                    color = "#e74c3c", linetype = "dashed", size = 1) +
        labs(
          x = get_variable_label(p[1]),
          y = get_variable_label(p[2])
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none",
              plot.margin = margin(10, 10, 10, 10))
    })
    
    grid.arrange(grobs = plots, ncol = 2)
  })
}

# ============================================================================
# EJECUTAR APLICACIÓN
# ============================================================================

shinyApp(ui = ui, server = server)



# ============================================================================
# Script de Instalación de Paquetes para Dashboard Haití
# Ejecuta este script ANTES de correr la aplicación
# ============================================================================

cat("\n========================================\n")
cat("  INSTALACIÓN DE PAQUETES - DASHBOARD HAITÍ\n")
cat("========================================\n\n")

# Lista de paquetes necesarios
paquetes_necesarios <- c(
  "shiny",
  "shinydashboard",
  "readxl",
  "dplyr",
  "ggplot2",
  "DT",
  "tidyr",
  "stringr",
  "scales",
  "gridExtra",
  "plotly",
  "rsconnect"  # Para despliegue en shinyapps.io
)

# Función para instalar paquetes faltantes
instalar_si_falta <- function(paquete) {
  if (!require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Instalando:", paquete, "...\n"))
    install.packages(paquete, dependencies = TRUE)
    library(paquete, character.only = TRUE)
    cat(paste("✓", paquete, "instalado correctamente\n\n"))
  } else {
    cat(paste("✓", paquete, "ya está instalado\n"))
  }
}

# Instalar todos los paquetes necesarios
cat("\nVerificando e instalando paquetes...\n\n")
for (paquete in paquetes_necesarios) {
  instalar_si_falta(paquete)
}

cat("\n========================================\n")
cat("  ✓ INSTALACIÓN COMPLETADA\n")
cat("========================================\n\n")

cat("Paquetes instalados:\n")
for (paquete in paquetes_necesarios) {
  version <- packageVersion(paquete)
  cat(sprintf("  - %s (versión %s)\n", paquete, version))
}

cat("\n========================================\n")
cat("  SIGUIENTE PASO\n")
cat("========================================\n")
cat("\nPuedes ejecutar tu aplicación de las siguientes formas:\n\n")
cat("1. En RStudio:\n")
cat("   - Abre el archivo 'app.R'\n")
cat("   - Presiona el botón 'Run App'\n\n")
cat("2. Desde la consola:\n")
cat("   shiny::runApp('app.R')\n\n")
cat("3. Para desplegar en shinyapps.io:\n")
cat("   - Configura tus credenciales primero (ver deploy.R)\n")
cat("   - Luego ejecuta: rsconnect::deployApp()\n\n")

cat("========================================\n\n")
