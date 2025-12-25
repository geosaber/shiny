# app.R - Aplicativo de Geoestatística Aplicada (VERSÃO COMPLETA)
# Com dataset meuse do pacote sp, meuse.grid, variogramas completos e krigagem indicadora

# Carregar pacotes necessários
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
library(waiter)
library(gstat)
library(sf)
library(tmap)
library(ggplot2)
library(dplyr)
library(DT)
library(RColorBrewer)
library(moments)
library(nortest)
library(sp)
library(spdep)

# Configurar ambiente
tmap_mode("plot")

# Carregar dados meuse do pacote sp
data("meuse", package = "sp")
data("meuse.grid", package = "sp")

# Converter para sf
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))
meuse_grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"))

# Definir CRS (RD New - Holanda)
st_crs(meuse_sf) <- 28992
st_crs(meuse_grid_sf) <- 28992

# UI do aplicativo
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", version = 5),
  useShinyjs(),
  useWaiter(),
  
  # Título
  titlePanel("Geoestatística Aplicada - Dataset Meuse (pacote sp)"),
  
  # CSS customizado
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 10px;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
      }
      .plot-container {
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
        background-color: white;
      }
      h3, h4, h5 {
        color: #2c3e50;
      }
      .sidebar-panel {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
      }
      .exercise {
        background-color: #e8f4fd;
        border-left: 4px solid #2196F3;
        padding: 10px;
        margin: 10px 0;
      }
      .nav-tabs > li > a {
        font-weight: bold;
      }
    "))
  ),
  
  # Layout principal
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-panel",
      
      # Navegação
      h4("Navegação", class = "text-primary"),
      selectInput(
        "nav_tab",
        "Selecione a análise:",
        choices = c(
          "1. Dados e Mapa" = "data",
          "2. Análise Exploratória" = "eda",
          "3. Interpolação IDW" = "idw",
          "4. Autocorrelação" = "autocorr",
          "5. Variografia" = "variography",
          "6. Krigagem" = "kriging"
        ),
        selected = "data"
      ),
      
      hr(),
      
      # Controles dinâmicos por aba
      uiOutput("dynamic_controls"),
      
      hr(),
      
      # Exercícios práticos
      h4("Exercícios Práticos", class = "text-primary"),
      div(class = "exercise",
          h5("Exercício 1: Normalidade e Transformação"),
          tags$ol(
            tags$li("Selecione a variável 'zinc'"),
            tags$li("Teste normalidade (Shapiro-Wilk)"),
            tags$li("Aplique transformação logarítmica"),
            tags$li("Compare histogramas")
          )
      ),
      div(class = "exercise",
          h5("Exercício 2: Variografia Completa"),
          tags$ol(
            tags$li("Calcule variograma omnidirecional"),
            tags$li("Ajuste modelo esférico"),
            tags$li("Analise variogramas direcionais"),
            tags$li("Verifique mapa de variograma")
          )
      ),
      div(class = "exercise",
          h5("Exercício 3: Comparação de Métodos"),
          tags$ol(
            tags$li("Compare IDW vs Krigagem Ordinária"),
            tags$li("Execute Krigagem Universal"),
            tags$li("Teste Krigagem Indicadora"),
            tags$li("Analise mapas de variância")
          )
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Outputs dinâmicos
      uiOutput("main_output")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Dados reativos
  data_reactive <- reactiveVal(meuse_sf)
  grid_reactive <- reactiveVal(meuse_grid_sf)
  selected_variable <- reactiveVal("zinc")
  
  # Observadores para variável selecionada
  observe({
    if (!is.null(input$variable_select)) {
      selected_variable(input$variable_select)
    }
  })
  
  # UI dinâmica para controles
  output$dynamic_controls <- renderUI({
    req(input$nav_tab)
    
    tagList(
      # Seletor de variável (comum a várias abas)
      if (input$nav_tab %in% c("eda", "idw", "autocorr", "variography", "kriging")) {
        selectInput(
          "variable_select",
          "Selecione a variável:",
          choices = c("Zinco" = "zinc", 
                      "Cobre" = "copper", 
                      "Chumbo" = "lead", 
                      "Cádmio" = "cadmium"),
          selected = selected_variable()
        )
      },
      
      # Controles específicos para cada aba
      if (input$nav_tab == "idw") {
        tagList(
          sliderInput("idw_power", "Potência (p):", 
                      min = 0.5, max = 10, value = 2, step = 0.5),
          numericInput("idw_nmax", "Nº máximo de vizinhos:", 
                       value = 10, min = 1, max = 50),
          checkboxInput("use_grid", "Usar meuse.grid como grade", value = TRUE)
        )
      },
      
      if (input$nav_tab == "autocorr") {
        tagList(
          selectInput("weight_type", "Tipo de matriz de pesos:",
                      choices = c("Vizinhos mais próximos" = "knn",
                                  "Distância limite" = "distance"),
                      selected = "knn"),
          conditionalPanel(
            condition = "input.weight_type == 'knn'",
            sliderInput("k_neighbors", "Nº de vizinhos:", 
                        min = 1, max = 20, value = 5)
          ),
          conditionalPanel(
            condition = "input.weight_type == 'distance'",
            sliderInput("dist_threshold", "Distância limite (m):",
                        min = 100, max = 2000, value = 1000)
          )
        )
      },
      
      if (input$nav_tab == "variography") {
        tagList(
          sliderInput("cutoff", "Cutoff (m):", 
                      min = 500, max = 3000, value = 1500),
          numericInput("width", "Largura da banda (m):", 
                       value = 100, min = 10, max = 500),
          selectInput("variogram_model", "Modelo de variograma:",
                      choices = c("Esférico" = "Sph",
                                  "Exponencial" = "Exp",
                                  "Gaussiano" = "Gau",
                                  "Matern" = "Mat"),
                      selected = "Sph"),
          checkboxInput("show_map", "Mostrar mapa de variograma", value = TRUE),
          checkboxInput("show_grouped", "Mostrar variogramas agrupados", value = TRUE)
        )
      },
      
      if (input$nav_tab == "kriging") {
        tagList(
          selectInput("kriging_type", "Tipo de krigagem:",
                      choices = c("Krigagem Ordinária" = "ordinary",
                                  "Krigagem Universal" = "universal",
                                  "Krigagem por Indicadora" = "indicator"),
                      selected = "ordinary"),
          conditionalPanel(
            condition = "input.kriging_type == 'universal'",
            selectInput("trend_model", "Modelo de tendência:",
                        choices = c("~ x + y (linear)" = "linear",
                                    "~ x + y + I(x^2) + I(y^2) (quadrático)" = "quadratic"),
                        selected = "linear")
          ),
          conditionalPanel(
            condition = "input.kriging_type == 'indicator'",
            sliderInput("threshold", "Limiar para krigagem indicadora:",
                        min = 0, max = 2000, value = 500, step = 50)
          ),
          checkboxInput("show_variance", "Mostrar mapa de variância", value = TRUE),
          checkboxInput("use_grid_krig", "Usar meuse.grid como grade", value = TRUE)
        )
      },
      
      # Botão de ação para cada aba
      if (input$nav_tab != "data") {
        actionButton("run_analysis", "Executar Análise", 
                     class = "btn-primary btn-block",
                     icon = icon("play"))
      }
    )
  })
  
  # Output principal dinâmico
  output$main_output <- renderUI({
    req(input$nav_tab)
    
    switch(input$nav_tab,
           "data" = tab_data(),
           "eda" = tab_eda(),
           "idw" = tab_idw(),
           "autocorr" = tab_autocorr(),
           "variography" = tab_variography(),
           "kriging" = tab_kriging()
    )
  })
  
  # 1. DADOS E MAPA
  tab_data <- function() {
    tagList(
      h3("1. Dados e Visualização Espacial - Dataset Meuse"),
      tabsetPanel(
        tabPanel("Pontos de Amostragem",
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              h4("Tabela de Dados (meuse)"),
                              DTOutput("data_table")
                          )
                   ),
                   column(6,
                          div(class = "plot-container",
                              h4("Mapa de Pontos - Zinco"),
                              tmapOutput("map_plot", height = "500px")
                          )
                   )
                 )
        ),
        tabPanel("Grade de Interpolação",
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              h4("Grade meuse.grid"),
                              plotOutput("grid_plot", height = "500px")
                          )
                   ),
                   column(6,
                          div(class = "plot-container",
                              h4("Informações da Grade"),
                              verbatimTextOutput("grid_info")
                          )
                   )
                 )
        ),
        tabPanel("Estatísticas",
                 fluidRow(
                   column(12,
                          div(class = "well",
                              h4("Informações do Dataset Meuse"),
                              verbatimTextOutput("dataset_info")
                          )
                   )
                 )
        )
      )
    )
  }
  
  output$data_table <- renderDT({
    datatable(
      st_drop_geometry(data_reactive()),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      class = 'display compact',
      caption = 'Dataset Meuse - 155 amostras de solo'
    )
  })
  
  output$map_plot <- renderTmap({
    tm_shape(data_reactive()) +
      tm_symbols(
        size = 0.15,
        col = "zinc",
        palette = "YlOrRd",
        title = "Concentração de Zinco (ppm)",
        border.col = "black",
        border.lwd = 0.3,
        alpha = 0.8
      ) +
      tm_layout(
        frame = FALSE,
        legend.outside = TRUE,
        legend.outside.position = "right"
      ) +
      tm_title("Pontos de Amostragem - Rio Meuse")
  })
  
  output$grid_plot <- renderPlot({
    ggplot() +
      geom_sf(data = grid_reactive(), color = "gray70", fill = NA, size = 0.1) +
      geom_sf(data = data_reactive(), aes(color = zinc), size = 2) +
      scale_color_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = "Zinco (ppm)"
      ) +
      labs(
        title = "Grade meuse.grid com Pontos de Amostragem",
        subtitle = paste("Grade:", nrow(grid_reactive()), "células")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  output$grid_info <- renderPrint({
    cat("=== GRADE MEUSE.GRID ===\n")
    cat("Número de células:", nrow(grid_reactive()), "\n")
    cat("Resolução aproximada: 40m x 40m\n")
    cat("Extensão (bbox):\n")
    bbox <- st_bbox(grid_reactive())
    cat("  X: ", round(bbox$xmin), "a", round(bbox$xmax), "m\n")
    cat("  Y: ", round(bbox$ymin), "a", round(bbox$ymax), "m\n")
    cat("\nVariáveis disponíveis:\n")
    cat("  part.a, part.b, dist, ffreq, soil, lime, landuse, dist.m\n")
    cat("\nUso: Grade de interpolação para métodos geoestatísticos\n")
  })
  
  output$dataset_info <- renderPrint({
    data_df <- st_drop_geometry(data_reactive())
    
    cat("=== DATASET MEUSE (pacote sp) ===\n")
    cat("Local: Rio Meuse, Holanda\n")
    cat("Amostras: ", nrow(data_reactive()), " pontos de solo\n")
    cat("\nVariáveis principais (concentrações em ppm):\n")
    
    # Estatísticas sumárias
    stats_summary <- data_df %>%
      summarise(
        zinc_mean = round(mean(zinc, na.rm = TRUE), 1),
        zinc_sd = round(sd(zinc, na.rm = TRUE), 1),
        copper_mean = round(mean(copper, na.rm = TRUE), 1),
        copper_sd = round(sd(copper, na.rm = TRUE), 1),
        lead_mean = round(mean(lead, na.rm = TRUE), 1),
        lead_sd = round(sd(lead, na.rm = TRUE), 1),
        cadmium_mean = round(mean(cadmium, na.rm = TRUE), 2),
        cadmium_sd = round(sd(cadmium, na.rm = TRUE), 2)
      )
    
    cat("Zinco:   ", stats_summary$zinc_mean, "±", stats_summary$zinc_sd, "ppm\n")
    cat("Cobre:   ", stats_summary$copper_mean, "±", stats_summary$copper_sd, "ppm\n")
    cat("Chumbo:  ", stats_summary$lead_mean, "±", stats_summary$lead_sd, "ppm\n")
    cat("Cádmio:  ", stats_summary$cadmium_mean, "±", stats_summary$cadmium_sd, "ppm\n")
    
    cat("\nVariáveis auxiliares:\n")
    cat("• elev: elevação (m)\n")
    cat("• dist: distância ao rio (m)\n")
    cat("• ffreq: frequência de inundação\n")
    cat("• soil: tipo de solo\n")
    cat("• lime: presença de calcário\n")
    cat("• landuse: uso do solo\n")
    
    cat("\nCRS: EPSG:28992 (RD New / Amersfoort)\n")
  })
  
  # 2. ANÁLISE EXPLORATÓRIA
  tab_eda <- function() {
    tagList(
      h3("2. Análise Exploratória dos Dados"),
      fluidRow(
        column(6,
               div(class = "plot-container",
                   plotOutput("hist_plot", height = "300px")
               )
        ),
        column(6,
               div(class = "plot-container",
                   plotOutput("qq_plot", height = "300px")
               )
        )
      ),
      fluidRow(
        column(6,
               div(class = "plot-container",
                   plotOutput("box_plot", height = "300px")
               )
        ),
        column(6,
               div(class = "well",
                   h4("Testes de Normalidade"),
                   tableOutput("normality_table")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "plot-container",
                   h4("Estatísticas Descritivas"),
                   tableOutput("descriptive_stats")
               )
        )
      )
    )
  }
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "eda") {
      showNotification("Análise exploratória atualizada!", 
                       type = "message", duration = 2)
    }
  })
  
  output$hist_plot <- renderPlot({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    var_name <- switch(selected_variable(),
                       "zinc" = "Zinco",
                       "copper" = "Cobre",
                       "lead" = "Chumbo",
                       "cadmium" = "Cádmio")
    
    # Dados transformados (log)
    var_data_log <- log(var_data + 1)
    
    # Plot comparativo
    p1 <- ggplot(data.frame(value = var_data), aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(title = paste("Original -", var_name),
           x = paste(var_name, "(ppm)"),
           y = "Densidade") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    p2 <- ggplot(data.frame(value = var_data_log), aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, fill = "darkgreen", alpha = 0.7) +
      geom_density(color = "orange", linewidth = 1) +
      labs(title = paste("Transformado (log+1) -", var_name),
           x = paste("log(", var_name, "+1)"),
           y = "Densidade") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Usar patchwork para combinar plots
    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p1, p2, ncol = 2)
    } else {
      p1  # Fallback para um único plot
    }
  })
  
  output$qq_plot <- renderPlot({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    var_name <- switch(selected_variable(),
                       "zinc" = "Zinco",
                       "copper" = "Cobre",
                       "lead" = "Chumbo",
                       "cadmium" = "Cádmio")
    
    # QQ plots comparativos
    p1 <- ggplot(data.frame(value = var_data), aes(sample = value)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste("QQ Plot Original -", var_name)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    p2 <- ggplot(data.frame(value = log(var_data + 1)), aes(sample = value)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = paste("QQ Plot Transformado -", var_name)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p1, p2, ncol = 2)
    } else {
      p1
    }
  })
  
  output$box_plot <- renderPlot({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    var_name <- switch(selected_variable(),
                       "zinc" = "Zinco",
                       "copper" = "Cobre",
                       "lead" = "Chumbo",
                       "cadmium" = "Cádmio")
    
    # Dados em formato longo para facet
    plot_data <- data.frame(
      value = c(var_data, log(var_data + 1)),
      transformation = rep(c("Original", "log(x+1)"), each = length(var_data))
    )
    
    ggplot(plot_data, aes(x = transformation, y = value, fill = transformation)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = paste("Boxplot Comparativo -", var_name),
        x = "Transformação",
        y = paste(var_name, ifelse(selected_variable() == "cadmium", "(ppm)", "(ppm)"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$normality_table <- renderTable({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    var_data_log <- log(var_data + 1)
    
    # Testes para dados originais
    shapiro_orig <- try(shapiro.test(var_data), silent = TRUE)
    ad_orig <- try(ad.test(var_data), silent = TRUE)
    
    # Testes para dados transformados
    shapiro_log <- try(shapiro.test(var_data_log), silent = TRUE)
    ad_log <- try(ad.test(var_data_log), silent = TRUE)
    
    results <- data.frame(
      Dados = character(),
      Teste = character(),
      Estatística = numeric(),
      p_valor = character(),
      Normalidade = character(),
      stringsAsFactors = FALSE
    )
    
    add_result <- function(dados, teste, stat, pval) {
      normal <- ifelse(pval > 0.05, "Normal ✓", "Não Normal ✗")
      rbind(results, data.frame(
        Dados = dados,
        Teste = teste,
        Estatística = round(stat, 4),
        p_valor = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 4))),
        Normalidade = normal,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!inherits(shapiro_orig, "try-error")) {
      results <- add_result("Original", "Shapiro-Wilk", 
                            shapiro_orig$statistic, shapiro_orig$p.value)
    }
    
    if (!inherits(ad_orig, "try-error")) {
      results <- add_result("Original", "Anderson-Darling", 
                            ad_orig$statistic, ad_orig$p.value)
    }
    
    if (!inherits(shapiro_log, "try-error")) {
      results <- add_result("log(x+1)", "Shapiro-Wilk", 
                            shapiro_log$statistic, shapiro_log$p.value)
    }
    
    if (!inherits(ad_log, "try-error")) {
      results <- add_result("log(x+1)", "Anderson-Darling", 
                            ad_log$statistic, ad_log$p.value)
    }
    
    results
  })
  
  output$descriptive_stats <- renderTable({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    var_data_log <- log(var_data + 1)
    
    stats_orig <- c(
      length(var_data),
      round(mean(var_data), 2),
      round(median(var_data), 2),
      round(sd(var_data), 2),
      round(var(var_data), 2),
      round(sd(var_data)/mean(var_data)*100, 1),
      round(skewness(var_data), 3),
      round(kurtosis(var_data), 3),
      round(min(var_data), 2),
      round(quantile(var_data, 0.25), 2),
      round(quantile(var_data, 0.75), 2),
      round(max(var_data), 2)
    )
    
    stats_log <- c(
      length(var_data_log),
      round(mean(var_data_log), 2),
      round(median(var_data_log), 2),
      round(sd(var_data_log), 2),
      round(var(var_data_log), 2),
      round(sd(var_data_log)/mean(var_data_log)*100, 1),
      round(skewness(var_data_log), 3),
      round(kurtosis(var_data_log), 3),
      round(min(var_data_log), 2),
      round(quantile(var_data_log, 0.25), 2),
      round(quantile(var_data_log, 0.75), 2),
      round(max(var_data_log), 2)
    )
    
    data.frame(
      Estatística = c("N", "Média", "Mediana", "Desvio Padrão", 
                      "Variância", "Coef. Variação (%)", "Assimetria", "Curtose",
                      "Mínimo", "1º Quartil", "3º Quartil", "Máximo"),
      Original = stats_orig,
      Transformado = stats_log,
      stringsAsFactors = FALSE
    )
  })
  
  # 3. INTERPOLAÇÃO IDW
  tab_idw <- function() {
    tagList(
      h3("3. Interpolação por Distância Inversa Ponderada (IDW)"),
      fluidRow(
        column(8,
               div(class = "plot-container",
                   plotOutput("idw_map", height = "500px")
               )
        ),
        column(4,
               div(class = "well",
                   h4("Parâmetros IDW"),
                   tableOutput("idw_params")
               ),
               div(class = "well",
                   h4("Validação Cruzada"),
                   tableOutput("idw_validation")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "plot-container",
                   plotOutput("idw_comparison", height = "300px")
               )
        )
      )
    )
  }
  
  idw_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "idw") {
      req(selected_variable(), input$idw_power, input$idw_nmax)
      
      waiter <- Waiter$new(
        id = "idw_map",
        html = spin_loader(),
        color = transparent(0.7)
      )
      waiter$show()
      
      on.exit(waiter$hide())
      
      tryCatch({
        # Preparar grade (usar meuse.grid ou criar grade regular)
        if (input$use_grid) {
          # Usar meuse.grid como grade
          grid_sp <- as(grid_reactive(), "Spatial")
        } else {
          # Criar grade regular
          bbox <- st_bbox(data_reactive())
          x_seq <- seq(bbox$xmin, bbox$xmax, length.out = 100)
          y_seq <- seq(bbox$ymin, bbox$ymax, length.out = 100)
          grid <- expand.grid(x = x_seq, y = y_seq)
          coordinates(grid) <- ~x+y
          proj4string(grid) <- CRS("+init=epsg:28992")
          grid_sp <- grid
        }
        
        # Converter dados para Spatial
        data_sp <- as(data_reactive(), "Spatial")
        
        # IDW
        idw_fit <- idw(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          newdata = grid_sp,
          idp = input$idw_power,
          nmax = input$idw_nmax
        )
        
        idw_results(idw_fit)
        
        showNotification("Interpolação IDW concluída!", 
                         type = "success", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro na IDW:", e$message), 
                         type = "error", duration = 5)
      })
    }
  })
  
  output$idw_map <- renderPlot({
    req(idw_results())
    
    # Converter para data.frame para plotagem
    idw_df <- as.data.frame(idw_results())
    data_df <- as.data.frame(data_reactive())
    
    ggplot() +
      geom_tile(data = idw_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      geom_point(data = data_df, aes(x = x, y = y), size = 1, color = "black") +
      scale_fill_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = paste(switch(selected_variable(),
                            "zinc" = "Zinco",
                            "copper" = "Cobre",
                            "lead" = "Chumbo",
                            "cadmium" = "Cádmio"), "(ppm)")
      ) +
      labs(
        title = paste("Interpolação IDW -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio"),
                      ifelse(input$use_grid, "(usando meuse.grid)", "")),
        subtitle = paste("p =", input$idw_power, "| nmax =", input$idw_nmax),
        x = "Coordenada X (m)",
        y = "Coordenada Y (m)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")
      ) +
      coord_fixed()
  })
  
  output$idw_params <- renderTable({
    req(input$idw_power, input$idw_nmax)
    
    data.frame(
      Parâmetro = c("Potência (p)", "Nº máximo de vizinhos", "Variável", "Grade usada"),
      Valor = c(
        input$idw_power, 
        input$idw_nmax,
        switch(selected_variable(),
               "zinc" = "Zinco",
               "copper" = "Cobre",
               "lead" = "Chumbo",
               "cadmium" = "Cádmio"),
        ifelse(input$use_grid, "meuse.grid", "grade regular")
      )
    )
  })
  
  output$idw_validation <- renderTable({
    req(selected_variable(), input$idw_power)
    
    tryCatch({
      data_sp <- as(data_reactive(), "Spatial")
      
      # Validação cruzada leave-one-out
      cv <- krige.cv(
        as.formula(paste(selected_variable(), "~ 1")),
        locations = data_sp,
        nfold = nrow(data_reactive()),
        set = list(idp = input$idw_power)
      )
      
      cv_df <- as.data.frame(cv)
      
      data.frame(
        Métrica = c("RMSE", "MAE", "ME (Bias)", "R²"),
        Valor = c(
          round(sqrt(mean(cv_df$residual^2, na.rm = TRUE)), 1),
          round(mean(abs(cv_df$residual), na.rm = TRUE), 1),
          round(mean(cv_df$residual, na.rm = TRUE), 2),
          round(cor(cv_df$observed, cv_df$observed - cv_df$residual, 
                    use = "complete.obs")^2, 3)
        ),
        Descrição = c("Raiz do Erro Quadrático Médio",
                      "Erro Absoluto Médio",
                      "Erro Médio (tendência)",
                      "Coeficiente de Determinação")
      )
    }, error = function(e) {
      data.frame(
        Métrica = c("RMSE", "MAE", "ME", "R²"),
        Valor = rep("N/A", 4),
        Descrição = rep("Erro no cálculo", 4)
      )
    })
  })
  
  output$idw_comparison <- renderPlot({
    req(selected_variable(), input$idw_power)
    
    tryCatch({
      data_sp <- as(data_reactive(), "Spatial")
      
      cv <- krige.cv(
        as.formula(paste(selected_variable(), "~ 1")),
        locations = data_sp,
        nfold = nrow(data_reactive()),
        set = list(idp = input$idw_power)
      )
      
      cv_df <- as.data.frame(cv)
      cv_df$predicted <- cv_df$observed - cv_df$residual
      
      ggplot(cv_df, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.6, color = "blue", size = 2) +
        geom_abline(slope = 1, intercept = 0, 
                    color = "red", linetype = "dashed", linewidth = 1) +
        geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linewidth = 0.8) +
        labs(
          title = paste("Validação Cruzada IDW -", 
                        switch(selected_variable(),
                               "zinc" = "Zinco",
                               "copper" = "Cobre",
                               "lead" = "Chumbo",
                               "cadmium" = "Cádmio")),
          subtitle = "Observado vs Predito (leave-one-out)",
          x = "Valores Observados (ppm)",
          y = "Valores Preditos (ppm)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold")
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Validação não disponível", size = 6) +
        theme_void()
    })
  })
  
  # 4. AUTOCORRELAÇÃO
  tab_autocorr <- function() {
    tagList(
      h3("4. Análise de Autocorrelação Espacial"),
      fluidRow(
        column(6,
               div(class = "plot-container",
                   plotOutput("moran_plot", height = "400px")
               )
        ),
        column(6,
               div(class = "well",
                   h4("Índice I de Moran"),
                   tableOutput("moran_table")
               ),
               div(class = "plot-container",
                   plotOutput("lisa_map", height = "300px")
               )
        )
      )
    )
  }
  
  moran_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "autocorr") {
      req(selected_variable())
      
      waiter <- Waiter$new(
        id = "moran_plot",
        html = spin_loader(),
        color = transparent(0.7)
      )
      waiter$show()
      
      on.exit(waiter$hide())
      
      tryCatch({
        # Extrair coordenadas
        coords <- st_coordinates(data_reactive())
        
        # Criar matriz de pesos
        if (input$weight_type == "knn") {
          knn <- knearneigh(coords, k = input$k_neighbors)
          nb <- knn2nb(knn)
        } else {
          nb <- dnearneigh(coords, d1 = 0, d2 = input$dist_threshold)
        }
        
        w <- nb2listw(nb, style = "W")
        
        # Teste de Moran global
        moran_global <- moran.test(
          data_reactive()[[selected_variable()]],
          listw = w,
          na.action = na.omit
        )
        
        # Moran local (LISA)
        moran_local <- localmoran(
          data_reactive()[[selected_variable()]],
          listw = w,
          na.action = na.omit
        )
        
        moran_results(list(
          global = moran_global,
          local = moran_local,
          weights = w
        ))
        
        showNotification("Análise de autocorrelação concluída!", 
                         type = "success", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro:", e$message), 
                         type = "error", duration = 5)
      })
    }
  })
  
  output$moran_plot <- renderPlot({
    req(moran_results(), selected_variable())
    
    variable <- data_reactive()[[selected_variable()]]
    lagged <- lag.listw(moran_results()$weights, variable)
    
    plot_data <- data.frame(
      x = variable,
      y = lagged
    )
    
    moran_i <- round(moran_results()$global$estimate[1], 3)
    p_value <- moran_results()$global$p.value
    
    ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(alpha = 0.7, color = "blue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
      geom_hline(yintercept = mean(lagged, na.rm = TRUE), 
                 linetype = "dashed", color = "gray", linewidth = 0.8) +
      geom_vline(xintercept = mean(variable, na.rm = TRUE), 
                 linetype = "dashed", color = "gray", linewidth = 0.8) +
      annotate("text", x = max(variable) * 0.8, y = max(lagged) * 0.9,
               label = paste("I =", moran_i, 
                             ifelse(p_value < 0.001, "\np < 0.001", 
                                    paste("\np =", round(p_value, 3)))),
               size = 4, color = "darkred", fontface = "bold") +
      labs(
        title = paste("Diagrama de Dispersão de Moran -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
        x = "Valor",
        y = "Valor Espacialmente Defasado"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  output$moran_table <- renderTable({
    req(moran_results())
    
    moran_test <- moran_results()$global
    
    significance <- ifelse(moran_test$p.value < 0.001, "***",
                           ifelse(moran_test$p.value < 0.01, "**",
                                  ifelse(moran_test$p.value < 0.05, "*", "ns")))
    
    data.frame(
      Estatística = c("Índice I de Moran", "Valor Esperado E[I]", 
                      "Variância", "Z-score", "Valor p", "Significância"),
      Valor = c(
        round(moran_test$estimate[1], 4),
        round(moran_test$estimate[2], 4),
        round(moran_test$estimate[3], 6),
        round(moran_test$statistic, 4),
        format.pval(moran_test$p.value, digits = 4, eps = 0.0001),
        significance
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$lisa_map <- renderPlot({
    req(moran_results(), selected_variable())
    
    # Calcular clusters LISA
    variable <- data_reactive()[[selected_variable()]]
    local_moran <- as.data.frame(moran_results()$local)
    lagged <- lag.listw(moran_results()$weights, variable)
    
    # Classificar clusters
    mean_var <- mean(variable, na.rm = TRUE)
    mean_lag <- mean(lagged, na.rm = TRUE)
    
    clusters <- rep("Não Significativo", length(variable))
    
    # Alto-Alto
    high_high <- variable > mean_var & lagged > mean_lag & local_moran$`Pr(z != E(Ii))` < 0.05
    clusters[high_high] <- "Alto-Alto"
    
    # Baixo-Baixo
    low_low <- variable < mean_var & lagged < mean_lag & local_moran$`Pr(z != E(Ii))` < 0.05
    clusters[low_low] <- "Baixo-Baixo"
    
    # Alto-Baixo
    high_low <- variable > mean_var & lagged < mean_lag & local_moran$`Pr(z != E(Ii))` < 0.05
    clusters[high_low] <- "Alto-Baixo"
    
    # Baixo-Alto
    low_high <- variable < mean_var & lagged > mean_lag & local_moran$`Pr(z != E(Ii))` < 0.05
    clusters[low_high] <- "Baixo-Alto"
    
    # Adicionar clusters aos dados
    data_with_clusters <- data_reactive()
    data_with_clusters$lisa_cluster <- factor(clusters, 
                                              levels = c("Alto-Alto", "Baixo-Baixo", 
                                                         "Alto-Baixo", "Baixo-Alto", 
                                                         "Não Significativo"))
    
    # Cores para clusters
    cluster_colors <- c("Alto-Alto" = "#d73027",
                        "Baixo-Baixo" = "#4575b4",
                        "Alto-Baixo" = "#fee090",
                        "Baixo-Alto" = "#91bfdb",
                        "Não Significativo" = "#cccccc")
    
    ggplot(data_with_clusters) +
      geom_sf(aes(color = lisa_cluster), size = 3) +
      scale_color_manual(values = cluster_colors, name = "Cluster LISA") +
      labs(
        title = paste("Clusters LISA -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
        subtitle = "Indicadores Locais de Associação Espacial"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
  })
  
  # 5. VARIOGRAFIA COMPLETA
  tab_variography <- function() {
    tagList(
      h3("5. Variografia Completa"),
      tabsetPanel(
        tabPanel("Variograma Omnidirecional",
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              plotOutput("variogram_plot", height = "400px")
                          )
                   ),
                   column(6,
                          div(class = "well",
                              h4("Parâmetros do Modelo"),
                              tableOutput("variogram_params")
                          ),
                          div(class = "plot-container",
                              h4("Resíduos do Ajuste"),
                              plotOutput("variogram_residuals", height = "200px")
                          )
                   )
                 )
        ),
        tabPanel("Variogramas Direcionais",
                 fluidRow(
                   column(12,
                          div(class = "plot-container",
                              plotOutput("directional_variogram", height = "500px")
                          )
                   )
                 )
        ),
        tabPanel("Variogramas Agrupados",
                 fluidRow(
                   column(12,
                          div(class = "plot-container",
                              plotOutput("grouped_variogram", height = "500px")
                          )
                   )
                 )
        ),
        tabPanel("Mapa de Variograma",
                 fluidRow(
                   column(12,
                          div(class = "plot-container",
                              plotOutput("variogram_map", height = "500px")
                          )
                   )
                 )
        )
      )
    )
  }
  
  variogram_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "variography") {
      req(selected_variable(), input$cutoff, input$width)
      
      waiter <- Waiter$new(
        id = "variogram_plot",
        html = spin_loader(),
        color = transparent(0.7)
      )
      waiter$show()
      
      on.exit(waiter$hide())
      
      tryCatch({
        data_sp <- as(data_reactive(), "Spatial")
        
        # 1. Variograma experimental omnidirecional
        v_exp <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          width = input$width
        )
        
        # 2. Ajustar modelo
        v_fit <- fit.variogram(
          v_exp,
          model = vgm(
            model = input$variogram_model,
            psill = var(data_sp[[selected_variable()]], na.rm = TRUE) * 0.8,
            range = input$cutoff / 3,
            nugget = var(data_sp[[selected_variable()]], na.rm = TRUE) * 0.2
          )
        )
        
        # 3. Variogramas direcionais
        v_dir <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          width = input$width,
          alpha = c(0, 45, 90, 135),
          tol.hor = 22.5
        )
        
        # 4. Variograma com agrupamento (cloud)
        v_cloud <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          cloud = TRUE
        )
        
        # 5. Mapa de variograma (map = TRUE)
        v_map <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          width = input$width,
          map = TRUE
        )
        
        variogram_results(list(
          experimental = v_exp,
          fitted = v_fit,
          directional = v_dir,
          cloud = v_cloud,
          map = v_map
        ))
        
        showNotification("Variografia completa calculada!", 
                         type = "success", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro na variografia:", e$message), 
                         type = "error", duration = 5)
      })
    }
  })
  
  output$variogram_plot <- renderPlot({
    req(variogram_results())
    
    plot(variogram_results()$experimental, 
         variogram_results()$fitted,
         main = paste("Variograma Omnidirecional -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
         xlab = "Distância (m)",
         ylab = "Semivariância",
         pch = 16,
         col = "darkblue",
         cex = 1.2)
    
    grid()
    legend("topleft", 
           legend = c("Experimental", "Modelo Ajustado"),
           col = c("darkblue", "red"),
           pch = c(16, NA),
           lty = c(NA, 1),
           lwd = 2,
           bty = "n")
  })
  
  output$variogram_params <- renderTable({
    req(variogram_results())
    
    v_fit <- variogram_results()$fitted
    
    data.frame(
      Parâmetro = c("Modelo", "Efeito Pepita (C₀)", "Patamar (C₀+C)", 
                    "Alcance (a)", "Razão C/(C₀+C)", "SSE"),
      Valor = c(
        switch(v_fit$model[2],
               "Sph" = "Esférico",
               "Exp" = "Exponencial",
               "Gau" = "Gaussiano",
               "Mat" = "Matern",
               v_fit$model[2]),
        round(v_fit$psill[1], 2),
        round(sum(v_fit$psill), 2),
        round(v_fit$range[2], 1),
        round(v_fit$psill[2]/sum(v_fit$psill), 3),
        round(attr(v_fit, "SSErr"), 4)
      ),
      Descrição = c(
        "Tipo de modelo",
        "Variância a distância zero",
        "Variância total",
        "Distância de dependência espacial",
        "Proporção de variância estruturada",
        "Soma dos quadrados dos resíduos"
      )
    )
  })
  
  output$variogram_residuals <- renderPlot({
    req(variogram_results())
    
    v_exp <- variogram_results()$experimental
    v_fit <- variogram_results()$fitted
    
    # Calcular valores preditos pelo modelo
    dist <- v_exp$dist
    pred <- variogramLine(v_fit, dist_vector = dist)$gamma
    
    # Resíduos
    residuals <- v_exp$gamma - pred
    
    plot_data <- data.frame(
      Distance = dist,
      Residual = residuals
    )
    
    ggplot(plot_data, aes(x = Distance, y = Residual)) +
      geom_point(color = "darkred", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = "Resíduos do Ajuste",
        x = "Distância (m)",
        y = "Resíduo (γ observado - γ predito)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$directional_variogram <- renderPlot({
    req(variogram_results())
    
    v_dir <- variogram_results()$directional
    
    ggplot(v_dir, aes(x = dist, y = gamma)) +
      geom_point(size = 2, color = "darkblue") +
      geom_smooth(method = "loess", se = FALSE, color = "red", span = 0.8) +
      facet_wrap(~ dir.hor, ncol = 2, 
                 labeller = labeller(dir.hor = function(x) paste(x, "°"))) +
      labs(
        title = paste("Variogramas Direcionais -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
        subtitle = paste("Cutoff =", input$cutoff, "m | Largura =", input$width, "m"),
        x = "Distância (m)",
        y = "Semivariância"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold")
      )
  })
  
  output$grouped_variogram <- renderPlot({
    req(variogram_results())
    
    v_cloud <- variogram_results()$cloud
    
    # Agrupar distâncias em bins para visualização
    v_cloud$bin <- cut(v_cloud$dist, breaks = 20)
    
    ggplot(v_cloud, aes(x = bin, y = gamma)) +
      geom_boxplot(fill = "lightblue", alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 18, 
                   size = 3, color = "red") +
      labs(
        title = paste("Variograma Agrupado (Cloud) -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
        subtitle = "Cada ponto representa um par de observações",
        x = "Distância (agrupada em bins)",
        y = "Semivariância"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$variogram_map <- renderPlot({
    req(variogram_results(), input$show_map)
    
    v_map <- variogram_results()$map
    
    # Converter para data.frame para plotagem com ggplot2
    v_map_df <- as.data.frame(v_map$map)
    
    ggplot(v_map_df, aes(x = dx, y = dy)) +
      geom_tile(aes(fill = gamma)) +
      scale_fill_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = "Semivariância"
      ) +
      labs(
        title = paste("Mapa de Variograma -", 
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio")),
        subtitle = "Semivariância em função do vetor de separação",
        x = "dx (m)",
        y = "dy (m)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")
      ) +
      coord_fixed()
  })
  
  # 6. KRIGAGEM COMPLETA
  tab_kriging <- function() {
    tagList(
      h3("6. Krigagem"),
      tabsetPanel(
        tabPanel("Mapa de Predição",
                 fluidRow(
                   column(8,
                          div(class = "plot-container",
                              plotOutput("kriging_map", height = "500px")
                          )
                   ),
                   column(4,
                          div(class = "well",
                              h4("Parâmetros da Krigagem"),
                              tableOutput("kriging_params")
                          ),
                          div(class = "well",
                              h4("Estatísticas do Modelo"),
                              tableOutput("kriging_stats")
                          )
                   )
                 )
        ),
        tabPanel("Mapa de Variância",
                 fluidRow(
                   column(12,
                          div(class = "plot-container",
                              plotOutput("kriging_variance", height = "500px")
                          )
                   )
                 )
        ),
        tabPanel("Validação Cruzada",
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              plotOutput("kriging_comparison", height = "400px")
                          )
                   ),
                   column(6,
                          div(class = "well",
                              h4("Estatísticas de Validação"),
                              tableOutput("kriging_validation")
                          ),
                          div(class = "plot-container",
                              plotOutput("kriging_residuals", height = "200px")
                          )
                   )
                 )
        )
      )
    )
  }
  
  kriging_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "kriging") {
      req(selected_variable())
      
      waiter <- Waiter$new(
        id = "kriging_map",
        html = spin_loader(),
        color = transparent(0.7)
      )
      waiter$show()
      
      on.exit(waiter$hide())
      
      tryCatch({
        # Preparar grade (usar meuse.grid ou criar grade regular)
        if (input$use_grid_krig) {
          grid_sp <- as(grid_reactive(), "Spatial")
        } else {
          bbox <- st_bbox(data_reactive())
          x_seq <- seq(bbox$xmin, bbox$xmax, length.out = 80)
          y_seq <- seq(bbox$ymin, bbox$ymax, length.out = 80)
          grid <- expand.grid(x = x_seq, y = y_seq)
          coordinates(grid) <- ~x+y
          proj4string(grid) <- CRS("+init=epsg:28992")
          grid_sp <- grid
        }
        
        # Converter dados para Spatial
        data_sp <- as(data_reactive(), "Spatial")
        
        # Calcular variograma
        v <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = 1500
        )
        
        # Ajustar modelo
        v_fit <- fit.variogram(v, vgm("Sph"))
        
        # Selecionar fórmula baseada no tipo de krigagem
        if (input$kriging_type == "ordinary") {
          formula_str <- paste(selected_variable(), "~ 1")
          krige_fit <- krige(
            as.formula(formula_str),
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit
          )
        } else if (input$kriging_type == "universal") {
          if (input$trend_model == "linear") {
            formula_str <- paste(selected_variable(), "~ x + y")
          } else {
            formula_str <- paste(selected_variable(), "~ x + y + I(x^2) + I(y^2)")
          }
          krige_fit <- krige(
            as.formula(formula_str),
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit
          )
        } else if (input$kriging_type == "indicator") {
          # Krigagem indicadora
          threshold <- input$threshold
          indicator <- ifelse(data_sp[[selected_variable()]] > threshold, 1, 0)
          data_sp$indicator <- indicator
          
          # Variograma para a variável indicadora
          v_ind <- variogram(
            indicator ~ 1,
            locations = data_sp
          )
          v_fit_ind <- fit.variogram(v_ind, vgm("Sph"))
          
          krige_fit <- krige(
            indicator ~ 1,
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit_ind
          )
        }
        
        # Validação cruzada
        if (input$kriging_type == "indicator") {
          cv <- krige.cv(
            indicator ~ 1,
            locations = data_sp,
            nfold = min(10, nrow(data_reactive()))
          )
        } else {
          cv <- krige.cv(
            as.formula(paste(selected_variable(), "~ 1")),
            locations = data_sp,
            nfold = min(10, nrow(data_reactive()))
          )
        }
        
        kriging_results(list(
          prediction = krige_fit,
          variogram = if(input$kriging_type == "indicator") v_fit_ind else v_fit,
          type = input$kriging_type,
          threshold = if(input$kriging_type == "indicator") input$threshold else NULL,
          cv = cv
        ))
        
        showNotification("Krigagem concluída!", 
                         type = "success", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro na krigagem:", e$message), 
                         type = "error", duration = 5)
      })
    }
  })
  
  output$kriging_map <- renderPlot({
    req(kriging_results())
    
    krige_df <- as.data.frame(kriging_results()$prediction)
    data_df <- as.data.frame(data_reactive())
    
    if (kriging_results()$type == "indicator") {
      # Para krigagem indicadora: probabilidade
      fill_label <- paste("Probabilidade >", kriging_results()$threshold, "ppm")
      title_suffix <- paste("(Limiar =", kriging_results()$threshold, "ppm)")
      palette <- "RdYlBu"
    } else {
      fill_label <- paste(switch(selected_variable(),
                                 "zinc" = "Zinco",
                                 "copper" = "Cobre",
                                 "lead" = "Chumbo",
                                 "cadmium" = "Cádmio"), "(ppm)")
      title_suffix <- ""
      palette <- "YlOrRd"
    }
    
    ggplot() +
      geom_tile(data = krige_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      geom_point(data = data_df, aes(x = x, y = y), size = 1, color = "black") +
      scale_fill_gradientn(
        colors = brewer.pal(9, palette),
        name = fill_label
      ) +
      labs(
        title = paste(switch(kriging_results()$type,
                             "ordinary" = "Krigagem Ordinária",
                             "universal" = "Krigagem Universal",
                             "indicator" = "Krigagem por Indicadora"),
                      "-",
                      switch(selected_variable(),
                             "zinc" = "Zinco",
                             "copper" = "Cobre",
                             "lead" = "Chumbo",
                             "cadmium" = "Cádmio"),
                      title_suffix),
        subtitle = ifelse(input$use_grid_krig, "(usando meuse.grid)", ""),
        x = "Coordenada X (m)",
        y = "Coordenada Y (m)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")
      ) +
      coord_fixed()
  })
  
  output$kriging_params <- renderTable({
    req(kriging_results())
    
    v_fit <- kriging_results()$variogram
    
    data.frame(
      Parâmetro = c("Tipo", "Modelo", "Efeito Pepita", "Patamar", "Alcance", "Grade"),
      Valor = c(
        switch(kriging_results()$type,
               "ordinary" = "Ordinária",
               "universal" = "Universal",
               "indicator" = paste("Indicadora (limiar =", kriging_results()$threshold, "ppm)")),
        switch(v_fit$model[2],
               "Sph" = "Esférico",
               v_fit$model[2]),
        round(v_fit$psill[1], 2),
        round(sum(v_fit$psill), 2),
        round(v_fit$range[2], 1),
        ifelse(input$use_grid_krig, "meuse.grid", "grade regular")
      )
    )
  })
  
  output$kriging_stats <- renderTable({
    req(kriging_results())
    
    krige_df <- as.data.frame(kriging_results()$prediction)
    
    data.frame(
      Estatística = c("Média Predita", "DP Predito", "Mínimo", "Máximo", 
                      "Pixels NA", "Resolução"),
      Valor = c(
        round(mean(krige_df$var1.pred, na.rm = TRUE), 1),
        round(sd(krige_df$var1.pred, na.rm = TRUE), 1),
        round(min(krige_df$var1.pred, na.rm = TRUE), 1),
        round(max(krige_df$var1.pred, na.rm = TRUE), 1),
        sum(is.na(krige_df$var1.pred)),
        ifelse(input$use_grid_krig, "40m", "variável")
      )
    )
  })
  
  output$kriging_variance <- renderPlot({
    req(kriging_results(), input$show_variance)
    
    krige_df <- as.data.frame(kriging_results()$prediction)
    
    ggplot() +
      geom_tile(data = krige_df, aes(x = coords.x1, y = coords.x2, fill = var1.var)) +
      scale_fill_gradientn(
        colors = brewer.pal(9, "Blues"),
        name = "Variância"
      ) +
      labs(
        title = paste("Variância da Krigagem -", 
                      switch(kriging_results()$type,
                             "ordinary" = "Ordinária",
                             "universal" = "Universal",
                             "indicator" = "Indicadora")),
        subtitle = "Medida de incerteza da predição",
        x = "Coordenada X (m)",
        y = "Coordenada Y (m)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")
      ) +
      coord_fixed()
  })
  
  output$kriging_validation <- renderTable({
    req(kriging_results())
    
    cv_df <- as.data.frame(kriging_results()$cv)
    
    data.frame(
      Métrica = c("RMSE", "MAE", "ME (Bias)", "R²", "Correlação"),
      Valor = c(
        round(sqrt(mean(cv_df$residual^2, na.rm = TRUE)), 2),
        round(mean(abs(cv_df$residual), na.rm = TRUE), 2),
        round(mean(cv_df$residual, na.rm = TRUE), 3),
        round(cor(cv_df$observed, cv_df$observed - cv_df$residual, 
                  use = "complete.obs")^2, 3),
        round(cor(cv_df$observed, cv_df$observed - cv_df$residual, 
                  use = "complete.obs"), 3)
      ),
      Descrição = c(
        "Raiz do Erro Quadrático Médio",
        "Erro Absoluto Médio",
        "Erro Médio (viés)",
        "Coeficiente de Determinação",
        "Correlação Observado-Predito"
      )
    )
  })
  
  output$kriging_comparison <- renderPlot({
    req(kriging_results())
    
    cv_df <- as.data.frame(kriging_results()$cv)
    cv_df$predicted <- cv_df$observed - cv_df$residual
    
    ggplot(cv_df, aes(x = observed, y = predicted)) +
      geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
      geom_abline(slope = 1, intercept = 0, 
                  color = "red", linetype = "dashed", linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
      labs(
        title = paste("Validação Cruzada -", 
                      switch(kriging_results()$type,
                             "ordinary" = "Krigagem Ordinária",
                             "universal" = "Krigagem Universal",
                             "indicator" = "Krigagem por Indicadora")),
        subtitle = "Observado vs Predito (cross-validation)",
        x = ifelse(kriging_results()$type == "indicator", 
                   "Indicadora Observada", "Valores Observados (ppm)"),
        y = ifelse(kriging_results()$type == "indicator", 
                   "Indicadora Predita", "Valores Preditos (ppm)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold")
      )
  })
  
  output$kriging_residuals <- renderPlot({
    req(kriging_results())
    
    cv_df <- as.data.frame(kriging_results()$cv)
    
    ggplot(cv_df, aes(x = residual)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = "Distribuição dos Resíduos",
        x = "Resíduo",
        y = "Densidade"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
}

# Executar aplicativo
shinyApp(ui, server)