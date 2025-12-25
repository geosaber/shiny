# app.R - Aplicativo de Geoestatística Aplicada (Posit Connect Compatível)
# Dataset: meuse do pacote sp

# Carregar pacotes necessários
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
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
  
  # Título
  titlePanel("Geoestatística Aplicada - Dataset Meuse"),
  
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
      
      # Controles dinâmicos
      uiOutput("dynamic_controls")
    ),
    
    mainPanel(
      width = 9,
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
  
  # Observador para variável
  observe({
    if (!is.null(input$variable_select)) {
      selected_variable(input$variable_select)
    }
  })
  
  # UI dinâmica para controles
  output$dynamic_controls <- renderUI({
    req(input$nav_tab)
    
    tagList(
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
      
      if (input$nav_tab == "idw") {
        tagList(
          sliderInput("idw_power", "Potência (p):", 
                      min = 0.5, max = 10, value = 2, step = 0.5),
          numericInput("idw_nmax", "Nº máximo de vizinhos:", 
                       value = 10, min = 1, max = 50)
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
          )
        )
      },
      
      if (input$nav_tab == "variography") {
        tagList(
          sliderInput("cutoff", "Cutoff (m):", 
                      min = 500, max = 3000, value = 1500),
          selectInput("variogram_model", "Modelo:",
                      choices = c("Esférico" = "Sph",
                                  "Exponencial" = "Exp",
                                  "Gaussiano" = "Gau"),
                      selected = "Sph")
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
            condition = "input.kriging_type == 'indicator'",
            sliderInput("threshold", "Limiar para krigagem indicadora:",
                        min = 0, max = 2000, value = 500, step = 50)
          )
        )
      },
      
      if (input$nav_tab != "data") {
        actionButton("run_analysis", "Executar Análise", 
                     class = "btn-primary btn-block")
      }
    )
  })
  
  # Output principal
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
      h3("1. Dados e Visualização Espacial"),
      fluidRow(
        column(6, DTOutput("data_table")),
        column(6, tmapOutput("map_plot", height = "500px"))
      )
    )
  }
  
  output$data_table <- renderDT({
    datatable(
      st_drop_geometry(data_reactive()),
      options = list(pageLength = 5, scrollX = TRUE),
      caption = 'Dataset Meuse'
    )
  })
  
  output$map_plot <- renderTmap({
    tm_shape(data_reactive()) +
      tm_symbols(
        size = 0.15,
        col = "zinc",
        palette = "YlOrRd",
        title = "Zinco (ppm)",
        border.col = "black",
        border.lwd = 0.3
      ) +
      tm_layout(frame = FALSE, legend.outside = TRUE)
  })
  
  # 2. ANÁLISE EXPLORATÓRIA
  tab_eda <- function() {
    tagList(
      h3("2. Análise Exploratória dos Dados"),
      fluidRow(
        column(6, plotOutput("hist_plot", height = "300px")),
        column(6, plotOutput("qq_plot", height = "300px"))
      ),
      fluidRow(
        column(12, tableOutput("descriptive_stats"))
      )
    )
  }
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "eda") {
      showNotification("Análise atualizada!", duration = 2)
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
    
    ggplot(data.frame(value = var_data), aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(title = paste("Histograma -", var_name),
           x = paste(var_name, "(ppm)"),
           y = "Densidade") +
      theme_minimal()
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
    
    ggplot(data.frame(value = var_data), aes(sample = value)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste("QQ Plot -", var_name)) +
      theme_minimal()
  })
  
  output$descriptive_stats <- renderTable({
    req(selected_variable())
    
    data <- st_drop_geometry(data_reactive())
    var_data <- data[[selected_variable()]]
    
    data.frame(
      Estatística = c("N", "Média", "Mediana", "Desvio Padrão", 
                      "Variância", "Assimetria", "Curtose",
                      "Mínimo", "Máximo"),
      Valor = c(
        length(var_data),
        round(mean(var_data), 2),
        round(median(var_data), 2),
        round(sd(var_data), 2),
        round(var(var_data), 2),
        round(skewness(var_data), 3),
        round(kurtosis(var_data), 3),
        round(min(var_data), 2),
        round(max(var_data), 2)
      )
    )
  })
  
  # 3. INTERPOLAÇÃO IDW
  tab_idw <- function() {
    tagList(
      h3("3. Interpolação IDW"),
      fluidRow(
        column(8, plotOutput("idw_map", height = "500px")),
        column(4, tableOutput("idw_validation"))
      )
    )
  }
  
  idw_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "idw") {
      req(selected_variable(), input$idw_power, input$idw_nmax)
      
      showNotification("Calculando IDW...", duration = 2)
      
      tryCatch({
        # Usar meuse.grid como grade
        grid_sp <- as(grid_reactive(), "Spatial")
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
        
        showNotification("IDW concluída!", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro:", e$message), duration = 5)
      })
    }
  })
  
  output$idw_map <- renderPlot({
    req(idw_results())
    
    idw_df <- as.data.frame(idw_results())
    
    ggplot() +
      geom_tile(data = idw_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      scale_fill_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = paste(switch(selected_variable(),
                           "zinc" = "Zinco",
                           "copper" = "Cobre",
                           "lead" = "Chumbo",
                           "cadmium" = "Cádmio"), "(ppm)")
      ) +
      labs(title = paste("IDW -", switch(selected_variable(),
                                        "zinc" = "Zinco",
                                        "copper" = "Cobre",
                                        "lead" = "Chumbo",
                                        "cadmium" = "Cádmio")),
           x = "Coordenada X (m)",
           y = "Coordenada Y (m)") +
      theme_minimal() +
      coord_fixed()
  })
  
  output$idw_validation <- renderTable({
    req(selected_variable(), input$idw_power)
    
    tryCatch({
      data_sp <- as(data_reactive(), "Spatial")
      
      cv <- krige.cv(
        as.formula(paste(selected_variable(), "~ 1")),
        locations = data_sp,
        nfold = 10,
        set = list(idp = input$idw_power)
      )
      
      cv_df <- as.data.frame(cv)
      
      data.frame(
        Métrica = c("RMSE", "MAE", "R²"),
        Valor = c(
          round(sqrt(mean(cv_df$residual^2, na.rm = TRUE)), 2),
          round(mean(abs(cv_df$residual), na.rm = TRUE), 2),
          round(cor(cv_df$observed, cv_df$observed - cv_df$residual, 
                   use = "complete.obs")^2, 3)
        )
      )
    }, error = function(e) {
      data.frame(Métrica = "Erro", Valor = "N/A")
    })
  })
  
  # 4. AUTOCORRELAÇÃO
  tab_autocorr <- function() {
    tagList(
      h3("4. Análise de Autocorrelação"),
      fluidRow(
        column(6, plotOutput("moran_plot", height = "400px")),
        column(6, tableOutput("moran_table"))
      )
    )
  }
  
  moran_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "autocorr") {
      req(selected_variable())
      
      showNotification("Calculando autocorrelação...", duration = 2)
      
      tryCatch({
        coords <- st_coordinates(data_reactive())
        
        # Matriz de pesos
        if (input$weight_type == "knn") {
          knn <- knearneigh(coords, k = input$k_neighbors)
          nb <- knn2nb(knn)
        } else {
          nb <- dnearneigh(coords, d1 = 0, d2 = 1000)
        }
        
        w <- nb2listw(nb, style = "W")
        
        # Teste de Moran
        moran_test <- moran.test(
          data_reactive()[[selected_variable()]],
          listw = w,
          na.action = na.omit
        )
        
        moran_results(moran_test)
        
        showNotification("Análise concluída!", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro:", e$message), duration = 5)
      })
    }
  })
  
  output$moran_plot <- renderPlot({
    req(moran_results(), selected_variable())
    
    coords <- st_coordinates(data_reactive())
    nb <- knnearneigh(coords, k = 5)
    w <- nb2listw(knn2nb(nb), style = "W")
    
    variable <- data_reactive()[[selected_variable()]]
    lagged <- lag.listw(w, variable)
    
    plot_data <- data.frame(x = variable, y = lagged)
    
    ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(alpha = 0.7, color = "blue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Diagrama de Moran -", 
                        switch(selected_variable(),
                               "zinc" = "Zinco",
                               "copper" = "Cobre",
                               "lead" = "Chumbo",
                               "cadmium" = "Cádmio")),
           x = "Valor",
           y = "Valor Defasado") +
      theme_minimal()
  })
  
  output$moran_table <- renderTable({
    req(moran_results())
    
    moran_test <- moran_results()
    
    data.frame(
      Estatística = c("Índice I de Moran", "Valor Esperado", "Z-score", "Valor p"),
      Valor = c(
        round(moran_test$estimate[1], 4),
        round(moran_test$estimate[2], 4),
        round(moran_test$statistic, 4),
        format.pval(moran_test$p.value, digits = 4)
      )
    )
  })
  
  # 5. VARIOGRAFIA (SIMPLIFICADA)
  tab_variography <- function() {
    tagList(
      h3("5. Variografia"),
      fluidRow(
        column(6, plotOutput("variogram_plot", height = "400px")),
        column(6, 
               tableOutput("variogram_params"),
               plotOutput("directional_variogram", height = "300px"))
      )
    )
  }
  
  variogram_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "variography") {
      req(selected_variable(), input$cutoff)
      
      showNotification("Calculando variogramas...", duration = 2)
      
      tryCatch({
        data_sp <- as(data_reactive(), "Spatial")
        
        # Variograma omnidirecional
        v_exp <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff
        )
        
        # Ajustar modelo
        v_fit <- fit.variogram(
          v_exp,
          model = vgm(
            model = input$variogram_model,
            psill = var(data_sp[[selected_variable()]], na.rm = TRUE) * 0.8,
            range = input$cutoff / 3,
            nugget = var(data_sp[[selected_variable()]], na.rm = TRUE) * 0.2
          )
        )
        
        # Variogramas direcionais (4 direções no mesmo plot)
        v_dir <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          alpha = c(0, 45, 90, 135),
          tol.hor = 22.5
        )
        
        variogram_results(list(
          experimental = v_exp,
          fitted = v_fit,
          directional = v_dir
        ))
        
        showNotification("Variografia calculada!", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro:", e$message), duration = 5)
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
         ylab = "Semivariância")
  })
  
  output$variogram_params <- renderTable({
    req(variogram_results())
    
    v_fit <- variogram_results()$fitted
    
    data.frame(
      Parâmetro = c("Modelo", "Efeito Pepita", "Patamar", "Alcance"),
      Valor = c(
        switch(v_fit$model[2],
               "Sph" = "Esférico",
               "Exp" = "Exponencial",
               "Gau" = "Gaussiano",
               v_fit$model[2]),
        round(v_fit$psill[1], 2),
        round(sum(v_fit$psill), 2),
        round(v_fit$range[2], 1)
      )
    )
  })
  
  output$directional_variogram <- renderPlot({
    req(variogram_results())
    
    v_dir <- variogram_results()$directional
    
    # Variogramas direcionais agrupados
    ggplot(v_dir, aes(x = dist, y = gamma, color = as.factor(dir.hor))) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, span = 0.8) +
      scale_color_brewer(palette = "Set1", name = "Direção (°)") +
      labs(title = "Variogramas Direcionais Agrupados",
           subtitle = "4 direções (0°, 45°, 90°, 135°)",
           x = "Distância (m)",
           y = "Semivariância") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # 6. KRIGAGEM
  tab_kriging <- function() {
    tagList(
      h3("6. Krigagem"),
      fluidRow(
        column(6, plotOutput("kriging_map", height = "400px")),
        column(6, 
               tableOutput("kriging_validation"),
               plotOutput("kriging_comparison", height = "300px"))
      )
    )
  }
  
  kriging_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    if (input$nav_tab == "kriging") {
      req(selected_variable())
      
      showNotification("Executando krigagem...", duration = 2)
      
      tryCatch({
        # Usar meuse.grid como grade
        grid_sp <- as(grid_reactive(), "Spatial")
        data_sp <- as(data_reactive(), "Spatial")
        
        # Calcular variograma
        v <- variogram(
          as.formula(paste(selected_variable(), "~ 1")),
          locations = data_sp
        )
        
        v_fit <- fit.variogram(v, vgm("Sph"))
        
        # Krigagem baseada no tipo
        if (input$kriging_type == "ordinary") {
          krige_fit <- krige(
            as.formula(paste(selected_variable(), "~ 1")),
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit
          )
        } else if (input$kriging_type == "indicator") {
          threshold <- input$threshold
          indicator <- ifelse(data_sp[[selected_variable()]] > threshold, 1, 0)
          data_sp$indicator <- indicator
          
          v_ind <- variogram(indicator ~ 1, locations = data_sp)
          v_fit_ind <- fit.variogram(v_ind, vgm("Sph"))
          
          krige_fit <- krige(
            indicator ~ 1,
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit_ind
          )
        } else {
          # Universal com tendência linear
          krige_fit <- krige(
            as.formula(paste(selected_variable(), "~ x + y")),
            locations = data_sp,
            newdata = grid_sp,
            model = v_fit
          )
        }
        
        # Validação cruzada
        if (input$kriging_type == "indicator") {
          cv <- krige.cv(
            indicator ~ 1,
            locations = data_sp,
            nfold = 10
          )
        } else {
          cv <- krige.cv(
            as.formula(paste(selected_variable(), "~ 1")),
            locations = data_sp,
            nfold = 10
          )
        }
        
        kriging_results(list(
          prediction = krige_fit,
          cv = cv,
          type = input$kriging_type,
          threshold = if(input$kriging_type == "indicator") input$threshold else NULL
        ))
        
        showNotification("Krigagem concluída!", duration = 3)
      }, error = function(e) {
        showNotification(paste("Erro:", e$message), duration = 5)
      })
    }
  })
  
  output$kriging_map <- renderPlot({
    req(kriging_results())
    
    krige_df <- as.data.frame(kriging_results()$prediction)
    
    if (kriging_results()$type == "indicator") {
      fill_label <- paste("Probabilidade >", kriging_results()$threshold, "ppm")
      palette <- "RdYlBu"
    } else {
      fill_label <- paste(switch(selected_variable(),
                                "zinc" = "Zinco",
                                "copper" = "Cobre",
                                "lead" = "Chumbo",
                                "cadmium" = "Cádmio"), "(ppm)")
      palette <- "YlOrRd"
    }
    
    ggplot() +
      geom_tile(data = krige_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      scale_fill_gradientn(colors = brewer.pal(9, palette), name = fill_label) +
      labs(title = paste(switch(kriging_results()$type,
                               "ordinary" = "Krigagem Ordinária",
                               "universal" = "Krigagem Universal",
                               "indicator" = "Krigagem por Indicadora"),
                        "-",
                        switch(selected_variable(),
                               "zinc" = "Zinco",
                               "copper" = "Cobre",
                               "lead" = "Chumbo",
                               "cadmium" = "Cádmio")),
           x = "Coordenada X (m)",
           y = "Coordenada Y (m)") +
      theme_minimal() +
      coord_fixed()
  })
  
  output$kriging_validation <- renderTable({
    req(kriging_results())
    
    cv_df <- as.data.frame(kriging_results()$cv)
    
    data.frame(
      Métrica = c("RMSE", "MAE", "R²"),
      Valor = c(
        round(sqrt(mean(cv_df$residual^2, na.rm = TRUE)), 2),
        round(mean(abs(cv_df$residual), na.rm = TRUE), 2),
        round(cor(cv_df$observed, cv_df$observed - cv_df$residual, 
                 use = "complete.obs")^2, 3)
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
      labs(title = "Validação Cruzada",
           x = "Valores Observados",
           y = "Valores Preditos") +
      theme_minimal()
  })
}

# Executar aplicativo
shinyApp(ui, server)
