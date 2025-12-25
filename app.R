# app.R - Geoestatística Aplicada
# Versão corrigida para Posit Connect

# Carregar pacotes
library(shiny)
library(shinyWidgets)
library(bslib)
library(gstat)
library(sf)
library(ggplot2)
library(dplyr)
library(DT)
library(RColorBrewer)

# Configurar dados
data("meuse", package = "sp")
data("meuse.grid", package = "sp")

# Criar cópias dos dataframes antes de converter para sf
meuse_df <- meuse
meuse_grid_df <- meuse.grid

# Converter para sf mantendo x e y como colunas
meuse_sf <- st_as_sf(meuse_df, coords = c("x", "y"))
meuse_grid_sf <- st_as_sf(meuse_grid_df, coords = c("x", "y"))

# Adicionar colunas x e y explicitamente (importante para ggplot)
meuse_sf$x <- meuse_df$x
meuse_sf$y <- meuse_df$y
meuse_grid_sf$x <- meuse_grid_df$x
meuse_grid_sf$y <- meuse_grid_df$y

# Definir CRS
st_crs(meuse_sf) <- 28992
st_crs(meuse_grid_sf) <- 28992

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Geoestatística Aplicada - Dataset Meuse"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      
      conditionalPanel(
        condition = "input.nav_tab != 'data'",
        selectInput(
          "variable",
          "Variável:",
          choices = c("Zinco" = "zinc", "Cobre" = "copper", 
                     "Chumbo" = "lead", "Cádmio" = "cadmium"),
          selected = "zinc"
        )
      ),
      
      conditionalPanel(
        condition = "input.nav_tab == 'idw'",
        sliderInput("idw_power", "Potência (p):", 1, 10, 2, 0.5),
        numericInput("idw_nmax", "Nº máximo de vizinhos:", value = 10, min = 1, max = 50)
      ),
      
      conditionalPanel(
        condition = "input.nav_tab == 'variography'",
        sliderInput("cutoff", "Cutoff (m):", 500, 3000, 1500),
        selectInput("variogram_model", "Modelo:",
                   choices = c("Esférico" = "Sph", "Exponencial" = "Exp", "Gaussiano" = "Gau"))
      ),
      
      conditionalPanel(
        condition = "input.nav_tab == 'kriging'",
        selectInput("krig_type", "Tipo de Krigagem:", 
                   c("Ordinária" = "ordinary", "Indicadora" = "indicator")),
        conditionalPanel(
          condition = "input.krig_type == 'indicator'",
          sliderInput("threshold", "Limiar (ppm):", 0, 2000, 500)
        )
      ),
      
      actionButton("run", "Executar Análise", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      uiOutput("main_output")
    )
  )
)

# Server
server <- function(input, output, session) {
  
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
  
  # 1. DADOS
  tab_data <- function() {
    tagList(
      h3("1. Dados e Visualização Espacial"),
      fluidRow(
        column(6, 
               div(class = "well",
                   h4("Tabela de Dados"),
                   DTOutput("table")
               )
        ),
        column(6,
               div(class = "well",
                   h4("Mapa de Pontos"),
                   plotOutput("map", height = "500px")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Informações do Dataset"),
                   verbatimTextOutput("dataset_info")
               )
        )
      )
    )
  }
  
  output$table <- renderDT({
    datatable(
      st_drop_geometry(meuse_sf),
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      caption = "Dataset Meuse - 155 amostras de solo"
    )
  })
  
  output$map <- renderPlot({
    ggplot(meuse_sf) +
      geom_point(aes(x = x, y = y, color = zinc), size = 3) +
      scale_color_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = "Zinco (ppm)"
      ) +
      labs(
        title = "Mapa de Pontos - Concentração de Zinco",
        x = "Coordenada X (m)",
        y = "Coordenada Y (m)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      ) +
      coord_fixed()
  })
  
  output$dataset_info <- renderPrint({
    cat("=== DATASET MEUSE ===\n")
    cat("Local: Rio Meuse, Holanda\n")
    cat("Amostras: 155 pontos de solo\n")
    cat("\nVariáveis principais (ppm):\n")
    cat("• zinc: concentração de zinco\n")
    cat("• copper: concentração de cobre\n")
    cat("• lead: concentração de chumbo\n")
    cat("• cadmium: concentração de cádmio\n")
    cat("\nVariáveis auxiliares:\n")
    cat("• elev: elevação (m)\n")
    cat("• dist: distância ao rio (m)\n")
    cat("• soil: tipo de solo\n")
    cat("• lime: presença de calcário\n")
  })
  
  # 2. ANÁLISE EXPLORATÓRIA
  tab_eda <- function() {
    tagList(
      h3("2. Análise Exploratória dos Dados"),
      fluidRow(
        column(6,
               div(class = "well",
                   h4("Histograma"),
                   plotOutput("hist", height = "300px")
               )
        ),
        column(6,
               div(class = "well",
                   h4("QQ Plot"),
                   plotOutput("qq", height = "300px")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Estatísticas Descritivas"),
                   tableOutput("stats")
               )
        )
      )
    )
  }
  
  observeEvent(input$run, {
    if (input$nav_tab == "eda") {
      showNotification("Análise exploratória atualizada!", duration = 2)
    }
  })
  
  output$hist <- renderPlot({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    var_name <- switch(input$variable,
                      "zinc" = "Zinco",
                      "copper" = "Cobre",
                      "lead" = "Chumbo",
                      "cadmium" = "Cádmio")
    
    ggplot(data.frame(value = data), aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, 
                     fill = "steelblue", 
                     alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(
        title = paste("Histograma -", var_name),
        x = paste(var_name, "(ppm)"),
        y = "Densidade"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$qq <- renderPlot({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    var_name <- switch(input$variable,
                      "zinc" = "Zinco",
                      "copper" = "Cobre",
                      "lead" = "Chumbo",
                      "cadmium" = "Cádmio")
    
    ggplot(data.frame(value = data), aes(sample = value)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(
        title = paste("QQ Plot -", var_name)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  output$stats <- renderTable({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    
    data.frame(
      Estatística = c("N", "Média", "Mediana", "Desvio Padrão", 
                     "Variância", "Mínimo", "Máximo"),
      Valor = c(
        length(data),
        round(mean(data), 2),
        round(median(data), 2),
        round(sd(data), 2),
        round(var(data), 2),
        round(min(data), 2),
        round(max(data), 2)
      )
    )
  })
  
  # 3. IDW
  tab_idw <- function() {
    tagList(
      h3("3. Interpolação por Distância Inversa Ponderada (IDW)"),
      fluidRow(
        column(8,
               div(class = "well",
                   plotOutput("idw_plot", height = "500px")
               )
        ),
        column(4,
               div(class = "well",
                   h4("Parâmetros IDW"),
                   tableOutput("idw_params")
               ),
               div(class = "well",
                   h4("Validação Cruzada"),
                   tableOutput("idw_stats")
               )
        )
      )
    )
  }
  
  observeEvent(input$run, {
    if (input$nav_tab == "idw") {
      req(input$variable, input$idw_power, input$idw_nmax)
      
      showNotification("Calculando IDW...", duration = 2)
    }
  })
  
  output$idw_plot <- renderPlot({
    req(input$variable, input$idw_power, input$idw_nmax)
    
    tryCatch({
      # Converter para Spatial
      data_sp <- as(meuse_sf, "Spatial")
      grid_sp <- as(meuse_grid_sf, "Spatial")
      
      # IDW
      idw_fit <- gstat::idw(
        as.formula(paste(input$variable, "~ 1")),
        locations = data_sp,
        newdata = grid_sp,
        idp = input$idw_power,
        nmax = input$idw_nmax
      )
      
      # Converter para dataframe para plot
      idw_df <- as.data.frame(idw_fit)
      
      var_name <- switch(input$variable,
                        "zinc" = "Zinco",
                        "copper" = "Cobre",
                        "lead" = "Chumbo",
                        "cadmium" = "Cádmio")
      
      ggplot() +
        geom_tile(data = idw_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
        geom_point(data = meuse_sf, 
                   aes(x = x, y = y), size = 1, color = "black") +
        scale_fill_gradientn(
          colors = brewer.pal(9, "YlOrRd"),
          name = paste(var_name, "(ppm)")
        ) +
        labs(
          title = paste("Interpolação IDW -", var_name),
          subtitle = paste("p =", input$idw_power, "| nmax =", input$idw_nmax),
          x = "Coordenada X (m)",
          y = "Coordenada Y (m)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)
        ) +
        coord_fixed()
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Erro na interpolação IDW", size = 6, color = "red") +
        theme_void()
    })
  })
  
  output$idw_params <- renderTable({
    req(input$idw_power, input$idw_nmax)
    
    data.frame(
      Parâmetro = c("Potência (p)", "Nº máximo de vizinhos", "Variável"),
      Valor = c(
        input$idw_power,
        input$idw_nmax,
        switch(input$variable,
              "zinc" = "Zinco",
              "copper" = "Cobre",
              "lead" = "Chumbo",
              "cadmium" = "Cádmio")
      )
    )
  })
  
  output$idw_stats <- renderTable({
    req(input$variable, input$idw_power)
    
    tryCatch({
      data_sp <- as(meuse_sf, "Spatial")
      
      cv <- gstat::krige.cv(
        as.formula(paste(input$variable, "~ 1")),
        locations = data_sp,
        nfold = 10,
        set = list(idp = input$idw_power, nmax = input$idw_nmax)
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
      data.frame(
        Métrica = c("RMSE", "MAE", "R²"),
        Valor = rep("N/A", 3)
      )
    })
  })
  
  # 4. AUTOCORRELAÇÃO
  tab_autocorr <- function() {
    tagList(
      h3("4. Análise de Autocorrelação Espacial"),
      fluidRow(
        column(6,
               div(class = "well",
                   h4("Diagrama de Moran"),
                   plotOutput("moran_plot", height = "400px")
               )
        ),
        column(6,
               div(class = "well",
                   h4("Estatísticas de Moran"),
                   tableOutput("moran_table")
               )
        )
      )
    )
  }
  
  output$moran_plot <- renderPlot({
    req(input$variable)
    
    tryCatch({
      # Instalar spdep se necessário
      if (!requireNamespace("spdep", quietly = TRUE)) {
        stop("Pacote spdep não instalado")
      }
      
      library(spdep)
      
      # Criar matriz de pesos (knn com 5 vizinhos)
      coords <- cbind(meuse_sf$x, meuse_sf$y)
      knn <- knearneigh(coords, k = 5)
      nb <- knn2nb(knn)
      w <- nb2listw(nb, style = "W")
      
      # Calcular valores lagged
      variable <- meuse_sf[[input$variable]]
      lagged <- lag.listw(w, variable)
      
      plot_data <- data.frame(
        x = variable,
        y = lagged
      )
      
      var_name <- switch(input$variable,
                        "zinc" = "Zinco",
                        "copper" = "Cobre",
                        "lead" = "Chumbo",
                        "cadmium" = "Cádmio")
      
      ggplot(plot_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.7, color = "blue", size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
        labs(
          title = paste("Diagrama de Moran -", var_name),
          x = paste(var_name, "(ppm)"),
          y = paste(var_name, "Defasado")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(face = "bold")
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Análise de autocorrelação não disponível", 
                 size = 6) +
        theme_void()
    })
  })
  
  output$moran_table <- renderTable({
    req(input$variable)
    
    tryCatch({
      library(spdep)
      
      coords <- cbind(meuse_sf$x, meuse_sf$y)
      knn <- knearneigh(coords, k = 5)
      nb <- knn2nb(knn)
      w <- nb2listw(nb, style = "W")
      
      moran_test <- moran.test(meuse_sf[[input$variable]], w)
      
      data.frame(
        Estatística = c("Índice I de Moran", "Valor Esperado", "Z-score"),
        Valor = c(
          round(moran_test$estimate[1], 4),
          round(moran_test$estimate[2], 4),
          round(moran_test$statistic, 4)
        )
      )
    }, error = function(e) {
      data.frame(
        Estatística = c("Índice I de Moran", "Valor Esperado", "Z-score"),
        Valor = rep("N/A", 3)
      )
    })
  })
  
  # 5. VARIOGRAFIA
  tab_variography <- function() {
    tagList(
      h3("5. Variografia"),
      fluidRow(
        column(6,
               div(class = "well",
                   h4("Variograma Omnidirecional"),
                   plotOutput("variogram_plot", height = "400px")
               )
        ),
        column(6,
               div(class = "well",
                   h4("Parâmetros do Modelo"),
                   tableOutput("variogram_params")
               ),
               div(class = "well",
                   h4("Variogramas Direcionais Agrupados"),
                   plotOutput("directional_plot", height = "300px")
               )
        )
      )
    )
  }
  
  output$variogram_plot <- renderPlot({
    req(input$variable, input$cutoff)
    
    tryCatch({
      data_sp <- as(meuse_sf, "Spatial")
      
      v <- gstat::variogram(
        as.formula(paste(input$variable, "~ 1")),
        locations = data_sp,
        cutoff = input$cutoff
      )
      
      v_fit <- gstat::fit.variogram(v, gstat::vgm("Sph"))
      
      plot(v, v_fit, 
           main = paste("Variograma Omnidirecional -", 
                       switch(input$variable,
                              "zinc" = "Zinco",
                              "copper" = "Cobre",
                              "lead" = "Chumbo",
                              "cadmium" = "Cádmio")),
           xlab = "Distância (m)", 
           ylab = "Semivariância")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Erro no cálculo do variograma", cex = 1.5, col = "red")
    })
  })
  
  output$variogram_params <- renderTable({
    req(input$variable, input$cutoff)
    
    tryCatch({
      data_sp <- as(meuse_sf, "Spatial")
      
      v <- gstat::variogram(
        as.formula(paste(input$variable, "~ 1")),
        locations = data_sp,
        cutoff = input$cutoff
      )
      
      v_fit <- gstat::fit.variogram(v, gstat::vgm("Sph"))
      
      data.frame(
        Parâmetro = c("Modelo", "Efeito Pepita", "Patamar", "Alcance"),
        Valor = c(
          "Esférico",
          round(v_fit$psill[1], 2),
          round(sum(v_fit$psill), 2),
          round(v_fit$range[2], 1)
        )
      )
    }, error = function(e) {
      data.frame(
        Parâmetro = c("Modelo", "Efeito Pepita", "Patamar", "Alcance"),
        Valor = rep("N/A", 4)
      )
    })
  })
  
  output$directional_plot <- renderPlot({
    req(input$variable, input$cutoff)
    
    tryCatch({
      data_sp <- as(meuse_sf, "Spatial")
      
      # Calcular variogramas para 4 direções
      angles <- c(0, 45, 90, 135)
      v_dir_list <- list()
      
      for (angle in angles) {
        v_dir <- gstat::variogram(
          as.formula(paste(input$variable, "~ 1")),
          locations = data_sp,
          cutoff = input$cutoff,
          alpha = angle,
          tol.hor = 22.5
        )
        v_dir$direction <- paste(angle, "°")
        v_dir_list[[as.character(angle)]] <- v_dir
      }
      
      # Combinar todos em um dataframe
      plot_data <- do.call(rbind, v_dir_list)
      
      var_name <- switch(input$variable,
                        "zinc" = "Zinco",
                        "copper" = "Cobre",
                        "lead" = "Chumbo",
                        "cadmium" = "Cádmio")
      
      ggplot(plot_data, aes(x = dist, y = gamma, color = direction)) +
        geom_point(size = 2) +
        labs(
          title = paste("Variogramas Direcionais Agrupados -", var_name),
          subtitle = "4 direções (0°, 45°, 90°, 135°)",
          x = "Distância (m)",
          y = "Semivariância",
          color = "Direção"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom"
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Erro nos variogramas direcionais", 
                 size = 6, color = "red") +
        theme_void()
    })
  })
  
  # 6. KRIGAGEM
  tab_kriging <- function() {
    tagList(
      h3("6. Krigagem"),
      fluidRow(
        column(8,
               div(class = "well",
                   plotOutput("kriging_plot", height = "500px")
               )
        ),
        column(4,
               div(class = "well",
                   h4("Parâmetros da Krigagem"),
                   tableOutput("kriging_params")
               ),
               div(class = "well",
                   h4("Validação Cruzada"),
                   tableOutput("kriging_stats")
               )
        )
      )
    )
  }
  
  output$kriging_plot <- renderPlot({
    req(input$variable, input$krig_type)
    
    tryCatch({
      # Converter para Spatial
      data_sp <- as(meuse_sf, "Spatial")
      grid_sp <- as(meuse_grid_sf, "Spatial")
      
      if (input$krig_type == "ordinary") {
        # Krigagem ordinária
        v <- gstat::variogram(
          as.formula(paste(input$variable, "~ 1")),
          locations = data_sp
        )
        
        v_fit <- gstat::fit.variogram(v, gstat::vgm("Sph"))
        
        krig_fit <- gstat::krige(
          as.formula(paste(input$variable, "~ 1")),
          locations = data_sp,
          newdata = grid_sp,
          model = v_fit
        )
        
        title <- "Krigagem Ordinária"
        palette <- "YlOrRd"
        fill_label <- paste(
          switch(input$variable,
                "zinc" = "Zinco",
                "copper" = "Cobre",
                "lead" = "Chumbo",
                "cadmium" = "Cádmio"),
          "(ppm)"
        )
        
      } else {
        # Krigagem indicadora
        threshold <- input$threshold
        indicator <- ifelse(data_sp[[input$variable]] > threshold, 1, 0)
        data_sp$indicator <- indicator
        
        v_ind <- gstat::variogram(indicator ~ 1, locations = data_sp)
        v_fit_ind <- gstat::fit.variogram(v_ind, gstat::vgm("Sph"))
        
        krig_fit <- gstat::krige(
          indicator ~ 1,
          locations = data_sp,
          newdata = grid_sp,
          model = v_fit_ind
        )
        
        title <- paste("Krigagem Indicadora (limiar =", threshold, "ppm)")
        palette <- "RdYlBu"
        fill_label <- "Probabilidade"
      }
      
      # Converter para dataframe para plot
      krig_df <- as.data.frame(krig_fit)
      
      var_name <- switch(input$variable,
                        "zinc" = "Zinco",
                        "copper" = "Cobre",
                        "lead" = "Chumbo",
                        "cadmium" = "Cádmio")
      
      ggplot() +
        geom_tile(data = krig_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
        geom_point(data = meuse_sf, 
                   aes(x = x, y = y), size = 1, color = "black") +
        scale_fill_gradientn(
          colors = brewer.pal(9, palette),
          name = fill_label
        ) +
        labs(
          title = paste(title, "-", var_name),
          x = "Coordenada X (m)",
          y = "Coordenada Y (m)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        coord_fixed()
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Erro na krigagem", 
                 size = 6, color = "red") +
        theme_void()
    })
  })
  
  output$kriging_params <- renderTable({
    req(input$variable, input$krig_type)
    
    if (input$krig_type == "ordinary") {
      data.frame(
        Parâmetro = c("Tipo", "Variável", "Modelo"),
        Valor = c(
          "Ordinária",
          switch(input$variable,
                "zinc" = "Zinco",
                "copper" = "Cobre",
                "lead" = "Chumbo",
                "cadmium" = "Cádmio"),
          "Esférico"
        )
      )
    } else {
      data.frame(
        Parâmetro = c("Tipo", "Variável", "Limiar", "Modelo"),
        Valor = c(
          "Indicadora",
          switch(input$variable,
                "zinc" = "Zinco",
                "copper" = "Cobre",
                "lead" = "Chumbo",
                "cadmium" = "Cádmio"),
          paste(input$threshold, "ppm"),
          "Esférico"
        )
      )
    }
  })
  
  output$kriging_stats <- renderTable({
    req(input$variable)
    
    tryCatch({
      data_sp <- as(meuse_sf, "Spatial")
      
      cv <- gstat::krige.cv(
        as.formula(paste(input$variable, "~ 1")),
        locations = data_sp,
        nfold = 10
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
      data.frame(
        Métrica = c("RMSE", "MAE", "R²"),
        Valor = rep("N/A", 3)
      )
    })
  })
}

# Executar app
shinyApp(ui, server)
