# app.R - Geoestatística Aplicada (Posit Connect Compatível)
# Versão simplificada e otimizada

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

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))
meuse_grid_sf <- st_as_sf(meuse.grid, coords = c("x", "y"))
st_crs(meuse_sf) <- 28992
st_crs(meuse_grid_sf) <- 28992

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Geoestatística Aplicada - Meuse Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "nav_tab",
        "Análise:",
        choices = c(
          "Dados" = "data",
          "Análise Exploratória" = "eda",
          "IDW" = "idw",
          "Variografia" = "variography",
          "Krigagem" = "kriging"
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
        sliderInput("idw_power", "Potência (p):", 1, 10, 2, 0.5)
      ),
      
      conditionalPanel(
        condition = "input.nav_tab == 'variography'",
        sliderInput("cutoff", "Cutoff (m):", 500, 3000, 1500)
      ),
      
      conditionalPanel(
        condition = "input.nav_tab == 'kriging'",
        selectInput("krig_type", "Tipo:", 
                   c("Ordinária" = "ordinary", "Indicadora" = "indicator")),
        conditionalPanel(
          condition = "input.krig_type == 'indicator'",
          sliderInput("threshold", "Limiar:", 0, 2000, 500)
        )
      ),
      
      actionButton("run", "Executar", class = "btn-primary")
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
           "variography" = tab_variography(),
           "kriging" = tab_kriging()
    )
  })
  
  # 1. DADOS
  tab_data <- function() {
    tagList(
      h3("Dados Meuse"),
      fluidRow(
        column(6, DTOutput("table")),
        column(6, plotOutput("map"))
      )
    )
  }
  
  output$table <- renderDT({
    datatable(st_drop_geometry(meuse_sf), 
              options = list(pageLength = 5))
  })
  
  output$map <- renderPlot({
    ggplot(meuse_sf) +
      geom_sf(aes(color = zinc), size = 3) +
      scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"), 
                           name = "Zinco (ppm)") +
      labs(title = "Mapa de Pontos - Concentração de Zinco") +
      theme_minimal()
  })
  
  # 2. ANÁLISE EXPLORATÓRIA
  tab_eda <- function() {
    tagList(
      h3("Análise Exploratória"),
      fluidRow(
        column(6, plotOutput("hist")),
        column(6, plotOutput("qq"))
      ),
      tableOutput("stats")
    )
  }
  
  output$hist <- renderPlot({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, 
                     fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      labs(title = "Histograma", 
           x = paste(input$variable, "(ppm)"),
           y = "Densidade") +
      theme_minimal()
  })
  
  output$qq <- renderPlot({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    
    ggplot(data.frame(x = data), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = "QQ Plot") +
      theme_minimal()
  })
  
  output$stats <- renderTable({
    req(input$variable)
    data <- meuse_sf[[input$variable]]
    
    data.frame(
      Estatística = c("N", "Média", "Mediana", "DP", "Mín", "Máx"),
      Valor = c(
        length(data),
        round(mean(data), 2),
        round(median(data), 2),
        round(sd(data), 2),
        round(min(data), 2),
        round(max(data), 2)
      )
    )
  })
  
  # 3. IDW
  tab_idw <- function() {
    tagList(
      h3("Interpolação IDW"),
      plotOutput("idw_plot", height = "500px"),
      tableOutput("idw_stats")
    )
  }
  
  observeEvent(input$run, {
    if (input$nav_tab == "idw") {
      showNotification("Calculando IDW...", duration = 2)
    }
  })
  
  output$idw_plot <- renderPlot({
    req(input$variable, input$idw_power)
    
    # Converter para Spatial
    data_sp <- as(meuse_sf, "Spatial")
    grid_sp <- as(meuse_grid_sf, "Spatial")
    
    # IDW
    idw_fit <- gstat::idw(
      as.formula(paste(input$variable, "~ 1")),
      locations = data_sp,
      newdata = grid_sp,
      idp = input$idw_power,
      nmax = 10
    )
    
    # Converter para dataframe para plot
    idw_df <- as.data.frame(idw_fit)
    
    ggplot() +
      geom_tile(data = idw_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      geom_point(data = as.data.frame(meuse_sf), 
                 aes(x = x, y = y), size = 1, color = "black") +
      scale_fill_gradientn(
        colors = brewer.pal(9, "YlOrRd"),
        name = paste(input$variable, "(ppm)")
      ) +
      labs(title = paste("IDW - p =", input$idw_power),
           x = "X", y = "Y") +
      theme_minimal() +
      coord_fixed()
  })
  
  output$idw_stats <- renderTable({
    req(input$variable, input$idw_power)
    
    data_sp <- as(meuse_sf, "Spatial")
    
    cv <- gstat::krige.cv(
      as.formula(paste(input$variable, "~ 1")),
      locations = data_sp,
      nfold = 10,
      set = list(idp = input$idw_power)
    )
    
    cv_df <- as.data.frame(cv)
    
    data.frame(
      Métrica = c("RMSE", "MAE", "R²"),
      Valor = c(
        round(sqrt(mean(cv_df$residual^2)), 2),
        round(mean(abs(cv_df$residual)), 2),
        round(cor(cv_df$observed, cv_df$observed - cv_df$residual)^2, 3)
      )
    )
  })
  
  # 4. VARIOGRAFIA
  tab_variography <- function() {
    tagList(
      h3("Variografia"),
      fluidRow(
        column(6, plotOutput("variogram_plot")),
        column(6, 
               tableOutput("variogram_params"),
               plotOutput("directional_plot", height = "300px"))
      )
    )
  }
  
  output$variogram_plot <- renderPlot({
    req(input$variable, input$cutoff)
    
    data_sp <- as(meuse_sf, "Spatial")
    
    v <- gstat::variogram(
      as.formula(paste(input$variable, "~ 1")),
      locations = data_sp,
      cutoff = input$cutoff
    )
    
    v_fit <- gstat::fit.variogram(v, gstat::vgm("Sph"))
    
    plot(v, v_fit, 
         main = "Variograma Omnidirecional",
         xlab = "Distância (m)", 
         ylab = "Semivariância")
  })
  
  output$variogram_params <- renderTable({
    req(input$variable, input$cutoff)
    
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
  })
  
  output$directional_plot <- renderPlot({
    req(input$variable, input$cutoff)
    
    data_sp <- as(meuse_sf, "Spatial")
    
    v_dir <- gstat::variogram(
      as.formula(paste(input$variable, "~ 1")),
      locations = data_sp,
      cutoff = input$cutoff,
      alpha = c(0, 45, 90, 135),
      tol.hor = 22.5
    )
    
    # Variogramas agrupados (4 direções no mesmo plot)
    ggplot(v_dir, aes(x = dist, y = gamma, color = as.factor(dir.hor))) +
      geom_point(size = 2) +
      labs(title = "Variogramas Direcionais Agrupados",
           x = "Distância (m)",
           y = "Semivariância",
           color = "Direção (°)") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # 5. KRIGAGEM
  tab_kriging <- function() {
    tagList(
      h3("Krigagem"),
      plotOutput("kriging_plot", height = "500px"),
      tableOutput("kriging_stats")
    )
  }
  
  output$kriging_plot <- renderPlot({
    req(input$variable, input$krig_type)
    
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
      fill_label <- paste(input$variable, "(ppm)")
      
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
    
    ggplot() +
      geom_tile(data = krig_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
      scale_fill_gradientn(colors = brewer.pal(9, palette), name = fill_label) +
      labs(title = title, x = "X", y = "Y") +
      theme_minimal() +
      coord_fixed()
  })
  
  output$kriging_stats <- renderTable({
    req(input$variable)
    
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
        round(sqrt(mean(cv_df$residual^2)), 2),
        round(mean(abs(cv_df$residual)), 2),
        round(cor(cv_df$observed, cv_df$observed - cv_df$residual)^2, 3)
      )
    )
  })
}

# Executar app
shinyApp(ui, server)
