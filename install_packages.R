# install_packages_final.R
install.packages(c(
  "shiny",
  "shinyWidgets",
  "bslib",
  "shinyjs",
  "waiter",
  "gstat",
  "sf",
  "tmap",
  "ggplot2",
  "dplyr",
  "DT",
  "RColorBrewer",
  "moments",
  "nortest",
  "spdep",
  "sp"
))

# Instalar patchwork para gráficos combinados (opcional)
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

cat("=== INSTALAÇÃO CONCLUÍDA ===\n")
cat("Pacotes necessários instalados com sucesso!\n")
cat("\nPara executar o aplicativo:\n")
cat("1. Salve o código como 'app.R'\n")
cat("2. Execute: shiny::runApp('app.R')\n")
cat("3. Aguarde o carregamento do aplicativo\n")
