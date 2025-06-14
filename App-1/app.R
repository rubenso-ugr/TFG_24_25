# Carga de librerías necesarias ----------------------------------------------------------------

library(shiny)
library(bslib)

# Código de la UI y el server modularizados
source("ui.R")
source("server.R")

# Iniciar la app
shinyApp(ui = ui, server = server)