# Carga de librerías necesarias ----------------------------------------------------------------

library(shiny)
library(bslib)

# Opciones globales
options(shiny.useragg = FALSE)
set.seed(123)

# Código de la UI y el server modularizados
source("ui.R")
source("server.R")

# Inicia la app
shinyApp(ui = ui, server = server)