# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)


library(RandomFieldsUtils)
library(RandomFields)

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

T<- c(1,1,1)

rea_to_show <- 1 #Realización a mostrar


# Define UI --------------------------------------------------------------------

ui <- page_fluid(
  layout_sidebar(
    sidebar = sidebar(
      numericInput("ancho", "Ancho (pixeles)", value = 100, min = 1, max = 5000),
      numericInput("alto", "Alto (pixeles)", value = 100, min = 1, max = 5000),
      numericInput("alpha", "Alpha", value = 2, min = 1e-20, max = 2),
      numericInput("beta", "Beta", value = 0.1, min = 1e-20),
      numericInput("percentil", "Percentil", value = 0.15, min = 1e-20, max = 1),
      numericInput("realizaciones", "Realizaciones", value = 50, min = 1, max = 1000),
      selectInput("modelo", "Elige modelo", choices = c("Cauchy", "Gneiting")),
      actionButton("button", "Calcular")
    ),
    
    # Panel derecho con pestañas
    navset_card_underline(
      nav_panel("Primera simulación", plotOutput("primera_simulacion")),
      nav_panel("Conjunto Excursión", plotOutput("conjunto_excursion")),
      nav_panel("Metodología aplicada", plotOutput("metodología")),
      nav_panel("Resumen", verbatimTextOutput("mensaje_resumen"))
      
    )
  )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$button, {
    modelo <- switch(input$modelo,
                     "Cauchy" = RMgencauchy(alpha = input$alpha, beta = input$beta, var = 0.1, scale = 1),
                     "Gneiting" = RMnsst(
                       phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = 0.1, scale = 1),
                       psi = RMstable(alpha = input$alpha, var = 0.1, scale = 1),
                       delta = 2
                     ))
    
    x <- 0:(input$ancho - 1)
    y <- 0:(input$alto - 1)
    sim <- RFsimulate(model = modelo, x = x, y = y, T = c(1, 1, 1), n = input$realizaciones)
    
    sim_values <- matrix(unlist(sim@data), nrow = input$ancho * input$alto, ncol = input$realizaciones)
    
    min_val_rea1 <- min(sim_values[, rea_to_show])
    min_val <- min(sim_values)
    diferencia <- min_val - min_val_rea1
    max_val_rea1 <- max(sim_values[, rea_to_show])
    
    
    sim_values_pos_rea1 <- sim_values[, rea_to_show] - min_val_rea1
    
    sim_values_pos <- sim_values - min_val  # ahora el mínimo de la primera realización es 0
    
    realization_matrix <- matrix(sim_values_pos[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    
    
    output$primera_simulacion <- renderPlot({
      # Dibujamos la realización escogida
      filled.contour(x, y, realization_matrix,
                     color.palette = mis.colores,
                     asp = 1,
                     axes = TRUE,
                     frame.plot = 0,
                     main = paste("Realización", rea_to_show),
                     xlim =  c(0, input$ancho),
                     ylim =  c(0, input$alto))
    })
    
    
    
    #Guardamos el valor máximo global y de la primera realización.
    #max_val <- max(sim_values_pos)
    max_val_rea1 <- max(sim_values_pos_rea1)
    
    
    #Estimamos el umbral de riesgo (percentil 1 - alpha)
    all_vals <- as.vector(sim_values_pos)
    threshold <- quantile(all_vals, probs = 1 - input$percentil)
    
    #Obtenemos el conjunto de excursión (asignamos 0 a valores por debajo del umbral en TODAS las realizaciones)
    excursion_values <- sim_values_pos
    excursion_values[sim_values_pos < threshold] <- 0
    
    #Tomamos el conjunto de excursión de la realización escogida
    excursion_matrix <- matrix(excursion_values[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    
    output$conjunto_excursion <- renderPlot({
      filled.contour(x, y, excursion_matrix,
                     color.palette = mis.colores,
                     asp = 1,
                     axes = TRUE,
                     frame.plot = 0,
                     main = paste("Conjunto de Excursión (Realización", rea_to_show, ")"),
                     xlim =  c(0, input$ancho),
                     ylim =  c(0, input$alto),
                     zlim = c(min_val_rea1 - min_val, max_val_rea1 - diferencia))
    })
    

    
    output$mensaje_resumen <- renderPrint({
      cat("Min. global:", min_val, "\n")
      cat("Min. realización mostrada:", min_val_rea1, "\n")
      cat("Diferencia:", diferencia, "\n")
      cat("Max. realización mostrada:", max_val_rea1, "\n")
      cat("Umbral: ", threshold, "\n")
    })
    
    
  })
}

 
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)