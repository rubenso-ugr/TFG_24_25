# Load packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)

library(bslib)


library(RandomFieldsUtils)
library(RandomFields)

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

T<- c(1,1,1)

rea_to_show <- 1 #Realización a mostrar


# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    title = "TFG Estadística espacial",
    sidebar = sidebar(
      useShinyjs(), #para que no se pueda pulsar el boton de descarga antes de tiempo
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
      nav_panel("Primera simulación", plotOutput("primera_simulacion"), downloadButton("downloadPlot", "Descargar imagen PNG", disabled = TRUE)),
      nav_panel("Conjunto Excursión", plotOutput("conjunto_excursion")),
      nav_panel("Metodología aplicada",
                plotOutput("metodología"),
                selectInput("medida", "Medida de riesgo a usar", 
                            choices = c("VaR Histórico",
                                        "VaR Paramétrico",
                                        "VaR Montecarlo",
                                        "ES"))),
      nav_panel("Resumen", verbatimTextOutput("mensaje_resumen")),
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
    
    
    #Metodologia -----------------------------------------------------------------------
    
    
    # Definimos los parámetros para ventanas deslizantes
    window_size <- 5        # tamaño de ventana 
    overlap <- 1            # solapamiento entre ventanas 
    step <- window_size - overlap
    alpha2 <- 0.05  #Segundo percentil a usar
    
    # Creamos una copia exacta de sim_values_pos para cada medida y metodologia a usar
    refined_values_hist <- excursion_values  # VaR histórico
    refined_values_param <- excursion_values # VaR paramétrico
    refined_values_mc <- excursion_values    # VaR Monte Carlo
    refined_values_es <- excursion_values    # Expected Shortfall
    
    # Recorremos la malla con ventanas deslizantes
    for (i in seq(1, input$ancho, by = step)) {
      for (j in seq(1, input$alto, by = step)) {
        
        # Ajustamos dinámicamente los tamaños de la ventana en i y j para poder recorrer todos los pixeles
        actual_window_size_i <- min(window_size, input$ancho - i + 1)
        actual_window_size_j <- min(window_size, input$alto - j + 1)
        
        # Índices espaciales de la ventana
        indices_i <- i:(i + actual_window_size_i - 1)
        indices_j <- j:(j + actual_window_size_j - 1)
        
        # Creamos la cuadrícula completa de coordenadas
        grid <- expand.grid(i = indices_i, j = indices_j)
        
        # Convertimos a índices lineales
        linear_indices <- (grid$i - 1) * input$alto + grid$j
        
        # Extraemos los datos de la ventana (todas las realizaciones)
        window_data <- excursion_values[linear_indices, , drop = FALSE]
        
        mask_above_global <- window_data > 0
        
        if (any(mask_above_global)) {   # Comprobamos que haya algun valor que supere el umbral
          
          filtered_data <- window_data[mask_above_global] #Tomamos los valores de la ventana que han superado el primer umbral
          
          # VaR Histórico
          var_hist <- quantile(filtered_data, probs = 1 - alpha2) 
          #refined_values_hist[linear_indices, ][mask_above_global] <- var_hist
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          tmp <- refined_values_hist[linear_indices, , drop = FALSE]
          tmp[mask_above_global] <- var_hist
          refined_values_hist[linear_indices, ] <- tmp
          
          # VaR Paramétrico
          mu <- mean(filtered_data)     # media
          sigma <- sd(filtered_data)    # desv. tipica
          z_alpha <- qnorm(1 - alpha2)  # cuantil de distrib. normal estandar a nivel de confianza 1- alpha2
          var_param <- mu + sigma * z_alpha
          #refined_values_param[linear_indices, ][mask_above_global] <- var_param
          
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          tmp_param <- refined_values_param[linear_indices, , drop = FALSE]
          tmp_param[mask_above_global] <- var_param
          refined_values_param[linear_indices, ] <- tmp_param
          
          
          
          # VaR Monte Carlo (simulación normal simple con misma media y sigma)
          sim_mc <- rnorm(10000, mean = mu, sd = sigma)   # conjunto de simulaciones
          var_mc <- quantile(sim_mc, probs = 1 - alpha2)
          #refined_values_mc[linear_indices, ][mask_above_global] <- var_mc
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          tmp_mc <- refined_values_mc[linear_indices, , drop = FALSE]
          tmp_mc[mask_above_global] <- var_mc
          refined_values_mc[linear_indices, ] <- tmp_mc
          
          
          # Expected Shortfall histórico
          es_val <- mean(filtered_data[filtered_data >= var_hist])
          #refined_values_es[linear_indices, ][mask_above_global] <- es_val
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          tmp_es <- refined_values_es[linear_indices, , drop = FALSE]
          tmp_es[mask_above_global] <- es_val
          refined_values_es[linear_indices, ] <- tmp_es
          
          
        }
      }
    }
    
    # Creamos matrices 2D de cada resultado para una realización
    matrix_VaR_hist <- matrix(refined_values_hist[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    matrix_VaR_param <- matrix(refined_values_param[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    matrix_VaR_mc <- matrix(refined_values_mc[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    matrix_ES <- matrix(refined_values_es[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    
    #Obtenemos el maximo de entre todas las simulaciones para que la comparación de colores sea correcta
    max_z <- max(
      refined_values_hist,
      refined_values_param,
      refined_values_mc,
      refined_values_es,
      na.rm = TRUE  # por si acaso hay NA
    )
    
    
  
    
    
    output$metodología <- renderPlot({
      switch(input$medida,
             "VaR Histórico" = filled.contour(x, y, matrix_VaR_hist,
                                              color.palette = mis.colores,
                                              asp = 1,
                                              axes = TRUE,
                                              frame.plot = 0,
                                              main = paste("VaR Histórico 95% - Realización", rea_to_show),
                                              xlim = c(0, input$ancho),
                                              ylim = c(0, input$alto),
                                              zlim = c(0, max_z)),
             "VaR Paramétrico" = filled.contour(x, y, matrix_VaR_param,
                                                color.palette = mis.colores,
                                                asp = 1,
                                                axes = TRUE,
                                                frame.plot = 0,
                                                main = paste("VaR Paramétrico 95% - Realización", rea_to_show),
                                                xlim = c(0, input$ancho),
                                                ylim = c(0, input$alto),
                                                zlim = c(0, max_z)),
             "VaR Montecarlo" =  filled.contour(x, y, matrix_VaR_mc,
                                                color.palette = mis.colores,
                                                asp = 1,
                                                axes = TRUE,
                                                frame.plot = 0,
                                                main = paste("VaR Monte Carlo 95% - Realización", rea_to_show),
                                                xlim = c(0, input$ancho),
                                                ylim = c(0, input$alto),
                                                zlim = c(0, max_z)),
             "ES" = filled.contour(x, y, matrix_ES,
                                   color.palette = mis.colores,
                                   asp = 1,
                                   axes = TRUE,
                                   frame.plot = 0,
                                   main = paste("Expected Shortfall 95% - Realización", rea_to_show),
                                   xlim = c(0, input$ancho),
                                   ylim = c(0, input$alto),
                                   zlim = c(0, max_z)))
    })
    

    
    
    
    
    #------------------------------------------------------------------------------------
    
    output$mensaje_resumen <- renderPrint({
      cat("Min. global:", min_val, "\n")
      cat("Min. realización mostrada:", min_val_rea1, "\n")
      cat("Diferencia:", diferencia, "\n")
      cat("Max. realización mostrada:", max_val_rea1, "\n")
      cat("Umbral: ", threshold, "\n")
    })
    
    #------------------------Descargar imagenes----------------------------------------------------------
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("primera_simulacion", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        filled.contour(x, y, realization_matrix,
                       color.palette = mis.colores,
                       asp = 1,
                       axes = TRUE,
                       frame.plot = 0,
                       main = paste("Realización", rea_to_show),
                       xlim = c(0, input$ancho),
                       ylim = c(0, input$alto))
        dev.off()
      }
    )
    
    
    
  })
  # Habilita el botón de descarga una vez haya un gráfico
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot")
  })
}

 
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)