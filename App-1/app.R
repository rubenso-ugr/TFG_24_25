# Load packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)

library(bslib)


library(RandomFieldsUtils)
library(RandomFields)

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

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
      numericInput("ventana", "Tamaño ventana", value = 5, min = 2, max = 50),
      numericInput("solapamiento", "Solapamiento", value = 2, min = 1, max = 15),
      actionButton("button", "Calcular")
    ),
    
    # Panel derecho con pestañas
    navset_card_underline(
      nav_panel("Primera simulación", plotOutput("primera_simulacion"), downloadButton("downloadPlot1", "Descargar imagen PNG", disabled = TRUE)),
      nav_panel("Conjunto Excursión", plotOutput("conjunto_excursion"), downloadButton("downloadPlot2", "Descargar imagen PNG", disabled = TRUE)),
      nav_panel("Mapa de varianzas",  plotOutput("mapa_varianzas"), downloadButton("downloadPlot3", "Descargar imagen PNG", disabled = TRUE)),
      nav_panel("Metodología aplicada",
                plotOutput("metodología"),
                selectInput("medida", "Medida de riesgo a usar", 
                            choices = c("VaR Histórico",
                                        "VaR Paramétrico",
                                        "VaR Montecarlo",
                                        "ES")),
                downloadButton("downloadPlot3", "Descargar imagen PNG", disabled = TRUE)),
      nav_panel("Resumen", verbatimTextOutput("mensaje_resumen")),
    )
    
    
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  #Consistencia tamaños ventana y solapamiento
  
  observe({
    # Límite superior para la ventana: mínimo entre alto y ancho
    max_ventana <- min(input$ancho, input$alto)
    
    # Actualizamos el input de la ventana
    updateNumericInput(session, "ventana", max = max_ventana)
    
    # Aseguramos que el valor actual de ventana no sea mayor que el nuevo máximo
    if (input$ventana > max_ventana) {
      updateNumericInput(session, "ventana", value = max_ventana)
    }
  })
  
  observe({
    # El máximo de solapamiento es el tamaño actual de la ventana
    updateNumericInput(session, "solapamiento", max = input$ventana)
    
    # Aseguramos que el valor actual de solapamiento no sea mayor que el nuevo máximo
    if (input$solapamiento > input$ventana) {
      updateNumericInput(session, "solapamiento", value = input$ventana-1)
    }
  })
  
  
  
  
  observeEvent(input$button, {
    
    if (input$alpha <= 0 || input$alpha > 2) {
      showModal(modalDialog(
        title = "Error en parámetro alpha",
        "El valor de alpha debe estar en el intervalo (0, 2].",
        easyClose = TRUE
      ))
      return(NULL)  # no continúa la ejecución
    }
    
    if (input$beta <= 0) {
      showModal(modalDialog(
        title = "Error en parámetro beta",
        "El valor de beta debe ser mayor que 0.",
        easyClose = TRUE
      ))
      return(NULL)  # no continúa la ejecución
    }
    
    
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
    

    min_val <- min(sim_values)
    max_val <- max(sim_values)
    max_val_rea <- max(sim_values[, rea_to_show])
    
    realization_matrix <- matrix(sim_values[, rea_to_show], nrow = input$ancho, ncol = input$alto)
    
    
    output$primera_simulacion <- renderPlot({
      # Dibujamos la realización escogida
      filled.contour(x, y, realization_matrix,
                     color.palette = mis.colores,
                     asp = 1,
                     axes = TRUE,
                     frame.plot = 0,
                     main = paste("Realización", rea_to_show),
                     xlim =  c(0, input$ancho),
                     ylim =  c(0, input$alto),
                     zlim = c(min_val, max_val_rea))
    })
    
    
    
    #Estimamos el umbral de riesgo (percentil 1 - alpha)
    all_vals <- as.vector(sim_values)
    threshold <- quantile(all_vals, probs = 1 - input$percentil)
    
    #Obtenemos el conjunto de excursión (asignamos min_val a valores por debajo del umbral en TODAS las realizaciones)
    excursion_values <- sim_values
    excursion_values[sim_values < threshold] <- min_val
    
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
                     zlim = c(min_val, max_val_rea))
    })
    
    
    #Mapa varianzas --------------------------------------------------------------------
    
    # Calculamos la varianza por fila (cada píxel a través de las n simulaciones)
    varianzas_vector <- apply(sim_values, 1, var)
    
    # Lo reestructuramos a matriz 200x200
    mapa_varianzas <- matrix(varianzas_vector, nrow = input$alto, ncol = input$ancho)
    
    #Dibujamos el mapa de varianas de las simulaciones realizadas
    output$mapa_varianzas <- renderPlot({
      filled.contour(x, y, mapa_varianzas,
                     color.palette = mis.colores,
                     asp = 1,
                     axes = TRUE,
                     frame.plot = 0,
                     main = paste("Mapa de varianzas"),
                     xlim = c(0, input$ancho),
                     ylim = c(0, input$alto))
    })
    
    
    
    
    #Metodologia -----------------------------------------------------------------------
    
    
    # Definimos los parámetros para ventanas deslizantes
    window_size <- input$ventana        # tamaño de ventana 
    overlap <- input$solapamiento            # solapamiento entre ventanas 
    step <- window_size - overlap
    alpha2 <- 0.05  #Segundo percentil a usar
    
    # Creamos una copia exacta de sim_values_pos para cada medida y metodologia a usar
    refined_values_hist <- matrix(NA, nrow = input$ancho, ncol = input$alto)  # VaR histórico
    refined_values_param <- matrix(NA, nrow = input$ancho, ncol = input$alto) # VaR paramétrico
    refined_values_mc <- matrix(NA, nrow = input$ancho, ncol = input$alto)    # VaR Monte Carlo
    refined_values_es <- matrix(NA, nrow = input$ancho, ncol = input$alto)    # Expected Shortfall
    
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
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          refined_values_hist[linear_indices] <- var_hist
          
          # VaR Paramétrico
          mu <- mean(filtered_data)     # media
          sigma <- sd(filtered_data)    # desv. tipica
          z_alpha <- qnorm(1 - alpha2)  # cuantil de distrib. normal estandar a nivel de confianza 1- alpha2
          var_param <- mu + sigma * z_alpha
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          
          refined_values_param[linear_indices] <- var_param
          
          
          
          # VaR Monte Carlo (simulación normal simple con misma media y sigma)
          sim_mc <- rnorm(10000, mean = mu, sd = sigma)   # conjunto de simulaciones
          var_mc <- quantile(sim_mc, probs = 1 - alpha2)
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          refined_values_mc[linear_indices] <- var_mc
          
          
          # Expected Shortfall histórico
          es_val <- mean(filtered_data[filtered_data >= var_hist])
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          refined_values_es[linear_indices] <- es_val
          
        }
      }
    }
    
   
    
    #Obtenemos el maximo y el mínimo entre todas las simulaciones para que la comparación de colores sea correcta
    max_z <- max(
      refined_values_hist,
      refined_values_param,
      refined_values_mc,
      refined_values_es,
      na.rm = TRUE  # por si acaso hay NA
    )
    
    min_z <- min(
      refined_values_hist,
      refined_values_param,
      refined_values_mc,
      refined_values_es,
      na.rm = TRUE  # por si acaso hay NA
    )
    
    
  
    
    
    output$metodología <- renderPlot({
      switch(input$medida,
             "VaR Histórico" = filled.contour(x, y, refined_values_hist,
                                              color.palette = mis.colores,
                                              asp = 1,
                                              axes = TRUE,
                                              frame.plot = 0,
                                              main = sprintf("VaR Histórico %.0f%%", (1 - alpha2) * 100),
                                              xlim = c(0, input$ancho),
                                              ylim = c(0, input$alto)),
             
             "VaR Paramétrico" = filled.contour(x, y, refined_values_param,
                                                color.palette = mis.colores,
                                                asp = 1,
                                                axes = TRUE,
                                                frame.plot = 0,
                                                main = sprintf("VaR Paramétrico %.0f%%", (1 - alpha2) * 100),
                                                xlim = c(0, input$ancho),
                                                ylim = c(0, input$alto)),
             
             "VaR Montecarlo" =  filled.contour(x, y, refined_values_mc,
                                                color.palette = mis.colores,
                                                asp = 1,
                                                axes = TRUE,
                                                frame.plot = 0,
                                                main = sprintf("VaR Monte Carlo %.0f%%", (1 - alpha2) * 100),
                                                xlim = c(0, input$ancho),
                                                ylim = c(0, input$alto)),
             
             "ES" = filled.contour(x, y, refined_values_es,
                                   color.palette = mis.colores,
                                   asp = 1,
                                   axes = TRUE,
                                   frame.plot = 0,
                                   main = sprintf("Expected Shortfall %.0f%%", (1 - alpha2) * 100),
                                   xlim = c(0, input$ancho),
                                   ylim = c(0, input$alto)))
    })
    

    
    
    
    
    #------------------------------------------------------------------------------------
    
    output$mensaje_resumen <- renderPrint({
      cat("Min. global:", min_val, "\n")
      cat("Max. global:", max_val, "\n")
      cat("Max. realizacion mostrada:", max_val_rea, "\n")
      cat("Umbral: ", threshold, "\n")
    })
    
    #------------------------Descargar imagenes----------------------------------------------------------
    
    output$downloadPlot1 <- downloadHandler(
      filename = function() {
        paste("simulacion", Sys.Date(), ".png", sep = "")
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
    
    output$downloadPlot2 <- downloadHandler(
      filename = function() {
        paste("simulacion_excursion", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        filled.contour(x, y, excursion_matrix,
                       color.palette = mis.colores,
                       asp = 1,
                       axes = TRUE,
                       frame.plot = 0,
                       main = paste("Conjunto de Excursión (Realización", rea_to_show, ")"),
                       xlim =  c(0, input$ancho),
                       ylim =  c(0, input$alto),
                       zlim = c(min_val_rea1 - min_val, max_val_rea1 - diferencia))
        dev.off()
      }
    )
    
    output$downloadPlot3 <- downloadHandler(
      filename = function() {
        paste("mapa_varianzas", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        filled.contour(x, y, excursion_matrix,
                       color.palette = mis.colores,
                       asp = 1,
                       axes = TRUE,
                       frame.plot = 0,
                       main = paste("Mapa de Varianzas"),
                       xlim =  c(0, input$ancho),
                       ylim =  c(0, input$alto))
        dev.off()
      }
    )
    
    
    
  })
  
  
  
  # Habilitamos el botón de descarga una vez haya un gráfico
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot1")
  })
  
  # Habilitamos el botón de descarga una vez haya un gráfico
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot2")
  })
  
  # Habilitamos el botón de descarga una vez haya un gráfico
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot3")
  })
  
}

 
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)