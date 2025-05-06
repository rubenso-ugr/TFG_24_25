modulo_simulacion <- function(sim_values, x, y, input, output, rea_to_show, mis.colores) {
  
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
  
  
}
