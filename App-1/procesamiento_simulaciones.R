mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))
rea_to_show <- 1 #Realización a mostrar
n_times <- 4
n_colors <- 100
plotly_colors <- mis.colores(n_colors)

modulo_simulacion <- function(sim_values, x, y, input, output) {

  
  alto <- input$alto + 1
  ancho <- input$ancho + 1
  percentil_1 <- input$percentil_1
  
  min_val <- min(sim_values) #Valor mínimo de entre todas las simulaciones
  max_val <- max(sim_values) #Valor máximo de entre todas las simulaciones
  max_val_rea <- max(sim_values[, rea_to_show]) #Valor máximo de la primera realización o primera simulación
  
  realization_matrix <- matrix(sim_values[, rea_to_show], nrow = ancho, ncol = alto)
  
  #Ajuste de escala de colores para visualización 3D
  color_scale <- generar_color_scale(min_val, max_val, n_colors, plotly_colors)
  
  #Comprobación de la opción escogida para mostrar
  output$primera_simulacion <- renderUI({
    if (input$visualizacion_primera_simulacion == "3D") {
      plotlyOutput("primera_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("primera_ggplot", width = "100%", height = "100%")
    }
  })
  
  
  #Se muestra la primera realización de las diferentes simulaciones realizadas en 2D
  output$primera_ggplot <- renderPlot({
    plot_2d(realization_matrix, x, y, ancho, alto, zlim = c(min_val, max_val_rea))
  })
  
  #Se muestra la primera realización de las diferentes simulaciones realizadas en 3D
  output$primera_plotly <- renderPlotly({
    plot_3d(realization_matrix, color_scale)
  })
  
  ### Conjunto excursion de la primera realización ###
  
  #Estimación del umbral de riesgo (percentil 1 - alpha)
  all_vals <- as.vector(sim_values)
  threshold <- quantile(all_vals, probs = 1 - percentil_1)
  
  #Obtención del conjunto de excursión (asignamos min_val a valores por debajo del umbral en TODAS las realizaciones)
  excursion_values <- sim_values
  excursion_values[sim_values < threshold] <- min_val
  
  #Se obtiene el conjunto de excursión de la realización escogida
  excursion_matrix <- matrix(excursion_values[, rea_to_show], nrow = ancho, ncol = alto)
  
  #Comprobación de la opción escogida para mostrar
  output$conjunto_excursion <- renderUI({
    if (input$visualizacion_excursion == "3D") {
      plotlyOutput("excursion_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("excursion_ggplot", width = "100%", height = "100%")
    }
  })
  
  #Se muestra el conjunto de excursion de la primera realización en 2D
  output$excursion_ggplot <- renderPlot({
    plot_2d(excursion_matrix, x, y, ancho, alto, zlim = c(min_val, max_val_rea))
  })
  
  #Se muestra conjunto de excursion de la primera realización en 3D
  output$excursion_plotly <- renderPlotly({
    plot_3d(excursion_matrix, color_scale)
  })
  
  ### Mapa varianzas ###
  
  # Cálculo de la varianza por filas (cada píxel a través de las n simulaciones)
  varianzas_vector <- apply(sim_values, 1, var)
  
  # Reestructuración a matriz
  mapa_varianzas <- matrix(varianzas_vector, nrow = alto, ncol = ancho)
  
  #Comprobación de la opción escogida para mostrar
  output$mapa_varianzas <- renderUI({
    if (input$visualizacion_varianza == "3D") {
      plotlyOutput("varianzas_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("varianzas_ggplot", width = "100%", height = "100%")
    }
  })
  
  
  #Se muestra el mapa de varianas de las simulaciones realizadas en 2D
  output$varianzas_ggplot <- renderPlot({
    plot_2d(mapa_varianzas, x, y, ancho, alto)
  })
  
  #Se muestra conjunto de excursion de la primera realización en 3D
  output$varianzas_plotly <- renderPlotly({
    plot_3d(mapa_varianzas, color_scale)
  })
  
  ### Resumen ###
  
  output$mensaje_resumen <- renderPrint({
    cat("Min. global:", min_val, "\n")
    cat("Max. global:", max_val, "\n")
    cat("Max. realizacion mostrada:", max_val_rea, "\n")
    cat("Primer umbral de riesgo: ", threshold, "\n")
  })
  
  #Se devuelve el umbral obtenido
  return(threshold)
  
}

#------------------------------------------Simulacion temporal ----------------------------------------

modulo_simulacion_temporal <- function(sim_values, x, y, input, output) {
  
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_1 <- input$percentil_1
  
  # Creación de una lista para almacenar las matrices y diferentes valores por instantes temporales
  realizations_by_time <- vector("list", n_times)
  excursion_by_time <- vector("list", n_times)
  varianza_by_time <- vector("list", n_times)
  threshold_by_time <- vector("list", n_times)
  max_val_rea_by_time <- vector("list", n_times)
  
  min_val <- min(sim_values) #Valor mínimo de entre todas las simulaciones de todos los instantes temporales
  max_val <- max(sim_values) #Valor máximo de entre todas las simulaciones de todos los instantes temporales
  
  # Extracción de la información de cada instante temporal
  for (t in 1:n_times) {
    
    start_row <- (t - 1) * ancho * alto + 1 #Inicio instante temporal t
    end_row <- t * ancho * alto             #Fin instante temporal t
    
    realizations_by_time[[t]] <- matrix(sim_values[start_row:end_row, rea_to_show], nrow = ancho, ncol = alto) #Primera realización en el instante temporal t
    
    max_val_rea_by_time[[t]] <- max(realizations_by_time[[t]])
    
    #Estimación del umbral de riesgo (percentil 1 - alpha)
    all_vals <- as.vector(sim_values[start_row:end_row,])
    threshold_by_time[[t]] <- quantile(all_vals, probs = 1 - percentil_1)
    
    #Obtención del conjunto de excursión (asignamos min_val a valores por debajo del umbral en TODAS las realizaciones)
    excursion_values <- sim_values[start_row:end_row,]
    excursion_values[sim_values[start_row:end_row,] < threshold_by_time[[t]]] <- min_val
    excursion_by_time[[t]] <- matrix(excursion_values[, rea_to_show], nrow = ancho, ncol = alto)
    
    # Cálculo de la varianza por fila (cada píxel a través de las n simulaciones)
    varianzas_vector <- apply(sim_values[start_row:end_row,], 1, var)
    
    # Reestructuración a matriz
    varianza_by_time[[t]] <- matrix(varianzas_vector, nrow = alto, ncol = ancho)
  }
  
  # Valor máximo de la primera realización en los 4 instantes temporales
  max_val_rea_time <- max(c(realizations_by_time[[1]], 
                     realizations_by_time[[2]], 
                     realizations_by_time[[3]], 
                     realizations_by_time[[4]]), 
                   na.rm = TRUE)
  
  #Ajuste de escala de colores para visualización 3D
  color_scale <- generar_color_scale(min_val, max_val, n_colors, plotly_colors)
  
  #Comprobación de la opción escogida para mostrar
  output$primera_simulacion <- renderUI({
    if (input$visualizacion_primera_simulacion == "3D") {
      plotlyOutput("primera_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("primera_ggplot", width = "100%", height = "100%")
    }
  })
  
  
  #Se muestra la primera realización en 2D en función del instante temporal escogido
  output$primera_ggplot <- renderPlot({
    selected_realization <- switch(input$visualizacion_primera_temporal,
                                   "T1" = realizations_by_time[[1]],
                                   "T2" = realizations_by_time[[2]],
                                   "T3" = realizations_by_time[[3]],
                                   "T4" = realizations_by_time[[4]])
    
    plot_2d(selected_realization, x, y, ancho, alto, zlim = c(min_val, max_val_rea_time))
  })
  
  #Se muestra la primera realización en 3D en función del instante temporal escogido
  output$primera_plotly <- renderPlotly({
    
    selected_realization <- switch(input$visualizacion_primera_temporal,
                                   "T1" = realizations_by_time[[1]],
                                   "T2" = realizations_by_time[[2]],
                                   "T3" = realizations_by_time[[3]],
                                   "T4" = realizations_by_time[[4]])
    
    plot_3d(selected_realization, color_scale)
  })
  
  ### Conjunto excursion de la primera realización ###
  
  #Comprobación de la opción escogida para mostrar
  output$conjunto_excursion <- renderUI({
    if (input$visualizacion_excursion == "3D") {
      plotlyOutput("excursion_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("excursion_ggplot", width = "100%", height = "100%")
    }
  })
 
  #Se muestra el conjunto de excursion de la primera realización en función del instante temporal escogido en 2D
  output$excursion_ggplot <- renderPlot({
    selected_excursion <- switch(input$visualizacion_excursion_temporal,
                                   "T1" = excursion_by_time[[1]],
                                   "T2" = excursion_by_time[[2]],
                                   "T3" = excursion_by_time[[3]],
                                   "T4" = excursion_by_time[[4]])
    
    plot_2d(selected_excursion, x, y, ancho, alto, zlim = c(min_val, max_val_rea_time))
  })
  
  #Se muestra conjunto de excursion de la primera realización en 3D
  output$excursion_plotly <- renderPlotly({
    
    selected_excursion <- switch(input$visualizacion_excursion_temporal,
                                "T1" = excursion_by_time[[1]],
                                "T2" = excursion_by_time[[2]],
                                "T3" = excursion_by_time[[3]],
                                "T4" = excursion_by_time[[4]])
    
    plot_3d(selected_excursion, color_scale)
  })
  
  ### Mapa varianzas ###

  #Comprobación de la opción escogida para mostrar
  output$mapa_varianzas <- renderUI({
    if (input$visualizacion_varianza == "3D") {
      plotlyOutput("varianzas_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("varianzas_ggplot", width = "100%", height = "100%")
    }
  })
  
  #Se muestra el mapa de varianzas en función del instante temporal escogido
  output$varianzas_ggplot <- renderPlot({
    selected_varianza <- switch(input$visualizacion_varianza_temporal,
                                "T1" = varianza_by_time[[1]],
                                "T2" = varianza_by_time[[2]],
                                "T3" = varianza_by_time[[3]],
                                "T4" = varianza_by_time[[4]])
    
    plot_2d(selected_varianza, x, y, ancho, alto)
  })
  
  #Se muestra conjunto de excursion de la primera realización en 3D
  output$varianzas_plotly <- renderPlotly({
    
    selected_varianza <- switch(input$visualizacion_varianza_temporal,
                                "T1" = varianza_by_time[[1]],
                                "T2" = varianza_by_time[[2]],
                                "T3" = varianza_by_time[[3]],
                                "T4" = varianza_by_time[[4]])
    
    plot_3d(selected_varianza, color_scale)
  })
  
  ### Resumen ###
  
  output$mensaje_resumen <- renderPrint({
    cat("Min. global:", min_val, "\n")
    cat("Max. global:", max_val, "\n")
    cat("Max. realizacion mostrada T1:", max_val_rea_by_time[[1]], "\n")
    cat("Max. realizacion mostrada T2:", max_val_rea_by_time[[2]], "\n")
    cat("Max. realizacion mostrada T3:", max_val_rea_by_time[[3]], "\n")
    cat("Max. realizacion mostrada T4:", max_val_rea_by_time[[4]], "\n")
    cat("Primer umbral de riesgo T1: ", threshold_by_time[[1]], "\n")
    cat("Primer umbral de riesgo T2: ", threshold_by_time[[2]], "\n")
    cat("Primer umbral de riesgo T3: ", threshold_by_time[[3]], "\n")
    cat("Primer umbral de riesgo T4: ", threshold_by_time[[4]], "\n")
  })
  
  #Se devuelve los umbrales de todos los intantes
  return(threshold_by_time)
  
}
  
#-------------------------------------- Metodologia -----------------------------------------------------------------------
  
modulo_metodologia <- function(sim_values, x, y, input, output, threshold) {
    
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_2 <- input$percentil_2
    
  # Definición de los parámetros para ventanas deslizantes
  window_size <- input$ventana        # tamaño de ventana 
  overlap <- input$solapamiento       # solapamiento entre ventanas 
  step <- window_size - overlap
  
  # Creación de una matriz para cada medida y metodologia a usar
  refined_values_hist <- matrix(NA, nrow = ancho, ncol = alto)  # VaR histórico
  refined_values_param <- matrix(NA, nrow = ancho, ncol = alto) # VaR paramétrico
  refined_values_mc <- matrix(NA, nrow = ancho, ncol = alto)    # VaR Monte Carlo
  refined_values_es <- matrix(NA, nrow = ancho, ncol = alto)    # Expected Shortfall
  
  # Se recorre la malla mediante ventanas deslizantes
  for (i in seq(1, ancho, by = step)) {
    for (j in seq(1, alto, by = step)) {
      
      # Ajuste dinámico de los tamaños de la ventana en i y j para poder recorrer todos los pixeles
      actual_window_size_i <- min(window_size, ancho - i + 1)
      actual_window_size_j <- min(window_size, alto - j + 1)
      
      # Índices espaciales de la ventana
      indices_i <- i:(i + actual_window_size_i - 1)
      indices_j <- j:(j + actual_window_size_j - 1)
      
      # Creación de la cuadrícula completa de coordenadas
      grid <- expand.grid(i = indices_i, j = indices_j)
      
      # Conversión a índices lineales
      linear_indices <- (grid$i - 1) * alto + grid$j
      
      # Extracción de los datos de la ventana (entre todas las realizaciones)
      window_data <- sim_values[linear_indices, , drop = FALSE]
      
      # Máscara que contiene los valores que superan el umbral
      mask_above_global <- window_data >= threshold
      
      if (any(mask_above_global)) {   # Se comprueba que haya algun valor que supere el umbral
        
        #Se toma los valores de la ventana que han superado el primer umbral
        filtered_data <- window_data[mask_above_global] 
        
        ## VaR Histórico ##
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_hist <- threshold
        } else {
          var_hist <- quantile(filtered_data, probs = 1 - percentil_2, na.rm = TRUE)
        }
        
        #Se aplica el valor obtenido a la matriz de valores
        refined_values_hist[linear_indices] <- var_hist
        
        ## VaR Paramétrico ##
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_param <- threshold
        } else {
          mu <- mean(filtered_data, na.rm = TRUE)
          sigma <- sd(filtered_data, na.rm = TRUE)
          z_alpha <- qnorm(1 - percentil_2)
          var_param <- mu + sigma * z_alpha
        }
        
        refined_values_param[linear_indices] <- var_param
        
        ## VaR Monte Carlo ##
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_mc <- threshold
        } else {
          sim_mc <- rnorm(10000, mean = mu, sd = sigma)
          var_mc <- quantile(sim_mc, probs = 1 - percentil_2, na.rm = TRUE)
        }
        
        refined_values_mc[linear_indices] <- var_mc
        
        ## Expected Shortfall Histórico ##
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          es_val <- threshold
        } else {
          es_val <- mean(filtered_data[filtered_data >= var_hist], na.rm = TRUE)
        }
      
        refined_values_es[linear_indices] <- es_val
        
      }
    }
  }
  
  #Obtenemos el maximo y el mínimo entre todas las simulaciones
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
  
  #Ajuste de escala de colores para visualización 3D
  color_scale <- generar_color_scale(min_Z, max_Z, n_colors, plotly_colors)
  
  #Comprobación de la opción escogida para mostrar
  output$metodologia_plot <- renderUI({
    if (input$visualizacion_medida == "3D") {
      plotlyOutput("metodologia_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("metodologia_ggplot", width = "100%", height = "100%")
    }
  })
  
  # Visualización del mapa de valores obtenido por la metodología según la medida escogida en 2D
  output$metodologia_ggplot <- renderPlot({
    values_matrix <- switch(input$medida,
                            "VaR Histórico" = refined_values_hist,
                            "VaR Paramétrico" = refined_values_param,
                            "VaR Montecarlo" = refined_values_mc,
                            "ES" = refined_values_es
    )
    
    plot_2d(values_matrix, x, y, ancho, alto, zlim = c(min_val, max_val_rea_time))
    
  })
  
  # Visualización del mapa de valores obtenido por la metodología según la medida escogida en 3D
  output$metodologia_plotly <- renderPlotly({
    values_matrix <- switch(input$medida,
                            "VaR Histórico" = refined_values_hist,
                            "VaR Paramétrico" = refined_values_param,
                            "VaR Montecarlo" = refined_values_mc,
                            "ES" = refined_values_es
    )
    plot_3d(values_matrix, color_scale)
  })
 
}


#-------------------------------Metodología temporal --------------------------------------------------------------



modulo_metodologia_temporal <- function(sim_values, x, y, input, output, threshold) {
  
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_2 <- input$percentil_2
  
  # Definición de los parámetros para ventanas deslizantes
  window_size <- input$ventana        # tamaño de ventana 
  overlap <- input$solapamiento       # solapamiento entre ventanas 
  step <- window_size - overlap
  
  # Creación de las listas de matrices para cada metodología e instante temporal
  refined_values <- list(
    hist = vector("list", n_times),    # VaR histórico
    param = vector("list", n_times),   # VaR paramétrico
    mc = vector("list", n_times),      # VaR Monte Carlo
    es = vector("list", n_times)       # Expected Shortfall
  )
  
  #Creación de las diferentes matrices
  for (method in names(refined_values)) {
    for (t in 1:n_times) {
      refined_values[[method]][[t]] <- matrix(NA, nrow = ancho, ncol = alto)
    }
  }
  

  # Aplicación de la metodología en cada instante temporal
  for (t in 1:n_times) {
    start_row <- (t - 1) * ancho * alto + 1
    end_row <- t * ancho * alto
    valores <- sim_values[start_row:end_row,]
    
  # Se recorre la malla con ventanas deslizantes
    for (i in seq(1, ancho, by = step)) {
      for (j in seq(1, alto, by = step)) {
        
        # Ajuste dinámico de los tamaños de la ventana en i y j para poder recorrer todos los pixeles
        actual_window_size_i <- min(window_size, ancho - i + 1)
        actual_window_size_j <- min(window_size, alto - j + 1)
        
        # Índices espaciales de la ventana
        indices_i <- i:(i + actual_window_size_i - 1)
        indices_j <- j:(j + actual_window_size_j - 1)
        
        # Creación de la cuadrícula completa de coordenadas
        grid <- expand.grid(i = indices_i, j = indices_j)
        
        # Conversión a índices lineales
        linear_indices <- (grid$i - 1) * alto + grid$j
        
        # Extracción de los datos de la ventana (entre todas las realizaciones)
        window_data <- valores[linear_indices, , drop = FALSE]
        
        # Máscara que contiene los valores que superan el umbral
        mask_above_global <- window_data >= threshold[[t]]
        
        if (any(mask_above_global)) {   # Se comprueba que haya algun valor que supere el umbral
          
          #Se toma los valores de la ventana que han superado el primer umbral
          filtered_data <- window_data[mask_above_global] 
          
          ## VaR Histórico ##
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_hist <- threshold[[t]]
          } else {
            var_hist <- quantile(filtered_data, probs = 1 - percentil_2, na.rm = TRUE)
          }
          
          #Se aplica el valor obtenido a la matriz de valores
          refined_values$hist[[t]][linear_indices] <- var_hist

          ## VaR Paramétrico ##
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_param <- threshold[[t]]
          } else {
            mu <- mean(filtered_data, na.rm = TRUE)
            sigma <- sd(filtered_data, na.rm = TRUE)
            z_alpha <- qnorm(1 - percentil_2)
            var_param <- mu + sigma * z_alpha
          }
          
          refined_values$param[[t]][linear_indices] <- var_param
          
          ## VaR Monte Carlo ##
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_mc <- threshold[[t]]
          } else {
            sim_mc <- rnorm(10000, mean = mu, sd = sigma)
            var_mc <- quantile(sim_mc, probs = 1 - percentil_2, na.rm = TRUE)
          }
          
          refined_values$mc[[t]][linear_indices] <- var_mc
          
          ## Expected Shortfall Histórico ##
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            es_val <- threshold[[t]]
          } else {
            es_val <- mean(filtered_data[filtered_data >= var_hist], na.rm = TRUE)
          }
          
          refined_values$es[[t]][linear_indices] <- es_val
          
        }
      }
    }
  }
  
  
  #Obtención del maximo y el mínimo entre todas las simulaciones entre todos los instantes temporales
  max_z <- max(
    unlist(lapply(refined_values$hist, max, na.rm = TRUE)),
    unlist(lapply(refined_values$param, max, na.rm = TRUE)),
    unlist(lapply(refined_values$mc, max, na.rm = TRUE)),
    unlist(lapply(refined_values$es, max, na.rm = TRUE)),
    na.rm = TRUE
  )
  
  min_z <- min(
    unlist(lapply(refined_values$hist, min, na.rm = TRUE)),
    unlist(lapply(refined_values$param, min, na.rm = TRUE)),
    unlist(lapply(refined_values$mc, min, na.rm = TRUE)),
    unlist(lapply(refined_values$es, min, na.rm = TRUE)),
    na.rm = TRUE
  )
  
  #Ajuste de escala de colores para visualización 3D
  color_scale <- generar_color_scale(min_Z, max_Z, n_colors, plotly_colors)

  #Comprobación de la opción escogida para mostrar
  output$metodologia_plot <- renderUI({
    if (input$visualizacion_medida == "3D") {
      plotlyOutput("metodologia_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("metodologia_ggplot", width = "100%", height = "100%")
    }
  })
  
  # Visualización del mapa de valores obtenido por la metodología según la medida y el instante temporal escogidos en 2D
  output$metodologia_ggplot <- renderPlot({
    # Selecciona la lista de matrices según la medida de riesgo
    values_matrix_list <- switch(input$medida,
                                 "VaR Histórico" = refined_values$hist,
                                 "VaR Paramétrico" = refined_values$param,
                                 "VaR Montecarlo" = refined_values$mc,
                                 "ES" = refined_values$es
    )
    
    # Selecciona la matriz específica según el instante temporal
    values_matrix <- switch(input$visualizacion_medida_temporal,
                            "T1" = values_matrix_list[[1]],
                            "T2" = values_matrix_list[[2]],
                            "T3" = values_matrix_list[[3]],
                            "T4" = values_matrix_list[[4]]
    )
    
    plot_2d(values_matrix, x, y, ancho, alto, zlim = c(min_val, max_val_rea_time))
  })
  
  # Visualización del mapa de valores obtenido por la metodología según la medida y el instante temporal escogidos en 3D
  output$metodologia_plotly <- renderPlotly({
    # Selecciona la lista de matrices según la medida de riesgo
    values_matrix_list <- switch(input$medida,
                                 "VaR Histórico" = refined_values$hist,
                                 "VaR Paramétrico" = refined_values$param,
                                 "VaR Montecarlo" = refined_values$mc,
                                 "ES" = refined_values$es
    )
    
    # Selecciona la matriz específica según el instante temporal
    values_matrix <- switch(input$visualizacion_medida_temporal,
                            "T1" = values_matrix_list[[1]],
                            "T2" = values_matrix_list[[2]],
                            "T3" = values_matrix_list[[3]],
                            "T4" = values_matrix_list[[4]]
    )

    plot_3d(values_matrix, color_scale)

  })
  
}


### Funciones Auxiliares para Validación de datos ###

validar_parametro_ab_cer <- function(nombre, valor, min_valor, max_valor) {
  
  # Verificar que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verificar que esté en el rango correcto
  if (valor <= min_valor || valor > max_valor) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "debe estar en el rango (", min_valor, ",", max_valor, "]."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

validar_parametro_ab_ab <- function(nombre, valor, min_valor, max_valor = Inf) {
  
  # Verificar que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verificar que esté en el rango correcto
  if (valor <= min_valor || valor >= max_valor) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "debe estar en el rango (", min_valor, ",", max_valor, "]."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

validar_parametro_cer_ab <- function(nombre, valor, min_valor, max_valor = Inf) {
  
  # Verificar que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verificar que esté en el rango correcto
  if (valor < min_valor || valor >= max_valor) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "debe estar en el rango (", min_valor, ",", max_valor, "]."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

validar_parametro_cer_cer <- function(nombre, valor, min_valor, max_valor) {
  
  # Verificar que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verificar que esté en el rango correcto
  if (valor < min_valor || valor > max_valor) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "debe estar en el rango [", min_valor, ",", max_valor, "]."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  return(TRUE)
}


generar_color_scale <- function(min_val, max_val, n_colors, colores) {
  lapply(seq(min_val, max_val, length.out = n_colors), function(val) {
    scaled_val <- (val - min_val) / (max_val - min_val)
    list(scaled_val, colores[round(scaled_val * (n_colors - 1)) + 1])
  })
}

plot_2d <- function(mat, x, y, ancho, alto, zlim = NULL) {
  if (is.null(zlim)) {
    # Sin zlim especificado
    filled.contour(x, y, mat,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   xlim = c(0, ancho),
                   ylim = c(0, alto))
  } else {
    # Con zlim especificado
    filled.contour(x, y, mat,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   xlim = c(0, ancho),
                   ylim = c(0, alto),
                   zlim = zlim)
  }
}

plot_3d <- function(mat, color_scale) {
  plot_ly(z = t(mat), name = "") %>% 
    add_surface(
      colorscale = color_scale,
      contours = list(
        z = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "white",
          project = list(z = TRUE)
        )
      )
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Valor")
      )
    )
}


