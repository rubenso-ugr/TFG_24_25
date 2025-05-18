mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))
rea_to_show <- 1 #Realización a mostrar
n_times <- 4

modulo_simulacion <- function(sim_values, x, y, input, output) {

  
  alto <- input$alto + 1
  ancho <- input$ancho + 1
  percentil_1 <- input$percentil_1
  
  min_val <- min(sim_values)
  max_val <- max(sim_values)
  max_val_rea <- max(sim_values[, rea_to_show])
  
  realization_matrix <- matrix(sim_values[, rea_to_show], nrow = ancho, ncol = alto)
  
  
  n_colors <- 100
  plotly_colors <- mis.colores(n_colors)
  
  
  color_scale <- lapply(seq(min_val, max_val, length.out = n_colors), function(val) {
    scaled_val <- (val - min_val) / (max_val - min_val)
    list(scaled_val, plotly_colors[round(scaled_val * (n_colors - 1)) + 1])
  })
  
  output$primera_simulacion <- renderUI({
    if (input$visualizacion_primera_simulacion == "3D") {
      plotlyOutput("primera_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("primera_ggplot", width = "100%", height = "100%")
    }
  })
  
  
  
  output$primera_ggplot <- renderPlot({
    filled.contour(x, y, realization_matrix,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Realización", rea_to_show),
                   xlim =  c(0, ancho),
                   ylim =  c(0, alto),
                   zlim = c(min_val, max_val_rea))
  })
  
  output$primera_plotly <- renderPlotly({

    # Renderiza el gráfico 3D
    plot_ly(z = ~t(realization_matrix)) %>% 
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
  })
  
  
  
  #------------------------------------------------------------------
  
  
  #Estimamos el umbral de riesgo (percentil 1 - alpha)
  all_vals <- as.vector(sim_values)
  threshold <- quantile(all_vals, probs = 1 - percentil_1)

  
  #Obtenemos el conjunto de excursión (asignamos min_val a valores por debajo del umbral en TODAS las realizaciones)
  excursion_values <- sim_values
  excursion_values[sim_values < threshold] <- min_val
  

  
  #Tomamos el conjunto de excursión de la realización escogida
  excursion_matrix <- matrix(excursion_values[, rea_to_show], nrow = ancho, ncol = alto)
  
  output$conjunto_excursion <- renderPlot({
    filled.contour(x, y, excursion_matrix,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Conjunto de Excursión (Realización", rea_to_show, ")"),
                   xlim =  c(0, ancho),
                   ylim =  c(0, alto),
                   zlim = c(min_val, max_val_rea))
  })
  
  
  
  
  #Mapa varianzas --------------------------------------------------------------------
  
  # Calculamos la varianza por fila (cada píxel a través de las n simulaciones)
  varianzas_vector <- apply(sim_values, 1, var)
  
  # Lo reestructuramos a matriz 200x200
  mapa_varianzas <- matrix(varianzas_vector, nrow = alto, ncol = ancho)
  
  #Dibujamos el mapa de varianas de las simulaciones realizadas
  output$mapa_varianzas <- renderPlot({
    filled.contour(x, y, mapa_varianzas,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Mapa de varianzas"),
                   xlim = c(0, ancho),
                   ylim = c(0, alto))
  })
  
  #----------------------- Resumen -------------------------------------------------------------
  
  output$mensaje_resumen <- renderPrint({
    cat("Min. global:", min_val, "\n")
    cat("Max. global:", max_val, "\n")
    cat("Max. realizacion mostrada:", max_val_rea, "\n")
    cat("Primer Umbral: ", threshold, "\n")
  })
  
  
  return(threshold)
  
}

#------------------------------------------Simulacion temporal ----------------------------------------


modulo_simulacion_temporal <- function(sim_values, x, y, input, output) {
  
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_1 <- input$percentil_1
  
  # Crear una lista para almacenar las matrices por tiempo
  realizations_by_time <- vector("list", n_times)
  excursion_by_time <- vector("list", n_times)
  varianza_by_time <- vector("list", n_times)
  max_val_rea_by_time <- vector("list", n_times)
  threshold_by_time <- vector("list", n_times)
  
  min_val <- min(sim_values)
  max_val <- max(sim_values)
  
  # Extraer cada instante temporal para la realización seleccionada
  for (t in 1:n_times) {
    start_row <- (t - 1) * ancho * alto + 1
    end_row <- t * ancho * alto
    
    max_val_rea_by_time <- max(sim_values[start_row:end_row, rea_to_show])
    
    realizations_by_time[[t]] <- matrix(sim_values[start_row:end_row, rea_to_show], nrow = ancho, ncol = alto)
    
    
    
    #Estimamos el umbral de riesgo (percentil 1 - alpha)
    all_vals <- as.vector(sim_values[start_row:end_row,])
    threshold_by_time[[t]] <- quantile(all_vals, probs = 1 - percentil_1)
    
    
    
    #Obtenemos el conjunto de excursión (asignamos min_val a valores por debajo del umbral en TODAS las realizaciones)
    excursion_values <- sim_values[start_row:end_row,]
    excursion_values[sim_values[start_row:end_row,] < threshold_by_time[[t]]] <- min_val
    
    excursion_by_time[[t]] <- matrix(excursion_values[, rea_to_show], nrow = ancho, ncol = alto)
    
    
    # Calculamos la varianza por fila (cada píxel a través de las n simulaciones)
    varianzas_vector <- apply(sim_values[start_row:end_row,], 1, var)
    
    # Lo reestructuramos a matriz 200x200
    varianza_by_time[[t]] <- matrix(varianzas_vector, nrow = alto, ncol = ancho)
    
    
  }
  
  n_colors <- 100
  plotly_colors <- mis.colores(n_colors)
  
  
  color_scale <- lapply(seq(min_val, max_val, length.out = n_colors), function(val) {
    scaled_val <- (val - min_val) / (max_val - min_val)
    list(scaled_val, plotly_colors[round(scaled_val * (n_colors - 1)) + 1])
  })
  
  output$primera_simulacion <- renderUI({
    if (input$visualizacion_primera_simulacion == "3D") {
      plotlyOutput("primera_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("primera_ggplot", width = "100%", height = "100%")
    }
  })
  
  #-------Primera realizacion
  
  output$primera_ggplot <- renderPlot({
    # Elige la realización basada en el input
    selected_realization <- switch(input$visualizacion_primera_temporal,
                                   "T1" = realizations_by_time[[1]],
                                   "T2" = realizations_by_time[[2]],
                                   "T3" = realizations_by_time[[3]],
                                   "T4" = realizations_by_time[[4]])
    
    # Genera el gráfico
    filled.contour(x, y, selected_realization,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Tiempo:", input$visualizacion_primera_temporal),
                   xlim = c(0, ancho),
                   ylim = c(0, alto),
                   zlim = c(min_val, max_val))
  })
  
  #-------Primera realizacion 3D

  
  output$primera_plotly <- renderPlotly({
    
    # Elige la realización basada en el input
    selected_realization <- switch(input$visualizacion_primera_temporal,
                                   "T1" = realizations_by_time[[1]],
                                   "T2" = realizations_by_time[[2]],
                                   "T3" = realizations_by_time[[3]],
                                   "T4" = realizations_by_time[[4]])
    
    # Renderiza el gráfico 3D
    plot_ly(z = ~t(selected_realization)) %>% 
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
  })
 
  #-------Conjunto excursion de la primera realizacion
  
  output$conjunto_excursion <- renderPlot({
    # Elige la varianza basada en el input
    selected_excursion <- switch(input$visualizacion_excursion_temporal,
                                   "T1" = excursion_by_time[[1]],
                                   "T2" = excursion_by_time[[2]],
                                   "T3" = excursion_by_time[[3]],
                                   "T4" = excursion_by_time[[4]])
    filled.contour(x, y, selected_excursion,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Conjunto de Excursión (Realización", rea_to_show, "):", input$visualizacion_excursion_temporal),
                   xlim =  c(0, ancho),
                   ylim =  c(0, alto),
                   zlim = c(min_val, max_val))
  })
  

  #Mapa varianzas --------------------------------------------------------------------
  
  #Dibujamos el mapa de varianas de las simulaciones realizadas
  output$mapa_varianzas <- renderPlot({
    # Elige la varianza basada en el input
    selected_varianza <- switch(input$visualizacion_varianza_temporal,
                                "T1" = varianza_by_time[[1]],
                                "T2" = varianza_by_time[[2]],
                                "T3" = varianza_by_time[[3]],
                                "T4" = varianza_by_time[[4]])
    
    filled.contour(x, y, selected_varianza,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = paste("Mapa de varianzas:", input$visualizacion_varianza_temporal),
                   xlim = c(0, ancho),
                   ylim = c(0, alto))
  })
  
  return(threshold_by_time)
  
}

  
  
#Metodologia -----------------------------------------------------------------------
  
modulo_metodologia <- function(sim_values, x, y, input, output, threshold) {
    
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_2 <- input$percentil_2
    
  # Definimos los parámetros para ventanas deslizantes
  window_size <- input$ventana        # tamaño de ventana 
  overlap <- input$solapamiento            # solapamiento entre ventanas 
  step <- window_size - overlap
  
  # Creamos una matriz para cada medida y metodologia a usar
  refined_values_hist <- matrix(NA, nrow = ancho, ncol = alto)  # VaR histórico
  refined_values_param <- matrix(NA, nrow = ancho, ncol = alto) # VaR paramétrico
  refined_values_mc <- matrix(NA, nrow = ancho, ncol = alto)    # VaR Monte Carlo
  refined_values_es <- matrix(NA, nrow = ancho, ncol = alto)    # Expected Shortfall
  
  # Recorremos la malla con ventanas deslizantes
  for (i in seq(1, ancho, by = step)) {
    for (j in seq(1, alto, by = step)) {
      
      # Ajustamos dinámicamente los tamaños de la ventana en i y j para poder recorrer todos los pixeles
      actual_window_size_i <- min(window_size, ancho - i + 1)
      actual_window_size_j <- min(window_size, alto - j + 1)
      
      # Índices espaciales de la ventana
      indices_i <- i:(i + actual_window_size_i - 1)
      indices_j <- j:(j + actual_window_size_j - 1)
      
      # Creamos la cuadrícula completa de coordenadas
      grid <- expand.grid(i = indices_i, j = indices_j)
      
      # Convertimos a índices lineales
      linear_indices <- (grid$i - 1) * alto + grid$j
      
      # Extraemos los datos de la ventana (todas las realizaciones)
      window_data <- sim_values[linear_indices, , drop = FALSE]
      
      mask_above_global <- window_data >= threshold
      
      if (any(mask_above_global)) {   # Comprobamos que haya algun valor que supere el umbral
        
        filtered_data <- window_data[mask_above_global] #Tomamos los valores de la ventana que han superado el primer umbral
        
        # VaR Histórico
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_hist <- threshold
        } else {
          var_hist <- quantile(filtered_data, probs = 1 - percentil_2, na.rm = TRUE)
        }
        
        refined_values_hist[linear_indices] <- var_hist
        
        # VaR Paramétrico
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_param <- threshold
        } else {
          mu <- mean(filtered_data, na.rm = TRUE)
          sigma <- sd(filtered_data, na.rm = TRUE)
          z_alpha <- qnorm(1 - percentil_2)
          var_param <- mu + sigma * z_alpha
        }
        
        refined_values_param[linear_indices] <- var_param
        
        # VaR Monte Carlo
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          var_mc <- threshold
        } else {
          sim_mc <- rnorm(10000, mean = mu, sd = sigma)
          var_mc <- quantile(sim_mc, probs = 1 - percentil_2, na.rm = TRUE)
        }
        
        refined_values_mc[linear_indices] <- var_mc
        
        # Expected Shortfall Histórico
        if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
          es_val <- threshold
        } else {
          es_val <- mean(filtered_data[filtered_data >= var_hist], na.rm = TRUE)
        }
      
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
  
  
  n_colors <- 100
  plotly_colors <- mis.colores(n_colors)
  
  
  color_scale <- lapply(seq(min_z, max_z, length.out = n_colors), function(val) {
    scaled_val <- (val - min_z) / (max_z - min_z)
    list(scaled_val, plotly_colors[round(scaled_val * (n_colors - 1)) + 1])
  })
  
  
  output$metodologia_plot <- renderUI({
    if (input$visualizacion_medida == "3D") {
      plotlyOutput("metodologia_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("metodologia_ggplot", width = "100%", height = "100%")
    }
  })
  
  output$metodologia_ggplot <- renderPlot({
    # Datos para la visualización 2D
    values_matrix <- switch(input$medida,
                            "VaR Histórico" = refined_values_hist,
                            "VaR Paramétrico" = refined_values_param,
                            "VaR Montecarlo" = refined_values_mc,
                            "ES" = refined_values_es
    )
    
    # Renderiza el gráfico 2D
    filled.contour(x, y, values_matrix,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = sprintf("%s %.0f%%", input$medida, (1 - percentil_2) * 100),
                   xlim = c(0, ancho),
                   ylim = c(0, alto))
  })
  
  output$metodologia_plotly <- renderPlotly({
    # Datos para la visualización 3D
    values_matrix <- switch(input$medida,
                            "VaR Histórico" = refined_values_hist,
                            "VaR Paramétrico" = refined_values_param,
                            "VaR Montecarlo" = refined_values_mc,
                            "ES" = refined_values_es
    )
    
    # Renderiza el gráfico 3D
    plot_ly(z = ~t(values_matrix)) %>% 
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
  })
 
}


#-------------------------------Metodología temporal --------------------------------------------------------------



modulo_metodologia_temporal <- function(sim_values, x, y, input, output, threshold) {
  
  alto <- input$alto +1
  ancho <- input$ancho +1
  percentil_2 <- input$percentil_2
  
  # Definimos los parámetros para ventanas deslizantes
  window_size <- input$ventana        # tamaño de ventana 
  overlap <- input$solapamiento            # solapamiento entre ventanas 
  step <- window_size - overlap
  
  # Crear las listas de matrices para cada metodología
  refined_values <- list(
    hist = vector("list", n_times),    # VaR histórico
    param = vector("list", n_times),   # VaR paramétrico
    mc = vector("list", n_times),      # VaR Monte Carlo
    es = vector("list", n_times)       # Expected Shortfall
  )
  
  for (method in names(refined_values)) {
    for (t in 1:n_times) {
      refined_values[[method]][[t]] <- matrix(NA, nrow = ancho, ncol = alto)
    }
  }
  

  # Extraer cada instante temporal para la realización seleccionada
  for (t in 1:n_times) {
    start_row <- (t - 1) * ancho * alto + 1
    end_row <- t * ancho * alto
    valores <- sim_values[start_row:end_row,]
    
  # Recorremos la malla con ventanas deslizantes
    for (i in seq(1, ancho, by = step)) {
      for (j in seq(1, alto, by = step)) {
        
        # Ajustamos dinámicamente los tamaños de la ventana en i y j para poder recorrer todos los pixeles
        actual_window_size_i <- min(window_size, ancho - i + 1)
        actual_window_size_j <- min(window_size, alto - j + 1)
        
        # Índices espaciales de la ventana
        indices_i <- i:(i + actual_window_size_i - 1)
        indices_j <- j:(j + actual_window_size_j - 1)
        
        # Creamos la cuadrícula completa de coordenadas
        grid <- expand.grid(i = indices_i, j = indices_j)
        
        # Convertimos a índices lineales
        linear_indices <- (grid$i - 1) * alto + grid$j
        
        # Extraemos los datos de la ventana (todas las realizaciones)
        window_data <- valores[linear_indices, , drop = FALSE]
        
        mask_above_global <- window_data >= threshold[[t]]
        
        if (any(mask_above_global)) {   # Comprobamos que haya algun valor que supere el umbral
          
          filtered_data <- window_data[mask_above_global] #Tomamos los valores de la ventana que han superado el primer umbral
          
          
          # VaR Histórico
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_hist <- threshold[[t]]
          } else {
            var_hist <- quantile(filtered_data, probs = 1 - percentil_2, na.rm = TRUE)
          }
          
          refined_values$hist[[t]][linear_indices] <- var_hist

          
          
          # VaR Paramétrico
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_param <- threshold[[t]]
          } else {
            mu <- mean(filtered_data, na.rm = TRUE)
            sigma <- sd(filtered_data, na.rm = TRUE)
            z_alpha <- qnorm(1 - percentil_2)
            var_param <- mu + sigma * z_alpha
          }
          
          refined_values$param[[t]][linear_indices] <- var_param
          
          # VaR Monte Carlo
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            var_mc <- threshold[[t]]
          } else {
            sim_mc <- rnorm(10000, mean = mu, sd = sigma)
            var_mc <- quantile(sim_mc, probs = 1 - percentil_2, na.rm = TRUE)
          }
          
          refined_values$mc[[t]][linear_indices] <- var_mc
          
          # Expected Shortfall Histórico
          if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
            es_val <- threshold[[t]]
          } else {
            es_val <- mean(filtered_data[filtered_data >= var_hist], na.rm = TRUE)
          }
          
          # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
          refined_values$es[[t]][linear_indices] <- es_val
          
        }
      }
    }
  }
  
  
  # Calcula los máximos y mínimos para cada instante temporal en cada metodología
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
  
  
  n_colors <- 100
  plotly_colors <- mis.colores(n_colors)
  
  
  color_scale <- lapply(seq(min_z, max_z, length.out = n_colors), function(val) {
    scaled_val <- (val - min_z) / (max_z - min_z)
    list(scaled_val, plotly_colors[round(scaled_val * (n_colors - 1)) + 1])
  })
  
  
  

  output$metodologia_plot <- renderUI({
    if (input$visualizacion_medida == "3D") {
      plotlyOutput("metodologia_plotly", width = "100%", height = "100%")
    } else {
      plotOutput("metodologia_ggplot", width = "100%", height = "100%")
    }
  })
  
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
    
    # Renderiza el gráfico 2D
    filled.contour(x, y, values_matrix,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   main = sprintf("%s %.0f%%", input$medida, (1 - percentil_2) * 100),
                   xlim = c(0, ancho),
                   ylim = c(0, alto))
  })
  
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

    
    # Renderiza el gráfico 3D
    plot_ly(z = ~t(values_matrix)) %>% 
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
  })
  
}


### Funciones Auxiliares para Validación de datos ###

validar_parametro_ab_cer <- function(nombre, valor, min_valor, max_valor) {
  
  # Verifica que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verifica que esté en el rango correcto
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
  
  # Verifica que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verifica que esté en el rango correcto
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
  
  # Verifica que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verifica que esté en el rango correcto
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
  
  # Verifica que no esté vacío
  if (!isTruthy(valor)) {
    showModal(modalDialog(
      title = paste("Error en parámetro", nombre),
      paste("El valor de", nombre, "no puede estar vacío."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Verifica que esté en el rango correcto
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


