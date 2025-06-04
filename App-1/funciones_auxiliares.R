mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

## Funciones Auxiliares para Validación de datos ##

# Función para validar que un valor numérico se encuentra en un intervalo abierto y cerrado elegido por el usuario
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

# Función para validar que un valor numérico se encuentra en un intervalo abierto elegido por el usuario
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

# Función para validar que un valor numérico se encuentra en un intervalo cerrado y abierto elegido por el usuario
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

# Función para validar que un valor numérico se encuentra en un intervalo cerrado elegido por el usuario
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

# Función para la validación de los datos ya simulados adjuntos. Se comprueba dimensiones y que todos los valores sean numéricos válidos.
validar_datos_simulados <- function(sim_values, modelo, ancho, alto, realizaciones) {
  
  filas_esperadas <- if (modelo == "Cauchy") {
    (ancho + 1) * (alto + 1)
  } else {
    (ancho + 1) * (alto + 1) * 4
  }
  
  columnas_esperadas <- realizaciones
  
  if (!all(dim(sim_values) == c(filas_esperadas, columnas_esperadas))) {
    showModal(modalDialog(
      title = "Error en la matriz",
      paste0(
        "La matriz debe tener ", filas_esperadas, " filas y ", columnas_esperadas, " columnas.\n",
        "Pero el archivo tiene ", nrow(sim_values), " filas y ", ncol(sim_values), " columnas."
      ),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  # Validar que todos los elementos sean numéricos y finitos
  if (!is.numeric(sim_values) || any(!is.finite(as.matrix(sim_values)))) {
    showModal(modalDialog(
      title = "Error en los datos",
      "La matriz contiene valores no numéricos o no finitos.",
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

# Función para la lectura, procesamiento y validación de archivos csv
leer_y_validar_archivo <- function(input_file, ancho_max, añadir_z = FALSE) {
  
  # Leer archivo sin mostrar tipos por consola
  datos <- readr::read_csv(input_file$datapath, show_col_types = FALSE)
  
  # Comprobar que tiene las columnas necesarias
  if (!all(c("x", "y", "valor") %in% names(datos))) {
    showModal(modalDialog(
      title = "Error en el archivo",
      "El archivo debe contener las columnas 'x', 'y' y 'valor'.",
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(NULL)
  }
  
  # Validar que todas las columnas sean numéricas y finitas
  if (!all(sapply(datos[c("x", "y", "valor")], is.numeric)) ||
      !all(is.finite(unlist(datos[c("x", "y", "valor")])))) {
    showModal(modalDialog(
      title = "Error en los datos",
      "Las columnas 'x', 'y' y 'valor' deben contener exclusivamente valores numéricos válidos.",
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(NULL)
  }

  # Verificar si hay al menos una fila válida
  if (nrow(datos) == 0) {
    showModal(modalDialog(
      title = "Error en los datos",
      "No hay filas válidas. Verifica que las columnas 'x', 'y' y 'valor' contengan al menos un conjunto de valores numéricos correctamente formateados.",
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(NULL)
  }
  
  # Verificar que x e y estén en el rango [0, ancho_max]
  if (any(datos$x < 0 | datos$x > ancho_max | datos$y < 0 | datos$y > ancho_max)) {
    showModal(modalDialog(
      title = "Error en las coordenadas",
      paste0("Las columnas 'x' e 'y' deben tener valores dentro del intervalo [0, ", ancho_max, "]."),
      footer = modalButton("Cerrar"),
      size = "l", easyClose = TRUE
    ))
    return(NULL)
  }
  
  # Reorganizar el dataframe para el modelo
  data <- data.frame(
    variable1 = datos$valor,
    coords.x1 = datos$y,
    coords.x2 = datos$x
  )
  
  # Si se requiere, añadir tercera coordenada z = 1
  if (añadir_z) {
    data$coords.x3 <- rep(1, nrow(data))
  }
  
  return(data)
}


## Funciones Auxiliares para la visualización ##

# Función para generar una escala de colores para la visualización 3D
generar_color_scale <- function(min_val, max_val, n_colors, colores) {
  lapply(seq(min_val, max_val, length.out = n_colors), function(val) {
    scaled_val <- (val - min_val) / (max_val - min_val)
    list(scaled_val, colores[round(scaled_val * (n_colors - 1)) + 1])
  })
}

# Función para la visualización en 2D
plot_2d <- function(mat, x, y, ancho, alto, zlim = NULL) {
  if (is.null(zlim)) {
    # Sin zlim especificado
    filled.contour(x, y, mat,
                   color.palette = mis.colores,
                   asp =1,
                   axes =TRUE,
                   frame.plot=0,
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

# Función para la visualización en 2D junto con los datos condicionados
plot_2d_cond <- function(mat, x, y, ancho, alto, condicionado,  zlim = NULL) {
  if (is.null(zlim)) {
    # Sin zlim especificado
    filled.contour(x, y, mat,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   xlim = c(0, ancho),
                   ylim = c(0, alto),
                   plot.axes = {
                     axis(1); axis(2)
                     points(condicionado$coords.x2, condicionado$coords.x1, 
                            pch = 10, 
                            col = "black")
                   }
    )
  } else {
    # Con zlim especificado
    filled.contour(x, y, mat,
                   color.palette = mis.colores,
                   asp = 1,
                   axes = TRUE,
                   frame.plot = 0,
                   xlim = c(0, ancho),
                   ylim = c(0, alto),
                   zlim = zlim,
                   plot.axes = {
                     axis(1); axis(2)
                     points(condicionado$coords.x2, condicionado$coords.x1, 
                            pch = 10, 
                            col = "black")
                   }
    )
  }
}

# Función para la visualización en 3D
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

# Función para la elección del tipo de salida de visualización según la elección del usuario (2D o 3D)
render_plot_selector <- function(input_value, plotly_id, plot_id) {
  if (input_value == "3D") {
    plotlyOutput(plotly_id, width = "100%", height = "100%")
  } else {
    plotOutput(plot_id, width = "100%", height = "100%")
  }
}

## Funciones Auxiliares para la metodología ##

# Función que contiene la metodología mediante ventanas deslizantes para la obtención de el mapa de riesgo
procesar_ventanas_deslizantes <- function(data_matrix, threshold, percentil_2, window_size,  step, refined_values, alto, ancho, tiempo = NULL) {
  
  # Determinar si es análisis temporal o estático
  is_temporal <- !is.null(tiempo)
  current_threshold <- if (is_temporal) threshold[[tiempo]] else threshold
  # Tamaño total de la ventana 
  tamano_ventana <- window_size^2
  
  #Se recorre la malla mediante ventanas deslizantes
  for (i in seq(1, ancho, by = step)) {
    for (j in seq(1, alto, by = step)) {
      
      # Ajuste de i para asegurar que la ventana completa entra en el ancho
      if ((i + window_size - 1) > ancho) {
        i_adj <- ancho - window_size + 1
      } else {
        i_adj <- i
      }
      
      # Ajuste de j para asegurar que la ventana completa entra en el alto
      if ((j + window_size - 1) > alto) {
        j_adj <- alto - window_size + 1
      } else {
        j_adj <- j
      }
      
      #Indices espaciales de la ventana
      indices_i <- i_adj:(i_adj + window_size - 1)
      indices_j <- j_adj:(j_adj + window_size - 1)
      
      #Creación de la cuadrícula completa de coordenadas
      grid <- expand.grid(i = indices_i, j = indices_j)
      
      #Conversión a indices lineales
      linear_indices <- grid$i + (grid$j - 1) * ancho
      
      # Extracción de los datos de la ventana (entre todas las realizaciones)
      window_data <- data_matrix[linear_indices, , drop = FALSE]
      
      # Vector con la fracción de valores que superan el umbral por realización
      fracciones_superan_umbral <- numeric(ncol(window_data))
      
      # Bucle por cada realización
      for (k in seq_len(ncol(window_data))) {
        valores_realizacion <- window_data[, k]
        #Área relativa
        fracciones_superan_umbral[k] <- sum(valores_realizacion >= current_threshold, na.rm = TRUE) / tamano_ventana
        
        #Área absoluta
        #fracciones_superan_umbral[k] <- sum(valores_realizacion >= current_threshold, na.rm = TRUE)
      }
        
      # Si todas las fracciones son NA o 0 (en el improbable caso de solo NA), usar el umbral como valor
      if (all(is.na(fracciones_superan_umbral)) || length(fracciones_superan_umbral) == 0) {
        var_hist <- var_param <- var_mc <- es_val <- current_threshold
      } else {
        
        # VaR Histórico sobre las fracciones
        var_hist <- quantile(fracciones_superan_umbral, probs = 1 - percentil_2, na.rm = TRUE)
        
        # VaR Paramétrico
        mu <- mean(fracciones_superan_umbral, na.rm = TRUE)
        sigma <- sd(fracciones_superan_umbral, na.rm = TRUE)
        z_alpha <- qnorm(1 - percentil_2)
        var_param <- mu + sigma * z_alpha
        
        # VaR Monte Carlo
        sim_mc <- rnorm(10000, mean = mu, sd = sigma)
        var_mc <- quantile(sim_mc, probs = 1 - percentil_2, na.rm = TRUE)
        
        # Expected Shortfall (sobre las fracciones mayores o iguales al VaR histórico)
        es_val <- mean(fracciones_superan_umbral[fracciones_superan_umbral >= var_hist], na.rm = TRUE)
      }
      
      # Asignación a las matrices correspondientes
      if (is_temporal) {
        refined_values$hist[[tiempo]][linear_indices]  <- var_hist
        refined_values$param[[tiempo]][linear_indices] <- var_param
        refined_values$mc[[tiempo]][linear_indices]    <- var_mc
        refined_values$es[[tiempo]][linear_indices]    <- es_val
      } else {
        refined_values$hist[linear_indices]  <- var_hist
        refined_values$param[linear_indices] <- var_param
        refined_values$mc[linear_indices]    <- var_mc
        refined_values$es[linear_indices]    <- es_val
      }
    }
  }
  
  return(refined_values)
}

# Función que ejecuta una simulación condicionada dependiendo de la elección del usuario y ejecuta la metodología completa
procesar_datos <- function(data, input, output) {
  x <- 0:(input$ancho)
  y <- 0:(input$alto)
  
  if (input$modelo == "Cauchy") {
    T <- 1:1
    giv <- cbind(rep(x, each = length(y)), rep(y, length(x)), rep(T, each = length(x) * length(y)))
    modelo <- RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale)
    sim <- RFsimulate(model = modelo, x = giv, data = data, n = input$realizaciones)
    sim_values <- matrix(unlist(sim@data), nrow = (input$ancho + 1) * (input$alto + 1), ncol = input$realizaciones)
    threshold <- modulo_simulacion(sim_values, x, y, input, output, data)
    modulo_metodologia(sim_values, x, y, input, output, threshold)
  } else {
    T <- 1:4
    giv <- cbind(rep(x, each = length(y)), rep(y, length(x)), rep(T, each = length(x) * length(y)))
    modelo <- RMnsst(
      phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale),
      psi = RMstable(alpha = input$alpha, var = input$var, scale = input$scale),
      delta = 2
    )
    sim <- RFsimulate(model = modelo, x = giv, data = data, n = input$realizaciones)
    sim_values <- matrix(unlist(sim@data), nrow = (input$ancho + 1) * (input$alto + 1) * 4, ncol = input$realizaciones)
    threshold_temporal <- modulo_simulacion_temporal(sim_values, x, y, input, output, data)
    modulo_metodologia_temporal(sim_values, x, y, input, output, threshold_temporal)
  }
  
  removeModal()
}