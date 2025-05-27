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

validar_dimensiones_matriz <- function(sim_values, modelo, ancho, alto, realizaciones) {
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
      easyClose = TRUE,
      footer = NULL
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

leer_y_validar_archivo <- function(input_file, añadir_z = FALSE) {
  datos <- readr::read_csv(input_file$datapath)
  
  if (!all(c("x", "y", "valor") %in% names(datos))) {
    showModal(modalDialog(
      title = "Error en el archivo",
      "El archivo debe contener las columnas 'x', 'y' y 'valor'.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  
  data <- data.frame(
    variable1 = datos$valor,
    coords.x1 = datos$y,
    coords.x2 = datos$x
  )
  
  if (añadir_z) {
    data$coords.x3 <- rep(1, nrow(datos))
  }
  
  return(data)
}