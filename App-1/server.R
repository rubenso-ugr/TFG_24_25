library(readr)
library(plotly)
library(magrittr)
library(RandomFieldsUtils)
library(RandomFields)

source("procesamiento_simulaciones.R")
source("funciones_auxiliares.R")

# Definición del server ----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1. Crear la variable reactiva para almacenar el modelo con el que se ejecutó la simulación
  modelo_ejecutado <- reactiveVal(NULL)
  
  ### Manejo de Modales y Formularios Dinámicos ###
  
  # Mostrar el modal para modo 'Simulación Condicionada'
  observeEvent(input$button, {
    
    # 2. Al pulsar el botón, guardar el modelo actual como el ejecutado
    modelo_ejecutado(input$modelo)
    
    # Validación de parámetros
    if (!validar_parametro_ab_cer("alpha", input$alpha, 0, 2)) return(NULL)
    if (!validar_parametro_ab_ab("beta", input$beta, 0)) return(NULL)
    if (!validar_parametro_ab_ab("var", input$var, 0)) return(NULL)
    if (!validar_parametro_ab_ab("scale", input$scale, 0)) return(NULL)
    if (!validar_parametro_ab_ab("primer percentil", input$percentil_1, 0, 1)) return(NULL)
    if (!validar_parametro_ab_ab("segundo percentil", input$percentil_2, 0, 1)) return(NULL)
    if (!validar_parametro_cer_ab("ventana", input$ventana, 2)) return(NULL)
    if (!validar_parametro_cer_ab("solapamiento", input$solapamiento, 1)) return(NULL)
    
    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
    
    if (input$modo == "Simulación Condicionada") {
      
      #Dependiendo del tipo de simulación se establecen unos máximos sobre los atributos ancho, alto y realizaciones
      if (input$modelo == "Cauchy"){
        if (!validar_parametro_cer_cer("ancho (píxeles)", input$ancho, 3, 100)) return(NULL)
        if (!validar_parametro_cer_cer("alto (píxeles)", input$alto, 3, 100)) return(NULL)
        if (!validar_parametro_cer_cer("realizaciones", input$realizaciones, 1, 50)) return(NULL)
      }else{
        if (!validar_parametro_cer_cer("ancho (píxeles)", input$ancho, 3, 50)) return(NULL)
        if (!validar_parametro_cer_cer("alto (píxeles)", input$alto, 3, 50)) return(NULL)
        if (!validar_parametro_cer_cer("realizaciones", input$realizaciones, 1, 20)) return(NULL)
      }
      
      #Se muestra una ventana para permitir al usuario introducir valores condicionados, ya sea manualmente o mediante un archivo .csv
      showModal(modalDialog(
        title = "Valores condicionales",
        fileInput("file_input", "Sube un archivo CSV", accept =".csv"),
        tagList(
          numericInput("n_filas_modal", "Número de valores:", value = 1, min = 1),
          uiOutput("formulario_dinamico")
        ),
        footer = tagList(
          modalButton("Cerrar"),
          actionButton("calcular_formulario", "Simular formulario"),
          actionButton("procesar_archivo", "Procesar archivo")
        ),
        size = "l", easyClose = TRUE
      ))
    } else if (input$modo == "Simulación") {      ### Lógica de simulación no condicionada###
      #Se realiza la simulación no condicionada y se lanzan las diferentes funciones de la metodología conlos datos simulados en función del modelo de simulación escogido
      
      #Se establecen unos máximos sobre los atributos ancho, alto y realizaciones para simulación no condicionada
      
      if (!validar_parametro_cer_ab("ancho (píxeles)", input$ancho, 3)) return(NULL)
      if (!validar_parametro_cer_ab("alto (píxeles)", input$alto, 3)) return(NULL)
      if (!validar_parametro_cer_ab("realizaciones", input$realizaciones, 1)) return(NULL)
      
      if (input$modelo == "Cauchy"){
        
        modelo <- RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale)
        sim <- RFsimulate(model = modelo, x = x, y = y, T = c(1, 1, 1), n = input$realizaciones)
        sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1), ncol = input$realizaciones)
        #write.csv(as.data.frame(sim_values), "cauchy.csv", row.names = FALSE)
        threshold <- modulo_simulacion(sim_values, x, y, input, output)
        modulo_metodologia(sim_values, x, y, input, output, threshold)
        
      } else {
        
        modelo <- RMnsst(
          phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale),
          psi = RMstable(alpha = input$alpha, var = input$var, scale = input$scale),
          delta = 2
        )
        
        sim <- RFsimulate(model = modelo, x = x, y = y, T = 1:4, n = input$realizaciones)
        sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1) *4, ncol = input$realizaciones)
        #write.csv(as.data.frame(sim_values), "gneiting.csv", row.names = FALSE)
        threshold_temporal <- modulo_simulacion_temporal(sim_values, x, y, input, output)
        
        modulo_metodologia_temporal(sim_values, x, y, input, output, threshold_temporal)
        
      }
    }
  })
  
  # Generación dinámica del formulario dentro del modal
  observe({
    req(input$n_filas_modal)
    output$formulario_dinamico <- renderUI({
      n <- input$n_filas_modal
      formularios <- lapply(1:n, function(i) {
        fluidRow(
          column(4, numericInput(paste0("fila_", i, "_val1"), "X", value = 0, min = 0, max = input$ancho)),
          column(4, numericInput(paste0("fila_", i, "_val2"), "Y", value = 0, min = 0, max = input$alto)),
          column(4, numericInput(paste0("fila_", i, "_val3"), "Valor", value = 0))
        )
      })
      do.call(tagList, formularios)
    })
  })
  
  # Procesamiento de los datos del formulario al hacer clic en 'Simular formulario'
  observeEvent(input$calcular_formulario, {
    req(input$n_filas_modal)
    n <- input$n_filas_modal
    data <- data.frame(
      variable1 = sapply(1:n, function(i) input[[paste0("fila_", i, "_val3")]]),
      coords.x1 = sapply(1:n, function(i) input[[paste0("fila_", i, "_val2")]]),
      coords.x2 = sapply(1:n, function(i) input[[paste0("fila_", i, "_val1")]]),
      coords.x3 = rep(1, n)
    )
    
    procesar_datos(data, input, output)
  })
  
  
  # procesamiento de los datos introducidos mediante archivo .csv al hacer clic en 'Procesar archivo'
  observeEvent(input$procesar_archivo, {
    
    req(input$file_input)
    
    # Leer el archivo CSVy validar parasimulación incondicionada
    data <- leer_y_validar_archivo(input$file_input, añadir_z = TRUE)
    if (is.null(data)) return(NULL)
    
    procesar_datos(data, input, output)
  })
  
  
  ### Consistencia de tamaños de ventana y solapamiento ###
  
  observe({
    
    # Solo se continúa si el valor es numérico y no está vacío
    if (!isTruthy(input$ventana)) return()
    if (!isTruthy(input$ancho)) return()
    if (!isTruthy(input$alto)) return()
    if (!isTruthy(input$solapamiento)) return()
    
    ## Comprobación dinámica del tamaño de la ventana ##
    
    # Límite superior para la ventana: mínimo entre alto y ancho
    max_ventana <- min(input$ancho, input$alto)
    
    # Se actualiza el input de la ventana
    updateNumericInput(session, "ventana", max = max_ventana)
    
    # Se ajusta el valor actual si supera el máximo
    if (input$ventana >= max_ventana) {
      updateNumericInput(session, "ventana", value = max_ventana)
    }
    
    ## Comprobación dinámica del solapamiento ##
    
    # Ajusta los límites del input de solapamiento
    updateNumericInput(session, "solapamiento", max = input$ventana, min = 1)
    
    # Se corrige los valores fuera de rango
    if (input$solapamiento >= input$ventana) {
      updateNumericInput(session, "solapamiento", value = input$ventana-1)
    } else if (input$solapamiento < 1) {
      updateNumericInput(session, "solapamiento", value = 1)
    }
  })
  
  
  
  
  # 3. Crear una salida reactiva para usar en conditionalPanel
  output$mostrar_temporal <- reactive({
    modelo_ejecutado() == "Gneiting"
  })
  
  # 4. Permitir que Shiny use esa salida en el UI dinámico
  outputOptions(output, "mostrar_temporal", suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$button_2, {
    modelo_ejecutado(input$modelo)
    showModal(
      modalDialog(
        title = "Adjuntar Datos",
        fileInput("archivo", "Selecciona un archivo:"),
        
        # Input adicional solo en modo 'Simulación Condicionada'
        conditionalPanel(
          condition = "input.modo == 'Simulación Condicionada'",
          fileInput("archivo_datos_condicionados", "Selecciona un archivo con datos condicionados:")
        ),
        
        # Botón para modo 'Simulación'
        conditionalPanel(
          condition = "input.modo == 'Simulación'",
          actionButton("confirmar_adjunto_simulacion", "Cargar (Simulación)")
        ),
        
        # Botón para modo 'Simulación Condicionada'
        conditionalPanel(
          condition = "input.modo == 'Simulación Condicionada'",
          actionButton("confirmar_adjunto_condicionada", "Cargar (Condicionada)")
        ),
        
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$confirmar_adjunto_simulacion, {
    
    req(input$archivo)
    
    # Leer el archivo tal cual, sin cabecera
    sim_values <- as.matrix(readr::read_csv(input$archivo$datapath, col_names = TRUE, show_col_types = FALSE))
    
    # Validación de dimensiones 
    
    if (!validar_dimensiones_matriz(sim_values, input$modelo, input$ancho, input$alto, input$realizaciones)) {
      return(NULL)
    }
    
    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
    if (input$modelo == "Cauchy"){
      threshold <- modulo_simulacion(sim_values, x, y, input, output)
      modulo_metodologia(sim_values, x, y, input, output, threshold)
    }else{
      threshold <- modulo_simulacion_temporal(sim_values, x, y, input, output)
      modulo_metodologia_temporal(sim_values, x, y, input, output, threshold)
    }
    
    removeModal()
  }
  )
  
  
  observeEvent(input$confirmar_adjunto_condicionada, {
    
    req(input$archivo)
    req(input$archivo_datos_condicionados)
    
    # Leer el archivo tal cual, sin cabecera
    sim_values <- as.matrix(readr::read_csv(input$archivo$datapath, col_names = TRUE, show_col_types = FALSE))
    
    # Leer el archivo CSV y validar para simulación condicionada
    data <- leer_y_validar_archivo(input$archivo_datos_condicionados)
    if (is.null(data)) return(NULL)
    
    
    # Validación de dimensiones 
    
    if (!validar_dimensiones_matriz(sim_values, input$modelo, input$ancho, input$alto, input$realizaciones)) {
      return(NULL)
    }
    
    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
    if (input$modelo == "Cauchy"){
      threshold <- modulo_simulacion(sim_values, x, y, input, output, data)
      modulo_metodologia(sim_values, x, y, input, output, threshold) 
    }else{
      threshold <- modulo_simulacion_temporal(sim_values, x, y, input, output, data)
      modulo_metodologia_temporal(sim_values, x, y, input, output, threshold) 
    }
    
    removeModal()
  })
  
}