# Carga de librerías necesarias ----------------------------------------------------------------


library(shiny)
library(bslib)
library(RandomFieldsUtils)
library(RandomFields)
library(readr)
library(plotly)
library(magrittr)

source("procesamiento_simulaciones.R")

options(shiny.useragg = FALSE) #Para que no se muestre rejilla
set.seed(123)

# Defición de la UI --------------------------------------------------------------------

ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    title = "TFG Estadística espacial",
    
    #Panel izquierdo con los diferentes inputs y opciones de atributos
    sidebar = sidebar(
      numericInput("ancho", "Ancho (pixeles)", value =50, min = 3, max = 1000, step = 1),
      numericInput("alto", "Alto (pixeles)", value = 50, min = 3, max = 1000, step = 1),
      numericInput("alpha", "Alpha", value = 2, min = 1e-20, max = 2, step = 0.01),
      numericInput("beta", "Beta", value = 0.1, min = 1e-20, step = 0.01),
      numericInput("var", "Varianza", value = 0.1, min = 1e-20, step = 0.01),
      numericInput("scale", "Escala espacial", value = 1, min = 1e-20, step = 0.01),
      numericInput("percentil_1", "Umbral de riesgo", value = 0.15, min = 1e-20, max = 1,  0.01),
      numericInput("percentil_2", "Nivel de confianza", value = 0.05, min = 1e-20, max = 1,  0.01),
      numericInput("realizaciones", "Realizaciones", value = 20, min = 1, max = 500, step = 1),
      selectInput("modelo", "Elige modelo", choices = c( "Gneiting", "Cauchy")),
      numericInput("ventana", "Tamaño ventana", value = 3, min = 2, step = 1),
      numericInput("solapamiento", "Solapamiento", value = 1, min = 1, step = 1),
      selectInput("modo", "Selecciona el modo:", choices = c("Simulación", "Simulación Condicionada")),
      actionButton("button", "Calcular"),
      actionButton("button_2", "Adjuntar")
    ),
    
    # UI del formulario dinámico
    mainPanel(
      conditionalPanel(
        condition = "input.modo == 'Simulación Condicionada'",
        uiOutput("formulario_dinamico"),
      )),

    
    # Panel derecho con pestañas
    navset_card_underline(
      nav_panel("Primera simulación",
                fluidRow(
                  column(6, selectInput("visualizacion_primera_simulacion", "Visualización", 
                                        choices = c("2D", "3D"))),
                  column(6, conditionalPanel(
                    condition = "output.mostrar_temporal",
                    selectInput("visualizacion_primera_temporal", "Instante temporal", 
                                choices = c("T1", "T2", "T3", "T4"))
                  ))
                ),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("primera_simulacion"),
                ),
      ),
      nav_panel("Conjunto Excursión",
                fluidRow(
                  column(6, selectInput("visualizacion_excursion", "Visualización", 
                                        choices = c("2D", "3D"))),
                  column(6, conditionalPanel(
                    condition = "output.mostrar_temporal",
                    selectInput("visualizacion_excursion_temporal", "Instante temporal", 
                                choices = c("T1", "T2", "T3", "T4"))
                  ))
                ),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("conjunto_excursion",  width = "100%", height = "100%"),
                )),
      nav_panel("Mapa de varianzas",
                fluidRow(
                  column(6, selectInput("visualizacion_varianza", "Visualización", 
                                        choices = c("2D", "3D"))),
                  column(6, conditionalPanel(
                    condition = "output.mostrar_temporal",
                    selectInput("visualizacion_varianza_temporal", "Instante temporal", 
                                choices = c("T1", "T2", "T3", "T4"))
                  ))
                ),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("mapa_varianzas",  width = "100%", height = "100%")
                )),
      nav_panel("Mapas de riesgo",
                fluidRow(
                  column(4,
                         selectInput("medida", "Medida de riesgo a usar", 
                                     choices = c("VaR Histórico", "VaR Paramétrico", "VaR Montecarlo", "ES"))
                  ),
                  column(4,
                         selectInput("visualizacion_medida", "Visualización", 
                                     choices = c("2D", "3D"))
                  ),
                  column(4, conditionalPanel(
                    condition = "output.mostrar_temporal",
                    selectInput("visualizacion_medida_temporal", "Instante temporal", 
                                choices = c("T1", "T2", "T3", "T4"))
                  ))
                ),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("metodologia_plot")
                )),
      nav_panel("Resumen", 
                verbatimTextOutput("mensaje_resumen")
                )
    )
)


# Definición del server ----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1. Crear la variable reactiva para almacenar el modelo con el que se ejecutó la simulación
  modelo_ejecutado <- reactiveVal(NULL)
  
  ### Manejo de Modales y Formularios Dinámicos ###
  
  # Mostrar el modal para modo 'Simulación Condicionada'
  observeEvent(input$button, {
    
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
      
      #Comprobación de que los valores introducidos en los atributos se encuentran en los límites correctos
      if (!validar_parametro_ab_cer("alpha", input$alpha, 0, 2)) return(NULL)
      if (!validar_parametro_ab_ab("beta", input$beta, 0)) return(NULL)
      if (!validar_parametro_ab_ab("primer percentil", input$percentil_1, 0, 1)) return(NULL)
      if (!validar_parametro_ab_ab("segundo percentil", input$percentil_2, 0, 1)) return(NULL)
      if (!validar_parametro_cer_ab("ventana", input$ventana, 2)) return(NULL)
      if (!validar_parametro_cer_ab("solapamiento", input$solapamiento, 1)) return(NULL)
      
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
    
    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
   #Se realiza la simulación condicionada y se lanzan las diferentes funciones de la metodología con
   #los datos simulados en función del modelo de simulación escogido
    if (input$modelo == "Cauchy"){
      T<-1:1
      giv<-cbind(rep(x,each=length(y)), rep(y,length(x)),  
                 rep(T,each=length(x)*length(y)))
      modelo <- RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale)
      sim <- RFsimulate(model = modelo,x=giv,data=data, n = input$realizaciones)
      sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1), ncol = input$realizaciones)
      threshold <- modulo_simulacion(sim_values, x, y, input, output, data)
      modulo_metodologia(sim_values, x, y, input, output, threshold)
    } else {
      T<-1:4
      # Processing grid for RFsimulate function
      giv<-cbind(rep(x,each=length(y)), rep(y,length(x)),  
                 rep(T,each=length(x)*length(y)))
      modelo <- RMnsst(
        phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale),
        psi = RMstable(alpha = input$alpha, var = input$var, scale = input$scale),
        delta = 2
      )
      sim <- RFsimulate(model = modelo, x=giv,data=data, n = input$realizaciones)
      sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1) *4, ncol = input$realizaciones)
      threshold_temporal <- modulo_simulacion_temporal(sim_values, x, y, input, output, data)
      modulo_metodologia_temporal(sim_values, x, y, input, output, threshold_temporal)
    }

    # Cerrar la ventana emergente después de simular
    removeModal()
  })
  
  
  # procesamiento de los datos introducidos mediante archivo .csv al hacer clic en 'Procesar archivo'
  observeEvent(input$procesar_archivo, {
    
    req(input$file_input)
    
    # Leer el archivo CSV
    archivo <- input$file_input$datapath
    datos <- read_csv(archivo)
    
    # Verificación de columnas esperadas
    if (!all(c("x", "y", "valor") %in% names(datos))) {
      showModal(modalDialog(
        title = "Error en el archivo",
        "El archivo debe contener las columnas 'x', 'y' y 'valor'.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Formato del data frame esperado
    data <- data.frame(
      variable1 = datos$valor,
      coords.x1 = datos$y,
      coords.x2 = datos$x,
      coords.x3 = rep(1, nrow(datos))
    )
    
    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
  
    #Se realiza la simulación condicionada y se lanzan las diferentes funciones de la metodología con 
    #los datos simulados en función del modelo de simulación escogido
    if (input$modelo == "Cauchy"){
      T<-1:1
      giv<-cbind(rep(x,each=length(y)), rep(y,length(x)),  
                 rep(T,each=length(x)*length(y)))
      modelo <- RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale)
      sim <- RFsimulate(model = modelo, x=giv,data=data, n = input$realizaciones)
      sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1), ncol = input$realizaciones)
      threshold <- modulo_simulacion(sim_values, x, y, input, output, data)
      modulo_metodologia(sim_values, x, y, input, output, threshold)
    } else {
      T<-1:4
      giv<-cbind(rep(x,each=length(y)), rep(y,length(x)),  
                 rep(T,each=length(x)*length(y)))
      modelo <- RMnsst(
        phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = input$var, scale = input$scale),
        psi = RMstable(alpha = input$alpha, var = input$var, scale = input$scale),
        delta = 2
      )
      sim <- RFsimulate(model = modelo, x=giv,data=data, n = input$realizaciones)
      sim_values <- matrix(unlist(sim@data), nrow = (input$ancho+1) * (input$alto+1) *4, ncol = input$realizaciones)
      threshold_temporal <- modulo_simulacion_temporal(sim_values, x, y, input, output, data)
      
      modulo_metodologia_temporal(sim_values, x, y, input, output, threshold_temporal)
    }
    
    # Cerrar la ventana emergente después de simular
    removeModal()
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
  

  ### Lógica de simulación no condicionada###
  
  observeEvent(input$button, {
    
    # 2. Al pulsar el botón, guardar el modelo actual como el ejecutado
    modelo_ejecutado(input$modelo)
  
    # Validación de parámetros
    if (!validar_parametro_ab_cer("alpha", input$alpha, 0, 2)) return(NULL)
    if (!validar_parametro_ab_ab("beta", input$beta, 0)) return(NULL)
    if (!validar_parametro_ab_ab("primer percentil", input$percentil_1, 0, 1)) return(NULL)
    if (!validar_parametro_ab_ab("segundo percentil", input$percentil_2, 0, 1)) return(NULL)
    if (!validar_parametro_cer_ab("ancho (píxeles)", input$ancho, 3)) return(NULL)
    if (!validar_parametro_cer_ab("alto (píxeles)", input$alto, 3)) return(NULL)
    if (!validar_parametro_cer_ab("realizaciones", input$realizaciones, 1)) return(NULL)
    if (!validar_parametro_cer_ab("ventana", input$ventana, 2)) return(NULL)
    if (!validar_parametro_cer_ab("solapamiento", input$solapamiento, 1)) return(NULL)

    #Se establece el grid de trabajo
    x <- 0:(input$ancho)
    y <- 0:(input$alto)
    
    #Se realiza la simulación no condicionada y se lanzan las diferentes funciones de la metodología con 
    #los datos simulados en función del modelo de simulación escogido
    if (input$modo == "Simulación") {  
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
    
    if (input$modelo == "Cauchy"){
      filas_esperadas <- (input$ancho + 1) * (input$alto + 1)
    }else{
      filas_esperadas <- (input$ancho + 1) * (input$alto + 1) * 4
    }
    columnas_esperadas <- input$realizaciones
    
    if (!all(dim(sim_values) == c(filas_esperadas, columnas_esperadas))) {
      showModal(modalDialog(
        title = "Error en la matriz",
        paste0("La matriz debe tener ", filas_esperadas, " filas y ", columnas_esperadas, " columnas.\n",
               "Pero el archivo tiene ", nrow(sim_values), " filas y ", ncol(sim_values), " columnas."),
        easyClose = TRUE,
        footer = NULL
      ))
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
    
    # Leer el archivo CSV
    datos <- read_csv(input$archivo_datos_condicionados$datapath)
    
    # Verificación de columnas esperadas
    if (!all(c("x", "y", "valor") %in% names(datos))) {
      showModal(modalDialog(
        title = "Error en el archivo",
        "El archivo debe contener las columnas 'x', 'y' y 'valor'.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Formato del data frame esperado
    data <- data.frame(
      variable1 = datos$valor,
      coords.x1 = datos$y,
      coords.x2 = datos$x,
    )
    
    
    
    # Validación de dimensiones 
    
    if (input$modelo == "Cauchy"){
      filas_esperadas <- (input$ancho + 1) * (input$alto + 1)
    }else{
      filas_esperadas <- (input$ancho + 1) * (input$alto + 1) * 4
    }
    columnas_esperadas <- input$realizaciones
    
    if (!all(dim(sim_values) == c(filas_esperadas, columnas_esperadas))) {
      showModal(modalDialog(
        title = "Error en la matriz",
        paste0("La matriz debe tener ", filas_esperadas, " filas y ", columnas_esperadas, " columnas.\n",
               "Pero el archivo tiene ", nrow(sim_values), " filas y ", ncol(sim_values), " columnas."),
        easyClose = TRUE,
        footer = NULL
      ))
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

 
# Create the Shiny app object ---------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)