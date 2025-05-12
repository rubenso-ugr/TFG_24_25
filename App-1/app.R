# Load packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)
library(bslib)
library(RandomFieldsUtils)
library(RandomFields)


library(plotly)
library(magrittr)

source("procesamiento_simulaciones.R")

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    title = "TFG Estadística espacial",
    sidebar = sidebar(
      useShinyjs(), #para que no se pueda pulsar el boton de descarga antes de tiempo
      numericInput("ancho", "Ancho (pixeles)", value = 100, min = 3, max = 5000),
      numericInput("alto", "Alto (pixeles)", value = 100, min = 3, max = 5000),
      numericInput("alpha", "Alpha", value = 2, min = 1e-20, max = 2),
      numericInput("beta", "Beta", value = 0.1, min = 1e-20),
      numericInput("percentil_1", "Primer percentil", value = 0.15, min = 1e-20, max = 1),
      numericInput("percentil_2", "Segundo percentil", value = 0.05, min = 1e-20, max = 1),
      numericInput("realizaciones", "Realizaciones", value = 50, min = 1, max = 1000),
      selectInput("modelo", "Elige modelo", choices = c("Cauchy", "Gneiting")),
      numericInput("ventana", "Tamaño ventana", value = 5, min = 2),
      numericInput("solapamiento", "Solapamiento", value = 1, min = 1),
      selectInput("modo", "Selecciona el modo:", choices = c("Simulación", "Formulario")),
      actionButton("button", "Calcular"),
    ),
    
    # UI del formulario dinámico
    mainPanel(
      conditionalPanel(
        condition = "input.modo == 'Formulario'",
        uiOutput("formulario_dinamico"),
      )),
    
    # Panel derecho con pestañas
    navset_card_underline(
      nav_panel("Primera simulación",
                selectInput("visualizacion_primera_simulacion", "Visualización", 
                            choices = c("2D", "3D")),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("primera_simulacion"), 
                ),
                downloadButton("downloadPlot1", "Descargar imagen PNG", disabled = TRUE)
      ),
      nav_panel("Conjunto Excursión",
                plotOutput("conjunto_excursion",  width = "100%", height = "100%"),
                downloadButton("downloadPlot2", "Descargar imagen PNG", disabled = TRUE)
                ),
      nav_panel("Mapa de varianzas",
                plotOutput("mapa_varianzas",  width = "100%", height = "100%")
                , downloadButton("downloadPlot3", "Descargar imagen PNG", disabled = TRUE)
                ),
      nav_panel("Metodología aplicada",
                fluidRow(
                  column(6,
                         selectInput("medida", "Medida de riesgo a usar", 
                                     choices = c("VaR Histórico", "VaR Paramétrico", "VaR Montecarlo", "ES"))
                  ),
                  column(6,
                         selectInput("visualizacion_medida", "Visualización", 
                                     choices = c("2D", "3D"))
                  )
                ),
                div(
                  style = " height: 100%; overflow-y: auto;",
                  uiOutput("metodologia_plot")
                ),
                downloadButton("downloadPlot3", "Descargar imagen PNG", disabled = TRUE)
      ),
      nav_panel("Resumen", 
                verbatimTextOutput("mensaje_resumen")
                )
    )
    
    
)


# Define server ----------------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Manejo de Modales y Formularios Dinámicos ###
  
  # Muestra el modal para modo 'Formulario'
  observeEvent(input$button, {
    if (input$modo == "Formulario") {
      showModal(modalDialog(
        title = "Valores condicionales",
        tagList(
          numericInput("n_filas_modal", "Número de valores:", value = 1, min = 1),
          uiOutput("formulario_dinamico")
        ),
        footer = tagList(
          modalButton("Cerrar"),
          actionButton("calcular_formulario", "Simular")
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
          column(4, numericInput(paste0("fila_", i, "_val1"), "X", value = 0, min = 0, max = input$ancho - 1)),
          column(4, numericInput(paste0("fila_", i, "_val2"), "Y", value = 0, min = 0, max = input$alto - 1)),
          column(4, numericInput(paste0("fila_", i, "_val3"), "Valor", value = 0))
        )
      })
      do.call(tagList, formularios)
    })
  })
  
  # Procesa los datos del formulario al hacer clic en 'Simular'
  data1 <- eventReactive(input$calcular_formulario, {
    req(input$n_filas_modal)
    n <- input$n_filas_modal
    data.frame(
      x = sapply(1:n, function(i) input[[paste0("fila_", i, "_val1")]]),
      y = sapply(1:n, function(i) input[[paste0("fila_", i, "_val2")]]),
      value = sapply(1:n, function(i) input[[paste0("fila_", i, "_val3")]])
    )
  })
  
  # Cierra el modal después de simular
  observeEvent(input$calcular_formulario, {
    removeModal()
  })
  
  
  ### Consistencia de tamaños de ventana y solapamiento ###
  
  observe({
    
    # Solo continúa si el valor es numérico y no está vacío
    if (!isTruthy(input$ventana)) return()
    if (!isTruthy(input$ancho)) return()
    if (!isTruthy(input$alto)) return()
    if (!isTruthy(input$solapamiento)) return()
    
    #Comprobación tamaño de la ventana
    
    # Límite superior para la ventana: mínimo entre alto y ancho
    max_ventana <- min(input$ancho, input$alto)
    
    # Actualiza el input de la ventana
    updateNumericInput(session, "ventana", max = max_ventana)
    
    # Ajusta el valor actual si supera el máximo
    if (input$ventana >= max_ventana) {
      updateNumericInput(session, "ventana", value = max_ventana)
    }

    # Comprobación del solapamiento
    
    # Ajusta los límites del input de solapamiento
    updateNumericInput(session, "solapamiento", max = input$ventana, min = 1)
    
    # Corrige valores fuera de rango
    if (input$solapamiento >= input$ventana) {
      updateNumericInput(session, "solapamiento", value = input$ventana-1)
    } else if (input$solapamiento < 1) {
      updateNumericInput(session, "solapamiento", value = 1)
    }
  })
  

  ### Lógica de simulación ###
  
  observeEvent(input$button, {
    
    # Validación de parámetros #

    if (!validar_parametro_ab_cer("alpha", input$alpha, 0, 2)) return(NULL)
    if (!validar_parametro_ab_ab("beta", input$beta, 0)) return(NULL)
    if (!validar_parametro_ab_ab("primer percentil", input$percentil_1, 0, 1)) return(NULL)
    if (!validar_parametro_ab_ab("segundo percentil", input$percentil_2, 0, 1)) return(NULL)
    if (!validar_parametro_cer_ab("ancho (píxeles)", input$ancho, 3)) return(NULL)
    if (!validar_parametro_cer_ab("alto (píxeles)", input$alto, 3)) return(NULL)
    if (!validar_parametro_cer_ab("realizaciones", input$realizaciones, 1)) return(NULL)
    if (!validar_parametro_cer_ab("ventana", input$ventana, 2)) return(NULL)
    if (!validar_parametro_cer_ab("solapamiento", input$solapamiento, 1)) return(NULL)

    
    if (input$modo == "Simulación") {  
      modelo <- switch(input$modelo,
                       "Cauchy" = RMgencauchy(alpha = input$alpha, beta = input$beta, var = 0.1, scale = 1),
                       "Gneiting" = RMnsst(
                         phi = RMgencauchy(alpha = input$alpha, beta = input$beta, var = 0.1, scale = 1),
                         psi = RMstable(alpha = input$alpha, var = 0.1, scale = 1),
                         delta = 2
                       ))
      set.seed(123)
      x <- 0:(input$ancho - 1)
      y <- 0:(input$alto - 1)
      sim <- RFsimulate(model = modelo, x = x, y = y, T = c(1, 1, 1), n = input$realizaciones)
      sim_values <- matrix(unlist(sim@data), nrow = input$ancho * input$alto, ncol = input$realizaciones)
      threshold <- modulo_simulacion(sim_values, x, y, input, output)
      modulo_metodologia(sim_values, x, y, input, output, threshold)
    }
  })
  
  ### Habilitación de botones de descarga ###
  
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot1")
    shinyjs::enable("downloadPlot2")
    shinyjs::enable("downloadPlot3")
  })

  
}

 
# Create the Shiny app object ---------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)