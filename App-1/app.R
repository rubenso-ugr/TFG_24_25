# Load packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)

library(bslib)


library(RandomFieldsUtils)
library(RandomFields)

source("procesamiento_simulaciones.R")

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

rea_to_show <- 1 #Realización a mostrar

valores <- reactiveValues(form_confirmado = FALSE)


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
  
  observeEvent(input$button, {
    if (input$modo == "Formulario") {
      showModal(modalDialog(
        title = "Formulario dinámico",
        tagList(
          numericInput("n_filas_modal", "Número de filas:", value = 1, min = 1),
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
  
  # Generamos el formulario según el número de filas ingresado en el modal
  observe({
    req(input$n_filas_modal)
    output$formulario_dinamico <- renderUI({
      n <- input$n_filas_modal
      formularios <- lapply(1:n, function(i) {
        fluidRow(
          column(4, numericInput(paste0("fila_", i, "_val1"), paste("Fila", i, "- Valor 1"), value = 0)),
          column(4, numericInput(paste0("fila_", i, "_val2"), paste("Fila", i, "- Valor 2"), value = 0)),
          column(4, numericInput(paste0("fila_", i, "_val3"), paste("Fila", i, "- Valor 3"), value = 0))
        )
      })
      do.call(tagList, formularios)
    })
  })
  
  
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
    updateNumericInput(session, "solapamiento", max = input$ventana, min = 1)
    
    # Ajustamos el valor actual si está fuera del nuevo rango permitido
    nuevo_valor <- input$solapamiento
    if (input$solapamiento > input$ventana) {
      nuevo_valor <- input$ventana
    } else if (input$solapamiento < 1) {
      nuevo_valor <- 1
    }
    
    updateNumericInput(session, "solapamiento", value = nuevo_valor)
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
      
    if (input$modo == "Simulación") {  
        
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
        
        modulo_simulacion(sim_values, x, y, input, output, rea_to_show, mis.colores)
  }
    
  })
  
  
  observeEvent(input$calcular_formulario, {
    removeModal()
  })
  
  
  
  # Habilitamos el botón de descarga una vez haya un gráfico
  
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot1")
  })
  
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot2")
  })
  
  observeEvent(input$button, {
    shinyjs::enable("downloadPlot3")
  })
  
}

 
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)