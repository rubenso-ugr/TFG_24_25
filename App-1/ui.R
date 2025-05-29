library(shiny)
library(bslib)

# Defición de la UI --------------------------------------------------------------------

ui <- tagList(
  page_sidebar(
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
    actionButton("boton_simular", "Simular"),
    actionButton("boton_adjuntar", "Adjuntar")
  ),
  
  # UI del formulario dinámico
  mainPanel(
    conditionalPanel(
      condition = "input.modo == 'Simulación Condicionada'",
      uiOutput("formulario_dinamico"),
    )
  ),
  
  
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
    nav_panel("Conjunto de Excursión",
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
    nav_panel("Mapa de varianza",
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
  ),
  
  # Footer personalizado
  tags$footer(
    style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      background-color: #f8f9fa;
      padding: 10px;
      text-align: center;
      border-top: 1px solid #dee2e6;",
    
    # Enlaces
    HTML('
    &copy; 2025 Rubén Soriano Vidal | 
    <a href="documentacion.pdf" download style="color: #007bff;">Documentación</a>
  ')
  )
)
)