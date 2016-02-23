shinyUI(fluidPage(
  titlePanel("HEEMOD GUI"),
  tabsetPanel(
      tabPanel("States", h3("Define States"),
               fluidRow(
                 column(4, 
                        wellPanel(numericInput("nbStates", label = "Number of States", value="", min = 1))
                ),
                 column(4, 
                        wellPanel(numericInput("nbStateVariables",  label = "Number of State Variables", value="", min = 1))
                ),
                column(4, 
                       wellPanel(numericInput("nbStrategies",  label = "Number of Strategies", value="", min = 1))
                )
              ),
              fluidRow(
                column(4, 
                       uiOutput("nameStates")
                ),
                column(4, 
                       uiOutput("nameStateVariables")
                ),
                column(4, 
                       uiOutput("nameStrategies")
                )
              )

      ),
      tabPanel("Transition Matrix",    
               fluidRow(
                 column(12, uiOutput("transMatrix1"),
                        conditionalPanel(condition = "input.nbStrategies > 1", column(3, offset=3, actionButton("copyValuesParametersTM", "Copy values for other strategies")), fluidRow(hr())),
                        uiOutput("transMatrix2")
                 )
               )
      ), 
      tabPanel("States Parameters",
               uiOutput("stateParameters1"),
               conditionalPanel(condition = "input.nbStrategies > 1", column(3, offset=3, actionButton("copyValuesParametersSP", "Copy values for other strategies")), fluidRow(hr())),
               uiOutput("stateParameters2")
      ), 
      tabPanel("Global Parameters", 
               uiOutput("globalParameters"),
              conditionalPanel(condition = "input.nbStrategies > 1", column(3, offset=3, actionButton("copyValuesParametersGP", "Copy values for other strategies")), fluidRow(hr()))
               
      ), 
      tabPanel("Output",
               verbatimTextOutput("outModel")
               
               #downloadButton("downloadData", "Télécharger le tableau")
      )
    )
))


