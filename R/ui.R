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
                 column(12, uiOutput("transmatrix"))
               )
      ), 
      tabPanel("States Parameters",
               uiOutput("stateParameters")
      ), 
      tabPanel("Global Parameters",  "rien"
      ), 
      tabPanel("Output", tableOutput("out")
      )
    )
))


