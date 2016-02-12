shinyUI(fluidPage(
  titlePanel("HEEMOD GUI"),
  tabsetPanel(
      tabPanel("States", h3("Define States"),
               actionButton("add","Add a State"),
               actionButton("rem","Remove Last State"),
               uiOutput("nameStates")
      ),
      tabPanel("Transition Matrix",    
               fluidRow(
                 column(12, uiOutput("transmatrix"))
               )
      ), 
      tabPanel("States Parameters",
               numericInput("nbStrategies", label = "Number of Strategies", value="", min = 0),
               uiOutput("nameStrategies")
      ), 
      tabPanel("Global Parameters",  "rien"
      ), 
      tabPanel("Output", tableOutput("out")
      )
    )
))