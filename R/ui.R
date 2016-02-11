shinyUI(fluidPage(
  titlePanel("HEEMOD GUI"),
  tabsetPanel(
      tabPanel("States",    
               h3("Define States"),
               actionButton("add","Add a State"),
               actionButton("rem","Remove Last State"),
               uiOutput("nameStates"),
               fluidRow(
                 column(12, uiOutput("transmatrix"))
               )
      ), 
      tabPanel("Parameters",  "rien"
      ), 
      tabPanel("Output", tableOutput("out")
      )
    )
))