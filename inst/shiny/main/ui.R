shinyUI(
  fluidPage(
    titlePanel("HEEMOD GUI"),
    checkboxInput(
      "checkShowHelp",
      "Show help",
      value = TRUE
    ),
    tabsetPanel(
      
      tabPanel(
        "States",
        h3("Define States"),
        fluidRow(
          column(
            4, 
            wellPanel(
              numericInput(
                "nbStates",
                label = "Number of States",
                value = "",
                min = 1
              )
            )
          ),
          column(
            4, 
            wellPanel(
              numericInput(
                "nbStateVariables",
                label = "Number of State Variables",
                value = "",
                min = 1
              )
            )
          ),
          column(
            4, 
            wellPanel(
              numericInput(
                "nbStrategies",
                label = "Number of Strategies",
                value="",
                min = 1
              )
            )
          )
        ),
        fluidRow(
          column(
            4, 
            uiOutput("nameStates")
          ),
          column(
            4, 
            uiOutput("nameStateVariables")
          ),
          column(
            4, 
            uiOutput("nameStrategies")
          )
        ),
        conditionalPanel(
          condition = "input.checkShowHelp == 1",
          fluidRow(
            column(
              4,
              em("Number of distinct states in the model, and their names.")
            ),
            column(
              4,
              em("Number of values attached to states (such as cost, utility...), and their names.
               Names should not contain spaces, start with '.', or special characters.")
            ),
            column(
              4,
              em("Number of strategies to compare.")
            )
          )
        )
        
      ),
      
      tabPanel(
        "Global Parameters", 
        uiOutput("globalParameters"),
        conditionalPanel(
          condition = "input.nbStrategies > 1",
          fluidRow(
            column(
              3,
              offset=3,
              actionButton(
                "copyValuesParametersGP",
                "Copy values for other strategies"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.checkShowHelp == 1",
          fluidRow(
            column(
              4,
              em("Optional parameters to be called in transition matrix or state values. 
               The variable "),
              strong("markov_cycle"),
              em(" is defined inside the model and takes values 0, 1, 2... n at each cycle.
               It can thus be used to define time-varying properties (such as age = 50 + markov_cycle).")
            )
          )
        )
      ), 
      
      tabPanel(
        "Transition Matrix",    
        fluidRow(
          column(
            12,
            uiOutput("transMatrix1"),
            conditionalPanel(
              condition = "input.nbStrategies > 1",
              column(
                3,
                offset=3,
                actionButton(
                  "copyValuesParametersTM",
                  "Copy values for other strategies"
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput("transMatrix2")
          )
        ),
        conditionalPanel(
          condition = "input.checkShowHelp == 1",
          fluidRow(
            column(
              4,
              em("Matrix of transition probabilities between states.
               References can be made to parameters computed in the previous tab.
               The sum of probabilities per row must equal 1. The alias "),
              strong("C"),
              em(" (meaning probability complement) means 1 minus the row sum of other probabilities.")
            )
          )
        )
      ), 
      
      tabPanel(
        "States Parameters",
        fluidRow(
          column(
            12,
            uiOutput("stateParameters1"),
            conditionalPanel(
              condition = "input.nbStrategies > 1",
              column(
                3,
                offset=3,
                actionButton(
                  "copyValuesParametersSP",
                  "Copy values for other strategies"
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput("stateParameters2")
          )
        ),
        conditionalPanel(
          condition = "input.checkShowHelp == 1",
          fluidRow(
            column(
              4,
              em("Values associated with each state for each strategy.")
              )
            )
        ),
        conditionalPanel(
          condition = "input.nbStates > 0 & input.nbStateVariables > 1",
          fluidRow(
            column(
              6,
              uiOutput(
                "costVariable"
              )
            ),
            column(
              6,
              uiOutput(
                "effectVariable"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.checkShowHelp == 1 & input.nbStates > 0 & input.nbStateVariables > 1",
          fluidRow(
            column(
              6,
              em("State variable name representing "),
              strong("cost"),
              em(" in the model (or mathematical expression using variable names, such as (var1+var2)/2).")
            ),
            column(
              6,
              em("State variable name representing "),
              strong("effect"),
              em(" in the model (or mathematical expression using variable names, such as (var1+var2)/2).")
              )
          )
        )
      ),
      tabPanel(
        "Results",
        uiOutput("outModel"),
        DT::dataTableOutput("tableResults"),
        uiOutput("titleICER"),
        DT::dataTableOutput("tableICER")
      ),
      tabPanel(
        "Plots",
        uiOutput("outCounts"),
        plotOutput("plotCounts")
      )
    )
  )
)


