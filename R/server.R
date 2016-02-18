
shinyServer(function(input, output, session) {
  valeurs <- reactiveValues(nbGlobalParameters = 1, addOne=TRUE)
  showStateParam <- function(nbStrat){
    nbStates = input$nbStates
    nbStateVariables = input$nbStateVariables
    req(nbStates)
    req(nbStateVariables)
    stateName <- ""
    variableStateName <- ""
    for (i in 1:nbStates)
      stateName[i] <- input[[paste0("stateName", i)]]
    for (i in 1:nbStateVariables)
      variableStateName[i] <- input[[paste0("variableStateName", i)]]
    if  (input$nbStates > 0) {
      start <- ifelse(nbStrat>1, 2, 1)
      lapply(start:nbStrat, function(x){
        tagList(
          h3(paste0("State Parameters for: \"", input[[paste0("strategyName",x)]], "\"")),
          tags$table(class='stateVariables',
                     tagList(
                       tags$th(),
                       lapply(1:nbStates, function(i){
                         tags$th(style='text-align:center', stateName[i])
                       }),
                       tags$th(style='text-align:center', "Discounting Rate"),
                       lapply(1:nbStateVariables, function(i){
                         tags$tr(tags$td(variableStateName[i]),
                                 lapply (1:nbStates, function (j) {
                                   isolate({tags$td(numericInput(paste0("stateVariable",x,i,j), value=ifelse(!is.null(input[[paste0("stateVariable",1,i,j)]]), input[[paste0("stateVariable",1,i,j)]],0), label=NULL, width="100%"))})
                                 }),
                                 isolate({tags$td(numericInput(paste0("discountingRate",x,i), label=NULL, value=ifelse(!is.null(input[[paste0("discountingRate",1,i)]]), input[[paste0("discountingRate",1,i)]],0), width="100%"))})
                         )
                       })
                     )
          )
        )
      })
    }
  }
  showTransMatrix <- function(nbStrat){
    nbStates = input$nbStates
    req(nbStates)
    stateName <- ""
    start <- ifelse(nbStrat>1, 2, 1)
    for (i in 1:nbStates){
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    if  (input$nbStates > 0)
    {
      tagList(
        lapply(start:nbStrat, function(x){
          tagList(
            h3(paste0("Transition Matrix for \"", input[[paste0("strategyName",x)]],"\"")),
            tags$table(class='transmatrix',
                       tagList(
                         tags$th(),
                         lapply(1:nbStates, function(i){
                           tags$th(style='text-align:center', stateName[i])
                         }),
                         lapply(1:nbStates, function(i){
                           tags$tr(tags$td(stateName[i]),
                                   lapply (1:nbStates, function (j) {
                                     isolate(tags$td(textInput(paste0("transmatrix",x,i,j), value=ifelse(!is.null(input[[paste0("transmatrix",1,i,j)]]), input[[paste0("transmatrix",1,i,j)]], ""), label=NULL, width="100%")))
                                   })
                           )
                         })
                       )
            )
          )
        })
      )
    }
  }
  
  output$nameStates <- renderUI({
    req(input$nbStates)
      lapply(1:input$nbStates, function(i) {
        isolate({textInput(paste0("stateName", i), paste("State Name", i), value = ifelse(!is.null(input[[paste0("stateName",i)]]), input[[paste0("stateName",i)]], paste("State",LETTERS[i])))})
      })
  })
  output$nameStateVariables <- renderUI({
    req(input$nbStateVariables)
    lapply(1:input$nbStateVariables, function(i) {
      isolate({textInput(paste0("variableStateName", i), paste("Variable Name", i), value = ifelse(!is.null(input[[paste0("variableStateName",i)]]), input[[paste0("variableStateName",i)]], paste("Variable",i)))})
    })
  })
  output$nameStrategies <- renderUI({
    req(input$nbStrategies)
    lapply(1:input$nbStrategies, function(i) {
      isolate({textInput(paste0("strategyName", i), paste("Strategy Name", i), value = ifelse(!is.null(input[[paste0("strategyName",i)]]), input[[paste0("strategyName",i)]], paste("Strategy",LETTERS[i])))})
    })
  })

  output$transMatrix1 <- renderUI({
    showTransMatrix(1)
    })
  output$transMatrix2 <- renderUI({
    req(input$copyValuesParametersTM)
    showTransMatrix(input$nbStrategies)
  })
  
  output$stateParameters1 <- renderUI({
    showStateParam(1)
  })
  output$stateParameters2 <- renderUI({
    req(input$copyValuesParametersSP)
    showStateParam(input$nbStrategies)
  })

  observe({
      n <- valeurs$nbGlobalParameters
      if (!is.null(input[[paste0("globalParamName",n)]]) & !is.null(input[[paste0("globalParamValue",n)]]))
        valeurs$addOne <- FALSE
  })
  
  observe({
    invalidateLater(1000, session)
    n <- valeurs$nbGlobalParameters
    if (isolate(valeurs$addOne)){
      valeurs$paramName <- isolate(input[[paste0("globalParamName",n)]])
      valeurs$paramValue <- isolate(input[[paste0("globalParamValue",n)]])
    } else
      isolate (valeurs$addOne <- TRUE)

  })

  
  output$globalParameters <- renderUI({
    n <- valeurs$nbGlobalParameters
    a <- tags$table(
        lapply(1:n, function(i){
          tags$tr(
              isolate(tags$td(textInput(paste0("globalParamName",i), label = NULL, value = input[[paste0("globalParamName",i)]], width="100%"))),
              isolate(tags$td(numericInput(paste0("globalParamValue",i), label = NULL, value = input[[paste0("globalParamValue",i)]], width="100%")))
            )
        })
    )
    tagAppendChildren(a,
        if (!is.null(valeurs$paramName) && valeurs$paramName != "" | !is.null(valeurs$paramValue) && !is.na(valeurs$paramValue)){
          tags$tr(
            tags$td(textInput(paste0("globalParamName",n+1), label = NULL, value = "", width="100%")),
            tags$td(numericInput(paste0("globalParamValue",n+1), label = NULL, value = NA, width="100%"))
          )
          valeurs$nbGlobalParameters <- n+1
        }
        
    )

  })
})

