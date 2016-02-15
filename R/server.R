shinyServer(function(input, output) {
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
  
  output$transmatrix <- renderUI({
    nbStates = input$nbStates
    req(nbStates)
    stateName <- ""
    for (i in 1:nbStates){
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    if  (input$nbStates > 0)
    {
      withTags({
        table(class='transmatrix',
              tagList(
                th(),
                lapply(1:nbStates, function(i){
                  th(style='text-align:center', stateName[i])
                }),
                lapply(1:nbStates, function(i){
                  tr(td(stateName[i]),
                    lapply (1:nbStates, function (j) {
                      td(textInput(paste0("transmatrix",i,j), value="", label=NULL, width="100%"))
                    })
                  )
                })
              )
        )
      })
    }
    })
  
  output$stateParameters1 <- renderUI({
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
    if  (input$nbStates > 0)
    {
       tagList(
        h3(paste0("Strategy: \"", input[[paste0("strategyName",1)]], "\"")),
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
                                 isolate({tags$td(numericInput(paste0("stateVariable",1,i,j), value=0, label=NULL, width="100%"))})
                               }),
                               tags$td(numericInput(paste0("discountingRate",1,i), label=NULL, value=0, width="100%"))
                       )
                     })
                   )
        )
      )        
    }
  })
  output$stateParameters2 <- renderUI({
    req(input$copyValuesParameters)
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
        lapply(2:input$nbStrategies, function(x){
          tagList(
            h3(paste0("Strategy: \"", input[[paste0("strategyName",x)]], "\"")),
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
                                     isolate({tags$td(numericInput(paste0("stateVariable",x,i,j), value=ifelse(input[[paste0("stateVariable",1,i,j)]] != 0, input[[paste0("stateVariable",1,i,j)]],0), label=NULL, width="100%"))})
                                   }),
                                   isolate({tags$td(numericInput(paste0("discountingRate",x,i), label=NULL, value=ifelse(input[[paste0("discountingRate",1,i)]] != 0, input[[paste0("discountingRate",1,i)]],0), width="100%"))})
                           )
                         })
                       )
            )
          )
        })
    }
  })
})

