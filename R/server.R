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
  
  output$stateParameters <- renderUI({
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
      lapply(1:input$nbStrategies, function(x){
        tagList(
        h3(paste("Strategy", x)),
        withTags({
          table(class='stateVariables',
                tagList(
                  th(),
                  lapply(1:nbStates, function(i){
                    th(style='text-align:center', stateName[i])
                  }),
                  th(style='text-align:center', "Discounting Rate"),
                  lapply(1:nbStateVariables, function(i){
                    tr(td(variableStateName[i]),
                       lapply (1:nbStates, function (j) {
                         td(numericInput(paste0("stateVariable",i,j), value=0, label=NULL, width="100%"))
                       }),
                       td(numericInput(paste0("discountingRate",i), label=NULL, value=0, width="100%"))
                    )
                  })
                )
          )
        })
      )
      })
    }
  })
})

