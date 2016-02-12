shinyServer(function(input, output) {
  value <- reactiveValues(nbNamedStates=0) 
  
  output$nameStates <- renderUI({
    req(input$nbStates)
    counter <- input$nbStates
      lapply(1:counter, function(i) {
        isolate({textInput(paste0("stateName", i), paste("State Name", i), value = ifelse(!is.null(input[[paste0("stateName",i)]]), input[[paste0("stateName",i)]], ""))})
      })
  })
  output$nameStateVariables <- renderUI({
    req(input$nbStateVariables)
    lapply(1:input$nbStateVariables, function(i) {
      isolate({textInput(paste0("variableStateName", i), paste("Variable Name", i), value = ifelse(!is.null(input[[paste0("variableStateName",i)]]), input[[paste0("variableStateName",i)]], ""))})
    })
  })
  output$valueStateVariables <- renderUI({
    req(input$nbStateVariables)
    lapply(1:input$nbStateVariables, function(i) {
      isolate({numericInput(paste0("variableStateValue", i), paste("Variable Value", i), value = 0)})
    })
  })
  
  output$transmatrix <- renderUI({
    req(input$nbStates)
    nbNamedStates <- 0
    stateName <- ""
    for (i in 1:input$nbStates){
      nbNamedStates <- ifelse(input[[paste0("stateName",i)]] != "", nbNamedStates + 1, nbNamedStates)
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    req(nbNamedStates)
    if  (nbNamedStates > 0)
    {
      withTags({
        table(class='transmatrix',
              tagList(
                th(),
                lapply(1:nbNamedStates, function(i){
                  th(style='text-align:center', stateName[i])
                }),
                lapply(1:nbNamedStates, function(i){
                  tr(td(stateName[i]),
                    lapply (1:nbNamedStates, function (j) {
                      td(textInput(paste0("transmatrix",i,j), value="", label=NULL, width="100%"))
                    })
                  )
                })
              )
        )
      })
    }
    })
})

