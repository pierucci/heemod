shinyServer(function(input, output) {
  value <- reactiveValues(nbNamedStates=0) 
  output$nameStates <- renderUI({
    counterState <- input$add - input$rem
    if (counterState < 0)
      counterState = 0
    if (counterState > 0){    
      lapply(1:counterState, function(i) {
        isolate({textInput(paste0("stateName", i), paste("State Name", i), value = ifelse(!is.null(input[[paste0("stateName",i)]]), input[[paste0("stateName",i)]], ""))})
      })
    }
  })
  
  output$transmatrix <- renderUI({
    counterState <- input$add - input$rem
    req(counterState)
    nbNamedStates <- 0
    for (i in 1:counterState)
      nbNamedStates <- ifelse(input[[paste0("stateName",i)]] != "", nbNamedStates + 1, nbNamedStates)
    
    req(nbNamedStates)
    if  (nbNamedStates > 0)
    {
      th <- ""
      tr <- ""
      h <- ""
      tab <- ""
      if (nbNamedStates > 0){
        h <- h3("Transition Matrix")
        tab <- "<table class='transmatrix'><th></th>"
        for (i in 1:nbNamedStates){
          th <- paste0(th, "<th style='text-align:center'>", input[[paste0("stateName",i)]], "</th>")
          tr <- paste0(tr, "<tr><td>",input[[paste0("stateName",i)]],"</td>")
          for (j in 1:nbNamedStates){
            tr <- paste0(tr, "<td>", textInput(paste0("transmatrix",i,j), value="", label=NULL, width="100%"), "</td>")
          }
          tr <- paste0(tr, "</tr>")
        }
        tab <- paste0(tab, th, tr, "</table>")
      }
      sortie <- tagList( h,
        HTML(tab)
      )
    }
    })
})