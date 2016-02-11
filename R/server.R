shinyServer(function(input, output) {
  value <- reactiveValues(nbNamedStates=0) 
  output$nameStates <- renderUI({
    counterState <- input$add - input$rem
    if (counterState < 0)
      counterState = 0
    w <- NULL
    nbNamedState = 0
    if (counterState > 0){
      for(i in 1:counterState) {
        w <- paste(w, textInput(paste0("stateName", i), paste("State Name", i), value = input[[sprintf("stateName%d",i)]]))
        nbNamedState <- ifelse(input[[sprintf("stateName%d",i)]] != "", nbNamedState + 1, nbNamedState)
      }
    }
    value$nbNamedStates <- nbNamedState
    HTML(w)
  })
  
  output$transmatrix <- renderUI({
    req(value$nbNamedStates)
    nbNamedStates <- value$nbNamedStates
    nbStates = input$nbStates
    th <- ""
    tr <- ""
    h <- ""
    tab <- ""
    if (nbNamedStates > 0){
      h <- h3("Transition Matrix")
      tab <- "<table class='transmatrix'><th></th>"
      for (i in 1:nbNamedStates){
        th <- paste0(th, "<th style='text-align:center'>", input[[sprintf("stateName%d",i)]], "</th>")
        tr <- paste0(tr, "<tr><td>",input[[sprintf("stateName%d",i)]],"</td>")
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
  })
})