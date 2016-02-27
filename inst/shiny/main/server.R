source("interface.R")
library(heemod)

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(nbGlobalParameters = 1)
  
  showStateParam <- function(nbStrat) {
    nbStates <- input$nbStates
    
    nbStateVariables <- input$nbStateVariables
    
    req(nbStates)
    req(nbStateVariables)
    
    stateName <- ""
    variableStateName <- ""
    
    for (i in seq_len(nbStates)) {
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    
    for (i in seq_len(nbStateVariables)) {
      variableStateName[i] <- input[[paste0("variableStateName", i)]]
    }
    
    if (input$nbStates > 0) {
      start <- ifelse(nbStrat > 1, 2, 1)
      
      lapply(
        seq(from = start, to = nbStrat),
        function(x) {
          tagList(
            h3(paste("State Parameters for", input[[paste0("strategyName",x)]])),
            tags$table(
              class='stateVariables',
              tagList(
                tags$th(),
                lapply(
                  seq_len(nbStates),
                  function(i) {
                    tags$th(style='text-align:center', stateName[i])
                  }),
                tags$th(style='text-align:center', "Discounting Rate"),
                lapply(
                  seq_len(nbStateVariables),
                  function(i) {
                    tags$tr(
                      tags$td(variableStateName[i]),
                      lapply(
                        seq_len(nbStates),
                        function (j) {
                          isolate({
                            tags$td(textInput(
                              paste0("stateVariable",x,i,j),
                              value = ifelse(
                                !is.null(input[[paste0("stateVariable",1,i,j)]]),
                                input[[paste0("stateVariable",1,i,j)]],
                                0),
                              label = NULL,
                              width="100%"))
                          })
                        }),
                      isolate({
                        tags$td(numericInput(
                          paste0("discountingRate",x,i),
                          label = NULL,
                          value = ifelse(
                            !is.null(input[[paste0("discountingRate",1,i)]]),
                            input[[paste0("discountingRate",1,i)]],
                            0),
                          width="100%"))})
                    )
                  })
              )
            )
          )
        })
    }
  }
  
  showTransMatrix <- function(nbStrat) {
    nbStates <- input$nbStates
    
    req(nbStates)
    
    stateName <- ""
    start <- ifelse(nbStrat > 1, 2, 1)
    
    for (i in seq_len(nbStates)) {
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    
    if (input$nbStates > 0) {
      tagList(
        lapply(
          seq(from = start, to = nbStrat),
          function(x) {
            tagList(
              h3(paste("Transition Matrix for", input[[paste0("strategyName",x)]])),
              renderPlot({
                tm <- ux_matrix(input, x)
                if (is.null(tm)) {
                  plot.new()
                  text(.5, .5, "Incorrect\ninput")
                } else {
                  plot(tm)
                }
              },
              width = 200,
              height = 200
              ),
              tags$table(
                class='transmatrix',
                tagList(
                  tags$th(),
                  lapply(
                    seq_len(nbStates),
                    function(i) {
                      tags$th(style='text-align:center', stateName[i])
                    }),
                  lapply(
                    seq_len(nbStates),
                    function(i) {
                      tags$tr(
                        tags$td(stateName[i]),
                        lapply(
                          seq_len(nbStates),
                          function(j) {
                            isolate(
                              tags$td(textInput(
                                paste0("transmatrix",x,i,j),
                                value = ifelse(
                                  !is.null(input[[paste0("transmatrix",1,i,j)]]),
                                  input[[paste0("transmatrix",1,i,j)]],
                                  "0"),
                                label=NULL,
                                width="100%")))
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
    lapply(
      seq_len(input$nbStates),
      function(i) {
        isolate({
          textInput(
            paste0("stateName", i),
            paste("State Name", i),
            value = ifelse(
              !is.null(input[[paste0("stateName",i)]]),
              input[[paste0("stateName",i)]],
              LETTERS[i]))
        })
      })
  })
  
  output$nameStateVariables <- renderUI({
    req(input$nbStateVariables)
    lapply(
      seq_len(input$nbStateVariables),
      function(i) {
        isolate({
          textInput(
            paste0("variableStateName", i),
            paste("Variable Name", i),
            value = ifelse(
              !is.null(input[[paste0("variableStateName",i)]]),
              input[[paste0("variableStateName",i)]],
              paste0("variable_",i)))
        })
      })
  })
  
  output$nameStrategies <- renderUI({
    req(input$nbStrategies)
    lapply(
      seq_len(input$nbStrategies),
      function(i) {
        isolate({
          textInput(
            paste0("strategyName", i),
            paste("Strategy Name", i),
            value = ifelse(
              !is.null(input[[paste0("strategyName",i)]]),
              input[[paste0("strategyName",i)]],
              as.character(as.roman(i))))
        })
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
  
  output$costVariable <- renderUI({
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = input$variableStateName1
    )
  })
  output$effectVariable <- renderUI({
    textInput(
      "effectVariable",
      label = "Effect Variable",
      value = input$variableStateName2
    )
  })
  
  observe({
    req(input$addParametersGP)
    isolate(values$nbGlobalParameters <- values$nbGlobalParameters + 1)
  })
  
  output$globalParameters <- renderUI({
    n <- values$nbGlobalParameters
    
    req(input$nbStrategies)
    
    if (input$copyValuesParametersGP[[1]] == 0) {
      #not sure whether it's a good shiny way of doing it,
      # but I can't figure out how to do better
      
      nbStrategies <- 1
    } else {
      nbStrategies <- input$nbStrategies
    }
    
    a <- tags$table(
      tags$tr(
        tags$th("Variable name"),
        lapply(
          seq_len(nbStrategies),
          function(x) {
            tags$th(paste("Value for", input[[paste0("strategyName",x)]]))
          })
      ),
      lapply(
        seq_len(n),
        function(i) {
          tags$tr(
            isolate(tags$td(
              textInput(
                paste0("globalParamName",i),
                label = NULL,
                value = input[[paste0("globalParamName",i)]],
                width="100%"))),
            lapply(
              seq_len(nbStrategies),
              function(x) {
                isolate(tags$td(
                  textInput(
                    paste0("globalParamValue",x,i),
                    label = NULL,
                    value = input[[paste0("globalParamValue",1,i)]],
                    width="100%")))
              }))
        })
    )
    
    tagList(
      a,
      actionButton("addParametersGP", "Add a new variable")
    )
  })
  
  output$outModel <- renderUI({
    
    values$model <- ux_run_models(input = input, values = values)
    values$summary_model <- summary(values$model)
    
    if (is.null(values$model)) {
      tagList(tags$h3("Model specification incomplete"))
    } else {
      tagList(
        tags$h1("Model results"),
        tags$h3("Total values")
      )
    }
  })
  
  output$tableResults <- DT::renderDataTable({
    
    req(values$model)
    req(values$summary_model$res)
    
    DT::datatable(
      values$summary_model$res,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$titleICER <- renderUI({
    
    req(values$model)
    req(values$summary_model$res_comp)
    
    tagList(
      tags$h3("Efficiency frontier"),
      tags$p(paste(values$summary_model$frontier, collapse = " -> ")),
      tags$h3("Model comparison")
    )
  })
  
  output$tableICER <- DT::renderDataTable({
    
    req(values$model)
    req(values$summary_model$res_comp)
    
    DT::datatable(
      values$summary_model$res_comp,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
})
