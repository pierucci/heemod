source("interface.R")
library(heemod)

shinyServer(function(input, output, session) {
  values <- reactiveValues(nbGlobalParameters = 1)
  loadedValues <- reactiveValues(loaded = 0, SP1 = 0, SP2 = 0, TM1 = 0, TM2 = 0)
  tmp <- reactiveValues(showStateParam = NULL)
  
  showStateParam <- function(nbStrat, input, values) {
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
  
  showTransMatrix <- function(nbStrat, input, values) {
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
              tags$div(
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
              ), style="text-align: center"
              ),
              tags$table(
                style="margin:0 auto;",
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
              ),
              hr()
            )
          })
      )
    }
  }
  
  observe({
    inFile <- input$loadButton
    if (is.null(inFile))
      return(NULL)
    else {
      load(inFile$datapath)
      updateNumericInput(session, "nbStates", value = input$nbStates)
      updateNumericInput(session, "nbStateVariables", value = input$nbStateVariables)
      updateNumericInput(session, "nbStrategies", value = input$nbStrategies)
    }
    loadedValues[["input"]] <- input
    loadedValues[["values"]] <- values
    loadedValues$SP1 <- 0
    
  })
  
  output$saveButton <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.RData')
    },
    content = function(file) {
      save(input, file=file)
    }
  )
  
  observeEvent(input$loadButton, {
    loadedValues$loaded <- loadedValues$loaded + 1
  })

  output$nameStates <- renderUI({
    req(input$nbStates)
    
    observeEvent(
      loadedValues$loaded, {
      input <- loadedValues$input
      values <- loadedValues$values
    })

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
    observeEvent(
      loadedValues$loaded, {
        input <- loadedValues$input
        values <- loadedValues$values
      })

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
    
    observeEvent(
      loadedValues$loaded, {
        input <- loadedValues$input
        values <- loadedValues$values
      })
    
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
  
  
  show_first <- function(val, FUN, required, loadedValues){
    req(required)
    if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
    }
    FUN(1, input, values)
  }
  
  copyValues <- function(trigger, input, values, FUN) {
    a <- eventReactive(input[[trigger]],{
      FUN(input$nbStrategies, input, values)
    })
    
    return(a())
  }
  
  show_next <- function(val, trigger, input, values, FUN, required, loadedValues){
    req(required)
    if (input[[trigger]]){
      copyValues(trigger, input = input, values = values, FUN)
    }
    else if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
      FUN(input$nbStrategies, input, values)
    } 
  }

    
  output$transMatrix1 <- renderUI({
    show_first(val = "TM1", FUN = showTransMatrix, required=input$nbStates, loadedValues)    
#     req(input$nbStates)
#     if(loadedValues$loaded > 0 & isolate(loadedValues$TM1 < loadedValues$loaded)){
#       input <- loadedValues$input
#       values <- loadedValues$values
#       loadedValues$TM1 <- loadedValues$loaded
#     }
#     showTransMatrix(1, input, values)

  })

#   copyTM <- eventReactive(input$copyValuesParametersTM,{
#     showTransMatrix <- showTransMatrix(input$nbStrategies, input, values)
#   })
  
  output$transMatrix2 <- renderUI({
    show_next(val = "TM2", trigger = "copyValuesParametersTM", input, values, showTransMatrix, c(input$nbStrategies, input$nbStates), loadedValues)
    
#     req(input$nbStrategies)
#     req(input$nbStates)
# 
#     if (input$copyValuesParametersTM)
#       copyValues("copyValuesParametersTM", input, values, showTransMatrix)
#       #copyTM()
#     else if(loadedValues$loaded > 0 & isolate(loadedValues$TM2 < loadedValues$loaded)){
#       input <- loadedValues$input
#       values <- loadedValues$values
#       loadedValues$TM2 <- loadedValues$loaded
#       showTransMatrix(input$nbStrategies, input, values)
#     }
#     else if (input$nbStates | input$nbStrategies)
#       showTransMatrix(input$nbStrategies, input, values)
  })  

  output$stateParameters1 <- renderUI({
    show_first(val = "SP1", FUN = showStateParam, required = c(input$nbStates, input$nbStateVariables), loadedValues = loadedValues)
#    req(input$nbStates)
#    req(input$nbStateVariables)
# 
#     if(loadedValues$loaded > 0 & isolate(loadedValues$SP1 < loadedValues$loaded)){
#       input <- loadedValues$input
#       values <- loadedValues$values
#       loadedValues$SP1 <- loadedValues$loaded
#     }
#     
#     showStateParam(1, input, values)
  })
  
#   copySP <- eventReactive(input$copyValuesParametersSP,{
#       showStateParam <- showStateParam(input$nbStrategies, input, values)
#   })
  

  
  output$stateParameters2 <- renderUI({
    show_next(val = "SP2", trigger = "copyValuesParametersSP", input, values, showStateParam, c(input$nbStrategies, input$nbStates, input$nbStateVariables), loadedValues)
#     req(input$nbStrategies)
#     req(input$nbStateVariables)
#     req(input$nbStates)
#     
#     if (input$copyValuesParametersSP)
#       copyValues("copyValuesParametersSP", input, values, showStateParam)
#     else if(loadedValues$loaded > 0 & isolate(loadedValues$SP2 < loadedValues$loaded)){
#       input <- loadedValues$input
#       values <- loadedValues$values
#       loadedValues$SP2 <- loadedValues$loaded
#       showStateParam(input$nbStrategies, input, values)
#     } 
    
  })
  
    
    
  output$costVariable <- renderUI({
    #####
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = input$variableStateName1
    )
  })
  output$effectVariable <- renderUI({
    isolate(if (loadedValues$loaded == TRUE){
      input <- loadedValues$input
      values <- loadedValues$values
    })
    
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
    isolate(if (loadedValues$loaded == TRUE){
      input <- loadedValues$input
      values <- loadedValues$values
    })
    
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
        tags$th(style='text-align:center', "Variable name"),
        lapply(
          seq_len(nbStrategies),
          function(x) {
            tags$th(style='text-align:center', paste("Value for", input[[paste0("strategyName",x)]]))
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
  
  output$outInit <- renderUI({
    #####
    req(
      nbState <- ux_nb_states(input),
      stateNames <- ux_state_names(input)
    )
    tagList(
      tags$h3("Initial counts per state"),
      tags$table(
        tagList(
          list(
            tags$th(""),
            tags$th(style='text-align:center', "Count")
          ),
          lapply(
            seq_len(nbState),
            function(i) {
              tags$tr(
                list(
                  tags$td(
                    strong(stateNames[i])),
                  tags$td(
                    if (i == 1) {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 1000,
                        width="100%"
                      )
                    } else {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 0,
                        width="100%"
                      )
                    }
                  )
                )
              )
            }
          )
        )
      )
    )
  })
  
  output$outModel <- renderUI({
    #####
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
    #####
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
    #####
    req(values$model)
    req(values$summary_model$res_comp)
    
    tagList(
      tags$h3("Efficiency frontier"),
      tags$p(paste(values$summary_model$frontier, collapse = " -> ")),
      tags$h3("Model comparison")
    )
  })
  
  output$tableICER <- DT::renderDataTable({
    #####
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
  
  output$outCounts <- renderUI({
    #####

    req(values$model)
    
      tagList(
        tags$h3("Plot state membership count"),
        selectInput(
          inputId = "modelPlotCounts",
          label = "Model",
          choices = as.vector(ux_model_names(input))
        )
      )
  })
  
  output$plotCounts <- renderPlot({
    #####
    req(values$model)
    model <- input$modelPlotCounts
    req(model)
    plot(
      values$model,
      type = "counts",
      model = model
    ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_colour_brewer(
        name = "State",
        palette = "Set1"
      )
  },
  width = 600)
})
