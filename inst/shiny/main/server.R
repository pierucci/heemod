source("interface.R")
library(dplyr)
library(heemod)
library(rgho)

REGION <- get_gho_codes(dimension = "REGION")
COUNTRY = get_gho_codes(dimension="COUNTRY")

shinyServer(function(input, output, session) {
  values <- reactiveValues(nbGlobalParameters = 1)
  loadedValues <- reactiveValues(loaded = 0, SP1 = 0, SP2 = 0, TM1 = 0, TM2 = 0, GP = 0)
  tmp <- reactiveValues(showStateParam = NULL)
  
  showStateParam <- function(nbStrat, input, values, click) {
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
                              value = ifelse(click == TRUE, 
                                             ifelse(!is.null(input[[paste0("stateVariable",1,i,j)]]), input[[paste0("stateVariable",1,i,j)]], 0),
                                             ifelse (!is.null(input[[paste0("stateVariable",x,i,j)]]), input[[paste0("stateVariable",x,i,j)]], 
                                                     ifelse(!is.null(input[[paste0("stateVariable",1,i,j)]]),input[[paste0("stateVariable",1,i,j)]],0)
                                             )),
                              label = NULL,
                              width="100%"))
                          })
                        }),
                      isolate({
                        tags$td(numericInput(
                          paste0("discountingRate",x,i),
                          label = NULL,
                          step=1,
                          value = ifelse(click == TRUE, 
                                         ifelse(!is.null(input[[paste0("discountingRate",1,i)]]), input[[paste0("discountingRate",1,i)]], 0),
                                         ifelse(!is.null(input[[paste0("discountingRate",x,i)]]), input[[paste0("discountingRate",x,i)]], 
                                                ifelse(!is.null(input[[paste0("discountingRate",1,i)]]), input[[paste0("discountingRate",1,i)]],0)
                                         )),
                          width="100%"))
                      })
                    )
                  })
              )
            )
          )
        })
    }
  }
  
  showTransMatrix <- function(nbStrat, input, values, click) {
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
                                value = ifelse(click == TRUE, 
                                               ifelse(!is.null(input[[paste0("transmatrix",1,i,j)]]), input[[paste0("transmatrix",1,i,j)]], "0"),
                                               ifelse(!is.null(input[[paste0("transmatrix",x,i,j)]]),input[[paste0("transmatrix",x,i,j)]],
                                                      ifelse(!is.null(input[[paste0("transmatrix",1,i,j)]]), input[[paste0("transmatrix",1,i,j)]], "0")
                                               )),
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
  
  showGlobalParameters <- function(nbStrat, input, values, click) {
    n <- values$nbGlobalParameters
    a <- tags$table(
      tags$tr(
        tags$th(style='text-align:center', "Variable name"),
        lapply(
          seq_len(nbStrat),
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
              seq_len(nbStrat),
              function(x) {
                isolate(tags$td(
                  textInput(
                    paste0("globalParamValue",x,i),
                    label = NULL,
                    value = ifelse(click == TRUE, 
                                   ifelse(!is.null(input[[paste0("globalParamValue",1,i)]]), input[[paste0("globalParamValue",1,i)]], ""),
                                   ifelse(!is.null(input[[paste0("globalParamValue",x,i)]]), input[[paste0("globalParamValue",x,i)]],
                                          ifelse(!is.null(input[[paste0("globalParamValue",1,i)]]), input[[paste0("globalParamValue",1,i)]], "")
                                   )),
                    width="100%")))
              }))
        })
    )
    
    tagList(
      a,
      actionButton("addParametersGP", "Add a new variable")
    )
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
      updateCheckboxInput(session, "use_morta", value = input$use_morta)
      updateNumericInput(session, "startAge", value = input$startAge)
      updateNumericInput(session, "cycleLength", value = input$cycleLength)
      updateRadioButtons(session, "gender", selected = input$gender)
    }
    loadedValues[["input"]] <- input
    loadedValues[["values"]] <- values
    #print(values$nbGlobalParameters)
  })
  
  output$saveButton <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.RData')
    },
    content = function(file) {
      save(input, values, file=file)
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
    FUN(1, input, values, click = FALSE)
  }
  
  copyValues <- function(trigger, input, values, FUN) {
    a <- eventReactive(input[[trigger]],{
      FUN(input$nbStrategies, input, values, TRUE)
    })
    
    return(a())
  }
  
  show_next <- function(val, trigger, input, values, FUN, required, loadedValues){
    req(required, input$nbStrategies>1)
    input[[trigger]]
    if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
      FUN(input$nbStrategies, input, values, click = FALSE)
    } 
    else if (input[[trigger]]){
      copyValues(trigger, input = input, values = values, FUN)
    } 
#     else {
#       FUN(input$nbStrategies, input, values, click = FALSE)
#     }
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
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
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
#   
#   copySP <- eventReactive(input$copyValuesParametersSP,{
#     showStateParam <- showStateParam(input$nbStrategies, input, values)
#   })
  
  
  
  output$stateParameters2 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
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
  
  observeEvent(input$addParametersGP, {
    if (loadedValues$loaded > 0){
      isolate(values$nbGlobalParameters <- loadedValues$values$nbGlobalParameters)
    }
    isolate(values$nbGlobalParameters <- values$nbGlobalParameters + 1)
  })
  
  output$globalParameters <- renderUI({
    req(input$nbStrategies)
    values$nbGlobalParameters
    if(loadedValues$loaded > 0 & isolate(loadedValues$GP < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$GP <- loadedValues$loaded
      print(values$nbGlobalParameters)
      showGlobalParameters(input$nbStrategies, input, values, click = FALSE)
    }
    else if (input$copyValuesParametersGP){
      copyValues("copyValuesParametersGP", input = input, values = values, showGlobalParameters)
    }
    else {
      
      
      if (loadedValues$loaded == 0 & input$copyValuesParametersGP[[1]] == 0) {
        #not sure whether it's a good shiny way of doing it,
        # but I can't figure out how to do better
        nbStrategies <- 1
      } else {
        nbStrategies <- input$nbStrategies
      }
      showGlobalParameters(nbStrategies, input, values, click = FALSE)
    }
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
  
  output$searchRegion <- renderUI({
    regionNames <- REGION %>%
      attr("labels")
    
    regionNames <- ifelse(
      regionNames == "NA" | grepl("^Not ", regionNames),
      "------",
      regionNames
    )
    
    vRegionCodes <- as.vector(REGION)
    names(vRegionCodes) <- regionNames
    
    selectizeInput(
      "regionChoice",
      label = "Region",
      selected <- ifelse(loadedValues$loaded == 0, "GLOBAL", loadedValues$input$regionChoice),
      choices = vRegionCodes
    )
  })
  
  output$searchCountry <- renderUI({
    req(input$regionChoice)
    
    countryCodes <- filter_attrs(
      COUNTRY,
      WHO_REGION_CODE == input$regionChoice
    )
    
    countryNames <- countryCodes %>%
      attr("labels")
    
    vCountryCodes <- as.vector(c("Global", countryCodes))
    names(vCountryCodes) <- c("Global", countryNames)
    
    selectizeInput(
      "countryChoice",
      label = "Country",
      choices = vCountryCodes,
      selected <-  ifelse(loadedValues$loaded == 0, "", loadedValues$input$countryChoice)
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
  
  output$debugParams <- renderUI({
    req(ux_nb_models(input))
    tagList(
      lapply(
        seq_len(ux_nb_models(input)),
        function(x) {
          renderPrint(ux_parameters(input, values, x))
        }
      )
    )
  })
  
  output$debugModels <- renderUI({
    req(ux_nb_models(input))
    tagList(
      lapply(
        seq_len(ux_nb_models(input)),
        function(x) {
          renderPrint(ux_model(
            input = input,
            values = values,
            model_number = x
          ))
      })
    )
  })
  
  output$debugRunModels <- renderPrint({
    ux_run_models_raw(input, values)
  })
})
