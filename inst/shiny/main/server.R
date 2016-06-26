source("interface.R")
library(dplyr)
library(heemod)
library(rgho)

REGION <- NULL
COUNTRY <- NULL

try({
  REGION <- get_gho_codes(dimension = "REGION")
  COUNTRY <- get_gho_codes(dimension="COUNTRY")
})

MODULES <- c(
  "Simple equation" = "equation",
  "WHO mortality rate" = "rgho",
  "Survival modeling" = "survival",
  "Time-dependant variable" = "timedep"
)

shinyServer(function(input, output, session) {
  values <- reactiveValues(nGlobalParameters = 1, nEquation = 0, nRGHO = 0, nSurvival = 0, nTimedep = 0, nTimedepNC = 1)
  loadedValues <- reactiveValues(loaded = 0, SP1 = 0, SP2 = 0, TM1 = 0, TM2 = 0, GP = 0, DSA = 0, nameStates = 0, nameStateVariables=0, nameStrategies = 0)
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
  
  showGlobalParameters <- function(input, values) {
    n <- values$nGlobalParameters
    a <- tags$table(
      tags$tr(
        tags$th(style='text-align:center', "Variable name"),
        tags$th(style='text-align:center', "Value")
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
            isolate(tags$td(
              textInput(
                paste0("globalParamValue",i),
                label = NULL,
                value =  ifelse(!is.null(input[[paste0("globalParamValue", i)]]), input[[paste0("globalParamValue",i)]], ""),
                width="100%")))
          )
          
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
      updateSelectInput(session,"countMethod", selected = input$countMethod)
      updateNumericInput(session, "cycles", value = input$cycles)
    }
    loadedValues[["input"]] <- input
    loadedValues[["values"]] <- values
    #print(values$nGlobalParameters)
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
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStates < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStates <- loadedValues$loaded
    }
    
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
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStateVariables < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStateVariables <- loadedValues$loaded
    }
    
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
              if (i == 1)
                "cost"
              else if (i==2)
                "outcome"
              else
                paste0("variable_",i)))
        })
      })
  })
  
  output$nameStrategies <- renderUI({
    req(input$nbStrategies)
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStrategies < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStrategies <- loadedValues$loaded
    }
    
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
  
  
  show_first <- function(val, FUN, loadedValues, input){
    req(input$nbStates, input$nbStrategies)
    if (val == "SP1")
      req(input$nbStateVariables)
    for (i in 1:input$nbStates){
      req(input[[paste0("stateName", i)]])
    }
    req(input$strategyName1)
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
  
  show_next <- function(val, trigger, input, values, FUN, loadedValues){
    req(input$nbStates, input$nbStrategies > 1)
    if (val == "SP2")
      req(input$nbStateVariables)
    
    for (i in 1:input$nbStates){
      req(input[[paste0("stateName", i)]])
    }
    for (i in 1:input$nbStrategies){
      req(input[[paste0("strategyName", i)]])
    }
    input[[trigger]]
    if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
      FUN(input$nbStrategies, input, values, click = FALSE)
    } 
    else if (input[[trigger]]){
      copyValues(trigger, input = input, values = values, FUN)
    } else {
      FUN(input$nbStrategies, input, values, click = FALSE)
    }
  }
  
  output$transMatrix1 <- renderUI({
    show_first(val = "TM1", FUN = showTransMatrix, loadedValues, input)    
  })
  
  output$transMatrix2 <- renderUI({
    show_next(val = "TM2", trigger = "copyValuesParametersTM", input, values, showTransMatrix, loadedValues)
    
  })  
  
  output$stateParameters1 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_first(val = "SP1", FUN = showStateParam, loadedValues = loadedValues, input)
  })
  
  output$stateParameters2 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_next(val = "SP2", trigger = "copyValuesParametersSP", input, values, showStateParam, loadedValues)
  })
  
  
  
  output$costVariable <- renderUI({
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = ifelse(loadedValues$loaded == 0, input$variableStateName1, loadedValues$input$costVariable)
    )
  })
  output$effectVariable <- renderUI({
    textInput(
      "effectVariable",
      label = "Effect Variable",
      value = ifelse(loadedValues$loaded == 0, input$variableStateName2, loadedValues$input$effectVariable)
    )
  })
  
  searchRegion <- function(inputName, editRegion){
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
      inputName,
      label <- if (grepl("rghoEditRegion", inputName))
        "Region"
      else 
        NULL,
      selected = ifelse(grepl("rghoEditRegion", inputName), "GLOBAL", ifelse(is.null(input[[inputName]]),  editRegion, input[[inputName]])),#ifelse(loadedValues$loaded == 0, "GLOBAL", loadedValues$input$regionChoice),
      choices = vRegionCodes
    )
  }
  
  searchCountry <- function(inputName, regionName, editRegion, editCountry){
    if (grepl("rghoEditCountry", inputName)){
      req(editRegion)
    } else 
      req(input[[regionName]])
    countryCodes <- filter_attrs(
      COUNTRY,
      WHO_REGION_CODE == input[[regionName]]
    )
    
    countryNames <- countryCodes %>%
      attr("labels")
    
    vCountryCodes <- as.vector(c("Global", countryCodes))
    names(vCountryCodes) <- c("Global", countryNames)
    
    selectizeInput(
      inputName,
      label <- if(grepl("rghoEditCountry", inputName))
        "Country"
      else 
        NULL,
      choices = vCountryCodes,
      selected = ifelse(grepl("rghoEditCountry", inputName), "Global", ifelse(is.null(input[[inputName]]),  editCountry, input[[inputName]]))      
      #selected <-  ifelse(loadedValues$loaded == 0, "Global", loadedValues$input$countryChoice)
    )
  }
  
  
  observe({
    if (loadedValues$loaded > 0){
      values$nGlobalParameters <- loadedValues$values$nGlobalParameters
    }
  })
  
  observeEvent(input$addParametersGP, {
    isolate(values$nGlobalParameters <- values$nGlobalParameters + 1)
  })
  
  output$globalParameters <- renderUI({
    req(input$nbStrategies)
    values$nGlobalParameters
    if(loadedValues$loaded > 0 & isolate(loadedValues$GP < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$GP <- loadedValues$loaded
    }
    showGlobalParameters(input, values)
  })
  
  #   observeEvent(input$addDeterministic,{
  #     isolate(values$nbDeterministic <- values$nbDeterministic + 1)
  #   })
  
  output$DSA <- renderUI({
    req(input$nbStates, input$nbStrategies)
    if(loadedValues$loaded > 0 & isolate(loadedValues$DSA < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$DSA <- loadedValues$loaded
    }
    dsa <- lapply(seq_len(values$nGlobalParameters), function(i){
      isolate({
        tags$tr(
          tags$td(disabled(textInput(paste0("recGlobalParamName", i), NULL, ifelse(!is.null(input[[paste0("globalParamName", i)]]), input[[paste0("globalParamName", i)]], ""), width="100%"))),
          tags$td(numericInput(paste0("minValue", i), NULL, ifelse(!is.null(input[[paste0("minValue", i)]]), input[[paste0("minValue", i)]], ""), width="100%")),
          tags$td(numericInput(paste0("maxValue", i), NULL, ifelse(!is.null(input[[paste0("maxValue", i)]]), input[[paste0("maxValue", i)]], ""), width="100%"))
        )
      })
    })
    
    tagList(
      tags$table(style="margin:0 auto;", tags$th("Variable name", style='text-align:center'), tags$th("Minimum value", style='text-align:center'), tags$th("Maximum value", style='text-align:center'), dsa),
      fluidRow(column(3, offset = 5, actionButton("addDeterministic", "Add a deterministic value")))
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
                        value = ifelse (loadedValues$loaded == 0, 1000, ifelse(!is.null(loadedValues$input[[paste0("init",i)]]), loadedValues$input[[paste0("init",i)]], 1000)),
                        width="100%"
                      )
                    } else {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = ifelse (loadedValues$loaded == 0, 0, ifelse(!is.null(loadedValues$input[[paste0("init",i)]]), loadedValues$input[[paste0("init",i)]], 0)),
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
  
  output$globalParameters <- renderUI({
    fluidRow(
      column(
        1,
        actionLink("newParam", "", icon("plus-circle", "fa-3x"))),
      column(
        11,
        hidden(
          tags$table(
            id = "tabnewParam",
            lapply(seq_along(MODULES), function(i){
              tags$tr(
                tags$td(actionLink(MODULES[i], names(MODULES)[i]))
              )
            })
          )
        )
      )
    )
  })
  
  onevent(
    "mouseenter",
    "newParam",
    show(
      "tabnewParam",
      anim = TRUE,
      animType = "fade"
    ))
  onevent(
    "mouseleave",
    "globalParameters",
    hide(
      "tabnewParam",
      anim = TRUE,
      animType = "fade"
    ))
  
  lapply(MODULES, function(module) {
    observeEvent(input[[module]], {
      if (module == "equation"){
        hide("editingSurvival")
        hide("editingRGHO")
        hide("editingTimedep")
        shinyjs::show("editingEquation")
      } else if (module == "rgho"){
        hide("editingSurvival")
        hide("editingEquation")
        hide("editingTimedep")
        shinyjs::show("editingRGHO")
      } else if (module == "survival"){
        hide("editingEquation")
        hide("editingRGHO")
        hide("editingTimedep")
        shinyjs::show("editingSurvival")
      } else if (module == "timedep"){
        hide("editingEquation")
        hide("editingRGHO")
        hide("editingSurvival")
        shinyjs::show("editingTimedep")
      }
    })
  })
  
  output$addModule <- renderUI({
    nEquation <- values$nEquation
    nRGHO <- values$nRGHO
    nSurvival <- values$nSurvival
    nTimedep <- values$nTimedep
    tagList(
      hidden(
        wellPanel(
        id = "editingEquation",
        tags$table(
          tags$tr(
            tags$th("Variable Name"), 
            tags$th("Value")
          ),
          tags$tr(
            tags$td(textInput(paste0("equationEditName", nEquation), NULL, "")),
            tags$td(textInput(paste0("equationEditValue", nEquation), NULL, ""))
          )
        ), 
        actionButton("equationOK", "OK")
      )
    ),
    hidden(
      wellPanel(
        id = "editingRGHO",
        textInput(paste0("rghoEditName", nRGHO), "Variable Name", ""),
        numericInput(paste0("rghoEditStartAge", nRGHO), "Age at beginning", 0),
        radioButtons(
          paste0("rghoEditGender", nRGHO),
          "Sex",
          choices = c(Female = "FMLE", Male = "MLE")
        ),
        renderUI(searchRegion(paste0("rghoEditRegion", nRGHO), input[[paste0("rghoEditRegion", nRGHO)]])),
        renderUI(searchCountry(paste0("rghoEditCountry", nRGHO), paste0("rghoEditRegion", nRGHO), input[[paste0("rghoEditRegion", nRGHO)]], input[[paste0("rghoEditCountry", nRGHO)]])),
        actionButton("rghoOK", "OK")
      )
    ),
    hidden(
      wellPanel(
        id = "editingSurvival",
        fluidRow(
          column(3,
            textInput(paste0("survivalEditName", nSurvival), "Variable Name", "")
          ),
          column(3, 
            radioButtons(
              paste0("survivalEditDistribution", nSurvival),
              "Distribution", 
              choices = c("Exponential", "Weibull"),
              selected = isolate({ifelse (!is.null(input[[paste0("survivalEditDistribution", nSurvival)]]),
                                          input[[paste0("survivalEditDistribution", nSurvival)]],
                                          character(0))
              })
            )
          )
        ),
        fluidRow(
          column(3,
            renderUI({
            if (!is.null(input[[paste0("survivalEditDistribution", nSurvival)]])){
              tagList(
                numericInput(
                  paste0("survivalEditLambda", nSurvival),
                  "lambda",
                  NULL
                ),
                if (input[[paste0("survivalEditDistribution", nSurvival)]]== "Weibull"){
                  numericInput(
                    paste0("survivalEditK", nSurvival),
                    "k",
                    NULL
                  )
                }
              )
            }
          })
        )
        ),
        actionButton("survivalOK", "OK")
      )
    ),
    hidden(
      wellPanel(
        id = "editingTimedep",
        fluidRow(
          column(3,
                 textInput(paste0("timedepEditName", nTimedep), "Variable Name", "")
          ),
          column(6, 
                 radioButtons(
                   paste0("timedepEditType", nTimedep),
                   "Type of time-dependent variable", 
                   choices = c("Constant variation with the number of cycles" = "constant", "Non-constant variation with the number of cycles" = "nonConstant"),
                   selected = isolate({ifelse (!is.null(paste0("timedepEditType", nTimedep)),
                                                        paste0("timedepEditType", nTimedep),
                                                character(0))
                   })
                 )
          )
        ), 
        renderUI({
        fluidRow(
          if (!is.null(input[[paste0("timedepEditType", nTimedep)]])){
            if (input[[paste0("timedepEditType", nTimedep)]] == "constant")
            column(6,
                  textInput(paste0("timedepEditValueC", nTimedep), "Variation for each cycle", "") 
            )
            else if (input[[paste0("timedepEditType", nTimedep)]] == "nonConstant"){
              # tagList(
              #   tags$table(
              #     tags$tr(
              #       tags$th("Start Cycle"), 
              #       tags$th("End Cycle"), 
              #       tags$th("Value"),
              #       tags$th("")
              #     ),
              #     lapply(seq_len(values$nTimedepNC), function(i){
              #       isolate({
              #       tags$tr(
              #         tags$td(numericInput(paste0("timedepEditStartNC", i), "" , ifelse(i == 1, 1, 
              #                                                                           ifelse(!is.null(input[[paste0("timedepEditEndNC", i-1)]]), input[[paste0("timedepEditEndNC", i-1)]] + 1 , 
              #                                                                                 ifelse(!is.null(input[[paste0("timedepEditStartNC", i)]]), input[[paste0("timedepEditStartNC", i)]], ""))))),
              #         tags$td(numericInput(paste0("timedepEditEndNC", i), "" , ifelse(!is.null(input[[paste0("timedepEditEndNC", i)]]), input[[paste0("timedepEditEndNC", i)]], ""))),
              #         tags$td(numericInput(paste0("timedepEditValueNC", i),"" , ifelse(!is.null(input[[paste0("timedepEditValueNC", i)]]), input[[paste0("timedepEditValueNC", i)]], ""))),
              #         tags$td(actionLink(paste0("timedepEditValueNCDelete", i), icon("trash-o", "fa-2x")))
              #       )
              #       })
              #     })
              #   ),
              #   actionButton("timedepEditNewValueNC", "New value")
              # )
              div(class = "alert alert-warning", icon("exclamation-triangle"), "In construction")
            }
            
          }
        )
        }),
        actionButton("timedepOK", "OK")
      )
    )
  )
  })
  
#   observe({
#     k <- 0
#     for (i in seq_len(values$nTimedepNonConstant))
#       if (!is.null(input[[paste0("timedepEditNonConstant", i)]]) && input[[paste0("timedepEditNonConstant", i)]] != ""){
#         k <- k + 1 
#       }
#     if (k == values$nTimedepNonConstant)
#       values$nTimedepNonConstant <- values$nTimedepNonConstant + 1 
#   })
#   observe(print(values$nTimedepNonConstant))
  
  observe(
    values$survivalDistribution <- input[[paste0("survivalEditDistribution", values$nSurvival)]] #becomes NULL when input$survivalOK clicked
  )
  
  
  output$allModules <- renderUI({
    equations <- tagList(
      lapply(seq_len(values$nEquation), function(i){
        tags$tr(
          tags$td(textInput(paste0("equationName", i), NULL, ifelse(is.null(input[[paste0("equationName", i)]]), input[[paste0("equationEditName", i-1)]], input[[paste0("equationName", i)]]))),
          tags$td(textInput(paste0("equationValue", i), NULL, ifelse(is.null(input[[paste0("equationValue", i)]]), input[[paste0("equationEditValue", i-1)]], input[[paste0("equationValue", i)]])))
        )
      })
    )
    rgho <- tagList(
      lapply(seq_len(values$nRGHO), function(i){
        tags$tr(
          tags$td(textInput(paste0("rghoName", i), NULL, ifelse(is.null(input[[paste0("rghoName", i)]]), input[[paste0("rghoEditName", i-1)]], input[[paste0("rghoName", i)]]))),
          tags$td(numericInput(paste0("rghoStartAge",i), NULL, ifelse(is.null(input[[paste0("rghoStartAge", i)]]), input[[paste0("rghoEditStartAge", i-1)]], input[[paste0("rghoStartAge", i)]]))),
          tags$td(renderUI(searchRegion(paste0("rghoRegion", i), input[[paste0("rghoEditRegion", i-1)]]))),
          tags$td(renderUI(searchCountry(paste0("rghoCountry", i), paste0("rghoRegion", i), input[[paste0("rghoEditRegion", i-1)]], input[[paste0("rghoEditCountry", i-1)]]))),
          tags$td(radioButtons(paste0("rghoGender",i), NULL, choices = c(Female = "FMLE", Male = "MLE"), selected = ifelse(is.null(input[[paste0("rghoGender", i)]]), input[[paste0("rghoEditGender", i-1)]], input[[paste0("rghoGender", i)]])))
        )
      })
    )
    survival <- tagList(
      lapply(seq_len(values$nSurvival), function(i){
        tags$tr(
          tags$td(textInput(paste0("survivalName", i), NULL, ifelse(is.null(input[[paste0("survivalName", i)]]), isolate(input[[paste0("survivalEditName", i-1)]]), input[[paste0("survivalName", i)]]))),
          tags$td(radioButtons(paste0("survivalDistribution", i), NULL, choices = c("Exponential", "Weibull"), selected = ifelse (is.null(input[[paste0("survivalDistribution", i)]]), isolate(input[[paste0("survivalEditDistribution", i-1)]]), input[[paste0("survivalDistribution", i)]]))),
          tags$td((numericInput(paste0("survivalLambda", i), NULL, ifelse(is.null(input[[paste0("survivalLambda", i)]]), isolate(input[[paste0("survivalEditLambda", i-1)]]), input[[paste0("survivalLambda", i)]])))),
          if (!is.null(input[[paste0("survivalDistribution", i)]]) && input[[paste0("survivalDistribution", i)]] == "Weibull" | isolate({!is.null(values[[paste0("survivalEditDistribution", i-1)]]) && values[[paste0("survivalEditDistribution", i-1)]] == "Weibull"}))
            tags$td((numericInput(paste0("survivalK", i), NULL, ifelse(!is.null(input[[paste0("survivalK", i)]]), input[[paste0("survivalK", i)]], 
                                  ifelse(!is.null(isolate(input[[paste0("survivalEditK", i-1)]])), isolate(input[[paste0("survivalEditK", i-1)]]), "")))
          ))
        )
      })
    )
    timedep <- tagList(
      lapply(seq_len(values$nTimedep), function(i){
        tags$tr(
          tags$td(textInput(paste0("timedepName", i), NULL, ifelse(is.null(input[[paste0("timedepName", i)]]), isolate(input[[paste0("timedepEditName", i-1)]]), input[[paste0("timedepName", i)]]))),
          tags$td(radioButtons(paste0("timedepType", i), NULL, choices = c("Constant variation with the number of cycles" = "constant", "Non-constant variation with the number of cycles" = "nonConstant"), selected=ifelse(is.null(input[[paste0("timedepType", i)]]), isolate(input[[paste0("timedepEditType", i-1)]]), input[[paste0("timedepType", i)]]))),
          tags$td(textInput(paste0("timedepValueC", i), NULL, ifelse(is.null(input[[paste0("timedepValueC", i)]]), isolate(input[[paste0("timedepEditValueC", i-1)]]), input[[paste0("timedepValueC", i)]])))
          
        )
      })
    )
    tagList(
      if (values$nEquation > 0){
        tagList(
          h4(names(MODULES)[1]),
          tags$table(tags$tr(tags$th("Name"), tags$th("Value")), equations)
        )
      },
      if (values$nRGHO > 0){
        tagList(
          h4(names(MODULES)[2]),
          tags$table(tags$tr(tags$th("Name"), tags$th("Age at beginning"), tags$th("Region"), tags$th("Country"), tags$th("Sex")), rgho)
        )
      },
      if (values$nSurvival > 0){
        tagList(
          h4(names(MODULES)[3]),
          tags$table(
            tags$tr(tags$th("Name"), 
                    tags$th("Distribution"), 
                    tags$th("lambda"), 
                    #if (!is.null(survivalK))
                    tags$th("k"), 
                    survival))
        )
      },
      if (values$nTimedep > 0){
        tagList(
          h4(names(MODULES)[4]),
          tags$table(tags$tr(tags$th("Name"), tags$th("Type")), timedep)
        )
      }
    )
  })
  
  observeEvent(input$equationOK, {
    hide("editingEquation")
    values$nEquation <- values$nEquation + 1
  })
  observeEvent(input$rghoOK, {
    hide("editingRGHO")
    values$nRGHO <- values$nRGHO + 1
  })
  observeEvent(input$survivalOK, {
    hide("editingSurvival")
    values$nSurvival <- values$nSurvival + 1
    values$survivalDistribution <- NULL
  })
  observeEvent(input$timedepOK, {
    hide("editingTimeDep")
    values$nTimedep <- values$nTimedep + 1
  })
  # observeEvent(input$timedepEditNewValueNC, {
  #   values$nTimedepNC <- values$nTimedepNC +1 
  # })
  # observe({
  #   lapply(seq_len(values$nTimedepNC), function(i){
  #     observeEvent(input[[paste0("timedepEditValueNCDelete", i)]], {
  #     for (j in i:(length(values$nTimedepNC)-1)){
  #       updateNumericInput(session, paste0("timedepEditStartNC", j), value = input[[paste0("timedepEditStartNC", j+1)]]) 
  #       updateNumericInput(session, paste0("timedepEditEndNC", j), value = input[[paste0("timedepEditEndNC", j+1)]])
  #       updateNumericInput(session, paste0("timedepEditValueNC", j), value = input[[paste0("timedepEditValueNC", j+1)]])
  #       
  #     }
  #      values$nTimedepNC <- values$nTimedepNC - 1
  #     })
  #   })
  # })
  # 
  # observe(print(values$nTimedepNC))
})

