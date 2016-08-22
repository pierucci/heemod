#'
#' Create a state definition from tabular input.
#' @description Takes tabular input defining states for 
#'   multiple models and creates a string to be passed to
#'   \code{\link{define_model}}.   
#' @param state_info A data frame or the name of a file containing the information.
#' @return A list with three elements:
#'   \itemize{
#'    \item state_def_string The string that would get passed
#'    to \code{\link{define_state_list}}. 
#'    \item state_command The The call to create the states - 
#'    \code{\link{define_state_list}} with the previous item.
#'    \item state_names The names of the resulting states,
#'      for convenience.}
#'           
#' @details   
#' 
#'    Columns of state_info besides .model and state include 
#'      costs and utilities we want to keep track of,
#'      with appropriate values (these may include parameters).   
#'      For any cost
#'      or utility that should be discounted, 
#'      an additional column with the name
#'      ".discount.<cost>" or ".discount.<effect>", 
#'      for the appropriate cost or effect, can be included.
#'      If no discounting is desired for a particular
#'      cost or effect, the corresponding column can be omitted.   
#'      
#'      A discount column
#'      can contain only a single value - a cost or benefit
#'      must be discounted by the same amount in each state.
#'      Discounts can be numbers or parameters (which will then need to be defined
#'      like any other).
#' 
#'      The input data frame is expected to contain state information
#'      for all the models you will use in an analysis.
#'      For more information see the vignette: 
#'      \code{vignette("file-input", package = "heemod")}.`
#'
f_create_state_definitions_from_tabular <- function(state_info){
  if(is.character(state_info))
    state_info <- f_read_file(state_info)
  if(length(state_info) == 0 | !inherits(state_info, "data.frame"))
    stop("state_info must be a non-empty data frame (or the name of a file containing the frame")
  if(!("state" %in% names(state_info)))
    stop('"state" should be one of the names of the input')
  state_names <- state_info$state
  types <- setdiff(names(state_info), c(".model", "state"))
  discounts <- types[grep("^.discount", types)]
  types <- setdiff(types, discounts)
  
  
  ## build the "insides" of the define_state command,
  ##   with each of the characteristics
  state_strings <- sapply(types, function(this_col){
    res <- paste(this_col, state_info[, this_col], sep = "=")
    discount_col <- paste(".discount.", this_col, sep = "")
    
    some_discount <- discount_col %in% names(state_info)
    if(some_discount){
      discount_val <- unique(state_info[, discount_col])
      discount_val <- discount_val[!is.na(discount_val)]
      if(length(discount_val) > 1)
        stop(paste("more than one distinct value in", discount_col))
      if(length(discount_val) == 0)
        stop(paste("no valid discount value in", discount_col))
      if(discount_val < 0)
        stop(paste("negative discount value is invalid:", discount_col))
      res <- paste(this_col, "=discount(", 
                   state_info[, this_col],
                   ", ", discount_val, ")", 
                   sep = "")
    }
    res
  }
  )
  
  state_strings <- apply(state_strings, 1, paste, collapse = ", ", sep = "")
  ## now combine those into the commands making individual states,
  each_state_command <- 
    paste(state_names, " = define_state(", state_strings, ")", sep = "")
  state_defs <- paste(each_state_command, collapse = ", ")
  ##combine them to make a state list
  state_command <- paste("define_state_list(",
                         state_defs, 
                         ")", sep = "")
  
  list(state_def_string = state_defs, 
       state_command = state_command,
       state_names = state_names)
  
}


#' 
#' Create a transition matrix from tabular input.
#'   
#' @param trans_probs A data frame or the name of a data frame.
#'   
#' @param state_names The names of the states used in the transition matrix.
#'   
#' @return A list with two elements:  
#'    \itemize{
#'    \item \code{string}, a character string that can be used 
#'    in defining a heemod model in 
#'    \code{\link{f_create_model_from_tabular}}
#'    \item
#'    \code{transition_matrix}, the matrix itself 
#'    (created by evaluating the command using 
#'    \code{\link{define_matrix}} exactly
#'    as though the text had been entered at the command line).
#'    }
#' @details 
#' The data frame \code{trans_probs} should have columns
#'  \code{from}, \code{to}, and \code{prob}, where \code{prob} 
#'  is the probability
#'   of a transition from the \code{from} state to the \code{to} state.  
#'   Prob can be defined in terms of parameters, 
#'   just as when using \code{define_matrix} at the keyboard.
#'   Probabilities of 0 need not be specified - 
#'   they will be automatically inserted.
#'   
#'   All state names must be used in the "from" column of the 
#'   transition matrix (otherwise you can just get rid of the state). 
#'   Absorbing states should have
#'   a transition from and to themselves with probability 1.
#'    
#'   If trans_probs is a string, it is the file name of a file
#'   (.csv, .xls, or .xlsx) containing the data frame.

#'      The input data frame is expected to contain transition 
#'      probabilities for all the models you will use in an analysis.
#'      For more information see the vignette: 
#'      \code{vignette("file-input", package = "heemod")}.`
#'      

f_transition_prob_matrix_from_tabular <-
  function(trans_probs, state_names){
    ## if we get the name of a file instead of a data frame, 
    ##   check for acceptable file types and read it
    
    if(length(trans_probs) == 0) stop("must specify trans_probs")
    if(is.character(trans_probs))
      trans_probs <- f_read_file(trans_probs)
    
    ## error checking
    stopifnot(all(c("from", "to", "prob") %in% names(trans_probs)))
    
    unique_from_states <- unique(trans_probs$from)
    unique_states <- unique(c(trans_probs$from, trans_probs$to))
    
    ## we can have an initial state where people start but can't get to,
    ##   so we don't check trans_probs$to states the way we check
    ##   trans_probs$from states
    stopifnot(length(state_names) > 0)
    if(!identical(sort(unique_from_states), sort(state_names)))
      stop("not all specified states have a transition probability out")
    if(!identical(sort(unique_states), sort(state_names)))
      stop("states specified in transition matrix are not the same as state_names")
    
    ## set up matrix of 0's, and then add the listed probabilities
    num_states <- length(unique_states)
    prob_mat <- matrix(0, nrow = num_states, ncol = num_states,
                       dimnames = list(state_names, state_names))
    prob_mat[as.matrix(trans_probs[, c("to", "from")])] <- trans_probs$prob
    
    ## create the command to send to define_matrix
    probs <- paste(prob_mat, collapse = ",")
    sn <- paste('c("', 
                paste(state_names, collapse = "\",\""),
                '")', sep = "")
    this_command <- paste("define_matrix(",
                          probs, 
                          ", state_names=",
                          sn,
                          ")")
    list(string = this_command,
         transition_matrix = eval(parse(text = this_command)))
  }


#' Create a heemod parameter file from tabular input.
#' @param input_file_name character. Contains the name (and path) to 
#'   R, CSV, XLS or XLSX file with parameters.
#' @param output_file_name_base character. If not NULL, the parameters will be
#'   saved as .R files with this prefix. NULL by default.
#' @param param_obj_name base name for parameter objects
#' @details The tabular parameter definition file can have the following columns:
#'   parameter (the parameter name, required), value (required), 
#'   low and high (if both are present, a deterministic sensitivity analysis 
#'   will be performed), and psa (a definition of a distribution to use in a
#'   probabilistic sensitivity analysis.
#'   Other columns can exist, but will be ignored.
#'   
#'   If it ends in ".csv", ".xls", or ".xlsx",
#'   name of a .csv file defining the parameters (see details for contents).  
#'   If the file name
#'   ends in ".R", a parameter definition file that can
#'   be sourced.  (This can be useful if you've previously read
#'   a  parameter definition and want to make modifications
#'   directly in the R.)
#'   
#'   If output_file_name_base is not \code{NULL} (the default), then it is the
#'   file name into which a heemod parameter definition
#'   file will be written.  The basic parameter file will get extension ".R",
#'   and the discrete and probabilistic sensitivity analysis parameter 
#'   definition files (if 
#'   created) will get extensions ".dsa.R" and ".psa.R" respectively.   
#'   If \code{NULL}, no file will be written.
#'   
#'      For more information see the vignette: 
#'      \code{vignette("file-input", package = "heemod")}.`
#'   
#' @return the parameter definition, invisibly.

f_parameters_from_tabular <-
  function(input_file_name,
           output_file_name_base = NULL,
           param_obj_name = "params") {
    ## If we are given a file name ending in ".R", assume
    ##    that the files have already been defined.  Source
    ##    them and return the results
    if (substr(input_file_name,
               nchar(input_file_name) - 1,
               nchar(input_file_name)) == ".R") {
      output1 <- source(input_file_name)[[1]]
      dsa_file_name <- gsub(input_file_name, ".R$", ".dsa.R")
      if (file.exists(dsa_file_name))
        output2 <- source(dsa_file_name)[[1]]
      psa_file_name <- gsub(input_file_name, ".R$", ".psa.R")
      if (file.exists(psa_file_name))
        output3 <- source(psa_file_name)[[1]]
    }
    
    ## If instead we have a .csv file or xlsx file,
    ## put together the command that will create
    ##   the parameters file, and then write it to
    ##   an R file, which later can be sourced.
    
    else{
      # Avoid NOTE related to "visible binding for global variable"
      low <- high <- NULL
      param_defs <- f_read_file(input_file_name)
      
      pieces <-
        paste(param_defs$parameter,
              param_defs$value,
              collapse = ",\n",
              sep = " = ")
      output1 <- paste(param_obj_name,
                       "<- define_parameters(", pieces, ")")
      if (!is.null(output_file_name_base)) {
        output_file_name_R <-
          paste(output_file_name_base, ".R", sep = "")
        cat(output1, file = output_file_name_R)
      }
      output2 <- output3 <- NULL
      if ("low" %in% names(param_defs) &
          "high" %in% names(param_defs)) {
        ## now we'll do something similar for the deterministic
        ##   sensitivity analysis (dsa)
        output2 <- ""
        output_file_name_dsa_R <-
          paste(output_file_name_base, ".dsa.R", sep = "")
        ## columns where low and high are not populated
        ##   will have come in as NA
        param_defs_dsa <-
          subset(param_defs, !is.na(low) & !is.na(high))
        
        if (nrow(param_defs_dsa) > 0) {
          piece <-
            paste(
              param_defs_dsa$parameter,
              "= c(",
              param_defs_dsa$low,
              ",",
              param_defs_dsa$high,
              ")",
              collapse = ", \n"
            )
          param_names_dsa <-
            paste(param_obj_name, "_dsa", sep = "")
          output2 <- paste(param_names_dsa,
                           "<- define_sensitivity(", piece, ")")
          if(!is.null(output_file_name_base))
            cat(output2, file = output_file_name_dsa_R)
        }
      }
      
      ## and finally for probabilistic sensitivity analysis (dsa)
      output3 <- ""
      if ("psa" %in% names(param_defs)) {
        # Avoid NOTE related to "visible binding for global variable"
        psa <- NULL
           
        output_file_name_psa_R <-
          paste(output_file_name_base, ".psa.R", sep = "")
        ## columns where psa is not populated
        ##   will have come in as NA or blank
        param_defs_psa <- subset(param_defs, (!is.na(psa)) &
                                   (psa != ""))
        if (nrow(param_defs_psa) > 0) {
          piece <-
            paste(
              param_defs_psa$parameter,
              " ~ ",
              param_defs_psa$psa,
              sep = "",
              collapse = ", \n"
            )
          param_names_psa <-
            paste(param_obj_name, "_psa", sep = "")
          output3 <- paste(param_names_psa,
                           "<- define_distrib(", piece, ")")
          if(!is.null(output_file_name_base))
            cat(output3, file = output_file_name_psa_R)
        }
      }
    }
    
    if (is.character(output1))
      output1 <- eval(parse(text = output1))
    if (is.character(output2))
      output2 <- eval(parse(text = output2))
    if (is.character(output3))
      output3 <- eval(parse(text = output3))
    invisible(list(
      params = output1,
      dsa_params = output2,
      psa_params = output3
    ))
  }


#' 
#' Create a heemod model based on information in csv files.
#'   
#' @param state_file The name of a csv file to be used by 
#'   \code{f.create.state.definitions.from.tabular}.
#' @param tm_file The name of a csv file to be used by 
#'   \code{f.transition.prob.matrix.from.tabular}. 
#' @return A heemod model as returned by \code{define_model}.
#' @details
#'      For more information see the vignette: 
#'      \code{vignette("tabular-input", package = "heemod")}.`

f_create_model_from_tabular <- 
  function(state_file,
           tm_file){
    
    states <- f_create_state_definitions_from_tabular(state_file)
    TM <- f_transition_prob_matrix_from_tabular(tm_file, states$state_names)
    
    model_string <- paste("define_model(", 
                          states$state_def_string, 
                          ", transition_matrix =", TM$string, ")")
    eval(parse(text = model_string))
  }

#' Specify inputs for multiple models in a single file
#' @importFrom dplyr summarize
#' @description Parse a matrix containing specifications for 
#' multiple models into the inputs required for each model.
#' @param multi_spec \code{data frame}, or, if of type 
#' \code{character}, the name of a file that contains the data frame.
#' @param split_on \code{character}, with the name of the variable
#'   in multi_spec to be split on.
#' @param group_vars \code{character}, one or more variable names 
#'   from \code{multi_spec}.   
#'@details
#'  Each combination of values of the columns specified by 
#'  \code{group_vars} should either be unique in the file (in which
#'   case it will be replicated for all values of split_on), 
#'   or must be repeated as many times as unique values of split_on.
#'
#'      For more information see the vignette: 
#'      \code{vignette("file-input", package = "heemod")}.`

#' @return  a list of data frames, one for each value of split_on.
#'
f_parse_multi_spec <-
  function(multi_spec, split_on = ".model", group_vars){
    
    if(is.character(multi_spec)) multi_spec <- f_read_file(multi_spec)
    if(length(split_on) != 1) 
      stop("split_on must have length exactly 1")
    if(any(names(multi_spec) == ""))
      stop("multi_spec can't have empty names")
    if(!all(c(split_on, group_vars) %in% names(multi_spec)))
      stop("split_on and group_vars must be column names of the input multi_spec")
    
    remaining <- setdiff(names(multi_spec), c(split_on, group_vars))
    
    ## we allow blank lines in the files for readability; 
    ##   remove them if they're found
    multi_spec <- multi_spec[apply(!is.na(multi_spec), 1, any), ]
    
    ## any line that exists for one split but not others
    ##   needs to be duplicated for all splits
    ## throw an error if a parameter exists for more than one,
    ##   but not all
    unique_splits <- unique(multi_spec[, split_on])
    num_splits <- length(unique_splits)
    
    transitions <- dplyr::summarize(dplyr::group_by_(multi_spec, .dots = group_vars),
                             count = n())
    orig_order <- unique(multi_spec[, group_vars, drop = FALSE])
    wrong_number_times <- 
      transitions$count > 1 & transitions$count < num_splits
    if(any(wrong_number_times)){
      transitions$num_splits <- num_splits
      print("problem in multi_spec specification")
      print(transitions[wrong_number_times, , drop = FALSE])
      stop("group_var combinations must be specified either once for all splits or once for each split")
    }
    just_once <-
      dplyr::group_by_(multi_spec, .dots = group_vars) %>% 
      dplyr::filter(n() == 1) %>%
      dplyr::select(-dplyr::one_of(split_on))
    just_once <- 
      data.frame(temp = rep(unique_splits, nrow(just_once)),
                 just_once[rep(1:nrow(just_once), each = num_splits),],
                 stringsAsFactors = FALSE)
    names(just_once)[1] <- split_on
    more_than_once <-
      dplyr::group_by_(multi_spec, .dots = group_vars) %>% dplyr::filter(n() > 1)
    if(all(names(just_once) == names(more_than_once)) == FALSE){
         different_names <- which(names(just_once) != names(more_than_once))
         print("Differing names do not allow rbind:")
         print(cat("Coliding name(s) in part 1:", names(just_once)[different_names]))
         print(cat("Coliding name(s) in part 2:",names(more_than_once)[different_names]))
         stop("After splitting the file with states into two, at least one variable name is diferent. This is sometimes caused by a trailing space in the variable name.")
    }
    multi_spec <- rbind(just_once, as.data.frame(more_than_once))
    rownames(multi_spec) <- NULL
    almost <- split(multi_spec, multi_spec[, split_on])
    ## sort by order of appearance of split variables in multi_spec
    ##   (making first model mentioned the base model, similar to earlier system)
    
    orig_split_order <- unique(multi_spec[, split_on])
    almost <- almost[orig_split_order]
    ## want to make sure everything comes out sorted
    ##   by order of appearance of values of group_vars
    lapply(almost, function(x){
      match_to_orig <-
        sapply(group_vars, function(this_var){
          match(x[, this_var], orig_order[, this_var])})
      x[do.call("order", as.data.frame(match_to_orig)),]
    })
  }

#'
#' Read in demographic table.
#'
#' @param demographicTable A demographic table, which contains one column for each
#'   stratifying variable in the model (for example, age and sex).
#' @param weight_col_name  The name of the column that contains the weight of each
#'   demographic group defined by the stratifying variables.
#' @param params Parameters of a model, to check that all the columns in the demographic
#'   table (other than the weight column) are in the model.
#' @param ... catches other, unwanted arguments.
#'
#' @return  A data frame with the information from the file.
#' @export
#' @details The demographic table should be a data frame with n+1 columns, where there are
#'   n variables we use to stratify the population.    The first n columns
#'   correspond to the stratification variables (for example sex and age),
#'   and the column names must correspond to the variables used in calculating
#'   probabilities in the parameters object.
#'   The n+1-st column gives the proportion of the population in each group.
#'   The values in the n+1-st column should add to 1; if they do not, they will
#'   be renormalized, with a warning.

f_read_demographic_table <- function(demographicTable,
                                     weight_col_name = "weight",
                                     params, ...){
  ## if demographicTable is a string, it is a file name.  Read the file.
  if(is.character(demographicTable))
    demographicTable <- f_read_file(demographicTable)
  
  ## should have one and only one column giving weights
  weight_col <- which(names(demographicTable) == weight_col_name)
  if(length(weight_col) < 1)
    stop(paste("column", weight_col_name, "not found"))
  ## make sure all variables in the demographicTable 
  ##   are part of the model
  var_names <- names(demographicTable)[-weight_col]
  valid_names <- var_names %in% names(params)
  if(!all(valid_names)){
    invalid_names <- paste(var_names[!valid_names], collapse = "; ")
    stop(paste("the following names in demographicTable are not parameters of the model:"),
         invalid_names, sep = "\n\t\t")
  }
  
  ## warn if weights don't add to 1
  
  sum_weights <- sum(demographicTable[, weight_col])
  if(sum_weights != 1.0){
    warning(paste("weights in demographicTable add up to", sum_weights,
                  "which is different from 1.0; renormalizing"))
    demographicTable[, weight_col] <- demographicTable[, weight_col]/sum_weights
  }
  names(demographicTable)[names(demographicTable) == weight_col_name] <- ".weights"
  class(demographicTable) <- c("demographicTable", class(demographicTable))
  demographicTable
}


#' Read the accepted file formats for tabular input
#'
#' @param file_name name of the file
#' 
#' @return The file, as a data frame, with strings not converted to factors.
#' Columns containing 'comment' in the header (case in-sensitive) are ignored. #'
#' 
f_read_file <- function(file_name){
  have_xls <- is.xls(file_name)
  have_xlsx <- is.xlsx(file_name)
  have_csv <- is.csv(file_name)
  
  if(!have_csv & !have_xls & !have_xlsx)
    stop("file names must be for csv, xls, or xlsx")
  if(have_csv)
    temp <- read.csv(file_name, header = TRUE,
                     stringsAsFactors = FALSE,
                     strip.white = TRUE)

  if(have_xls | have_xlsx){
    if(!requireNamespace("readxl", quietly = TRUE))
      stop("readxl packaged needed to read Excel files")
    else 
       temp <- as.data.frame(readxl::read_excel(file_name, skip = 0))
  }
  
  ## get rid of "comment" columns, if any
  indx <- grep("comment", tolower(names(temp))) 
  if (length(indx) > 0) temp <- temp[ , -indx, drop = FALSE]

  ## get rid of NA rows that may have come from blank rows in file
  all.na <- apply(is.na(temp), 1 ,all)
  temp <- temp[!all.na, , drop = FALSE]
  temp
}


#' File checkers
#' 
#' @param input_file_name The file name
#' @return whether the file is (respectively) csv, xlsx, or xls
#' @rdname file_checkers
#'
is.csv <- function(input_file_name){
  substr(input_file_name,
         nchar(input_file_name) - 3,
         nchar(input_file_name)) == ".csv"
}

#' @rdname file_checkers
is.xlsx <- function(input_file_name){
  substr(input_file_name,
         nchar(input_file_name) - 4,
         nchar(input_file_name)) == ".xlsx"
}

#' @rdname file_checkers
is.xls <- function(input_file_name){
  substr(input_file_name,
         nchar(input_file_name) - 3,
         nchar(input_file_name)) == ".xls"
}
