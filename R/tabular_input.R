#' Create State Definitions From Tabular Input
#' 
#' Transforms tabular input defining states into an
#' \code{heemod} object.
#' 
#' Columns of state_info besides .model and state include 
#' costs and utilities we want to keep track of, with 
#' appropriate values (these may include parameters). For 
#' any cost or utility that should be discounted, an 
#' additional column with the name ".discount.<cost>" or 
#' ".discount.<effect>", for the appropriate cost or effect,
#' can be included. If no discounting is desired for a 
#' particular cost or effect, the corresponding column can 
#' be omitted.
#' 
#' A discount column can contain only a single value - a 
#' cost or benefit must be discounted by the same amount in 
#' each state. Discounts can be numbers or parameters (which
#' will then need to be defined like any other).
#' 
#' The input data frame is expected to contain state 
#' information for all the models you will use in an 
#' analysis. For more information see the vignette: 
#' \code{vignette("file-input", package = "heemod")}.
#' 
#' @param state_info Result for one model of 
#'   \code{\link{parse_multi_spec}}.
#'   
#' @return A state list.
create_states_from_tabular <- function(state_info,
                                       df_env) {
  if(length(state_info) == 0 | !inherits(state_info, "data.frame"))
    stop("state_info must be a non-empty data frame (or the name of a file containing the frame).")
  
  if(!("state" %in% names(state_info)))
    stop("'state' should be a column name.")
  
  if (any(duplicated(state_info$state))) {
    stop(sprintf(
      "Duplicated state names: %s.",
      paste(unique(state_info$state[duplicated(state_info$state)]),
            sep = ", ")
    ))
  }
  
  state_names <- state_info$state
  values <- setdiff(names(state_info), c(".model", "state"))
  discounts <- values[grep("^\\.discount", values)]
  values <- setdiff(values, discounts)
  discounts_clean <- gsub("^\\.discount\\.(.+)", "\\1", discounts)
  
  if (! all(discounts_clean %in% values)) {
    stop(sprintf(
      "Discounting rates defined for non-existing values:\n  %s",
      paste(discounts[! discounts %in% values], collapse = ", ")
    ))
  }
  
  for (n in discounts) {
    if (all(is.na(state_info[[n]]))) {
      stop(sprintf(
        "No discount values found for '%s'.", n
      ))
    } else if (length(unique(na.omit(state_info[[n]]))) > 1) {
      stop(sprintf(
        "Multiple discount values for '%s'.", n
      ))
    } else {
      state_info[[n]] <- na.omit(state_info[[n]])[1]
    }
  }
  
  for (n in discounts_clean) {
    state_info[[n]] <- sprintf(
      "discount(%s, %s)",
      state_info[[n]],
      state_info[[paste0(".discount.", n)]]
    )
  }
  
  define_state_list_(
    setNames(lapply(
      state_info$state,
      function(state) {
        define_state_(
          lazyeval::as.lazy_dots(
            setNames(lapply(
              values,
              function(value) {
                state_info[[value]][state_info$state == state]
              }
            ), values),
            env = df_env
          )
        )
      }
    ), state_info$state)
  )
}


#' Create a Transition Matrix From Tabular Input
#' 
#' Transforms tabular input defining a transition matrix 
#' into an \code{heemod} object.
#' 
#' The data frame \code{trans_probs} should have columns 
#' \code{from}, \code{to}, and \code{prob}, where 
#' \code{prob} is the probability of a transition from the 
#' \code{from} state to the \code{to} state. Prob can be 
#' defined in terms of parameters, just as when using 
#' \code{define_matrix} at the keyboard. Probabilities of 0 
#' need not be specified - they will be automatically 
#' inserted.
#' 
#' All state names must be used in the \code{from} column of
#' the transition matrix (otherwise you can just get rid of
#' the state). Absorbing states should have a transition
#' from and to themselves with probability 1.
#' 
#' @param trans_probs  Result for one model of 
#'   \code{\link{parse_multi_spec}}.
#' @param state_names The names of the states used in the 
#'   transition matrix.
#'   
#' @return A transition matrix.
create_matrix_from_tabular <- function(trans_probs, state_names,
                                       df_env) {
  stopifnot(
    all(c("from", "to", "prob") %in% names(trans_probs)),
    length(state_names) > 0,
    all(trans_probs$from %in% state_names),
    all(trans_probs$to %in% state_names)
  )
  
  unique_states <- unique(c(trans_probs$from, trans_probs$to))
  
  ## we can have an initial state where people start but
  ## can't get to, so we don't check trans_probs$to states
  ## the way we check trans_probs$from states
  
  if (! all(trans_probs$to %in% trans_probs$from)) {
    stop(sprintf(
      "Some states do not have an exit probability: %s.",
      paste(
        unique(trans_probs$to)[! unique(trans_probs$to) %in% trans_probs$from],
        sep = ", "
      )
    ))
  }
  
  if(! all(unique_states %in% state_names)) {
    stop(sprintf(
      "Some states specified in the transition matrix differ from 'state_names': %s.",
      unique_states[! unique_states %in% state_names]
    ))
  }
  
  ## set up matrix of 0's, and then add the listed probabilities
  num_states <- length(unique_states)
  prob_mat <- matrix(0, nrow = num_states, ncol = num_states,
                     dimnames = list(state_names, state_names))
  prob_mat[as.matrix(trans_probs[, c("to", "from")])] <- trans_probs$prob
  
  define_matrix_(
    lazyeval::as.lazy_dots(prob_mat, env = df_env),
    state_names = state_names
  )
}

#' Create a Parameter Definition From Tabular Input
#' 
#' If specified in the tabular file, DSA and PSA can also be
#' created.
#' 
#' The tabular parameter definition file can have the 
#' following columns: \code{parameter} (the parameter name, 
#' required), \code{value} (required), \code{low} and 
#' \code{high} (if both are present, a deterministic 
#' sensitivity analysis will be performed), and \code{psa} 
#' (a definition of a distribution to use in a probabilistic
#' sensitivity analysis. Other columns will be ignored.
#' 
#' @param param_defs A parameter definition file.
#' @return the parameter definition, invisibly.
create_parameters_from_tabular <- function(param_defs,
                                           df_env) {
  if (xor("low" %in% names(param_defs),
          "high" %in% names(param_defs))) {
    stop("Both 'low' and 'high' columns must be present in parameter tabular file to define DSA.")
  }
  
  parameters <- define_parameters_(
    lazyeval::as.lazy_dots(
      setNames(
        lapply(param_defs$value, function(x) x),
        param_defs$parameter
      ),
      env = df_env
    )
  )
  
  output2 <- output3 <- NULL
  if ("low" %in% names(param_defs) &
      "high" %in% names(param_defs)) {
    
    if (! all(is.na(param_defs$low) ==
              is.na(param_defs$high))) {
      stop("'low' and 'high' must be both present in DSA tabular definition.")
    }
      
      param_sens
    dsa <- define_sensitivity_(
      lazyeval::as.lazy_dots(
        lapply()
      )
    )
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


#' Create a \code{heemod} Model From Tabular Files Info
#' 
#' Calls \code{\link{create_states_from_tabular}} and
#' \code{\link{create_matrix_from_tabular}}.
#' 
#' @param state_file A state tabular file (file path or 
#'   parsed file).
#' @param tm_file A transition matrix tabular file (file 
#'   path or parsed file).
#' @return A \code{heemod} model as returned by 
#'   \code{\link{define_model}}.
create_model_from_tabular <- function(state_file,
                                      tm_file,
                                      df_env) {
  
  states <- create_states_from_tabular(state_file,
                                       df_env = df_env)
  TM <- create_matrix_from_tabular(tm_file, states$state_names,
                                   df_env = df_env)
  
  define_model_(transition_matrix = TM, states = states)
}

#'Specify Inputs for Multiple Models From a Single File
#'
#'Parse a \code{data.frame} containing specifications for 
#'multiple models into a list of inputs required for each 
#'model.
#'
#'Each combination of values of the columns specified by 
#'\code{group_vars} should either be unique in the file (in 
#'which case it will be replicated for all values of 
#'\code{split_on}), or must be repeated as many times as 
#'unique values of \code{split_on}.
#'
#'\code{split_on} is usually the model name.
#'
#'\code{group_var} can be the state names, or from and to 
#'lines for a matrix definition...
#'
#'@param multi_spec \code{data frame}, or, if of type 
#'  \code{character}, the name of a file that contains the 
#'  data frame.
#'@param split_on \code{character} of length 1, with the 
#'  name of the variable in \code{multi_spec} to be split 
#'  on.
#'@param group_vars \code{character}, one or more variable 
#'  names from \code{multi_spec} that identify a line of 
#'  information.
#'  
#'@return A list of data frames, one for each value of 
#'  \code{split_on.}
#'  
parse_multi_spec <- function(multi_spec,
                             split_on = ".model",
                             group_vars,
                             df_dir) {
  
  if(is.character(multi_spec)) multi_spec <- read_file(multi_spec)
  
  if(length(split_on) != 1) stop("'split_on' must have a length of exactly 1.")
  if(any(names(multi_spec) == "")) stop("'multi_spec' can't have empty names")
  # but they can have no names??
  
  if(! all(c(split_on, group_vars) %in% names(multi_spec)))
    stop("'split_on' and 'group_vars' must be column names of the input 'multi_spec'")
  
  remaining <- setdiff(names(multi_spec), c(split_on, group_vars))
  
  multi_spec <- filter_blanks(multi_spec)
  
  ## any line that exists for one split but not others
  ##   needs to be duplicated for all splits
  ## throw an error if a parameter exists for more than one,
  ##   but not all
  unique_splits <- unique(multi_spec[, split_on])
  num_splits <- length(unique_splits)
  
  occurences <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>% 
    dplyr::summarize_(count = ~ n())
  
  orig_order <- unique(multi_spec[, group_vars, drop = FALSE])
  
  wrong_number_times <- 
    occurences$count > 1 & occurences$count < num_splits
  
  if(any(wrong_number_times)){
    occurences$num_splits <- num_splits
    cat("Problem in multi_spec specification:\n")
    print(occurences[wrong_number_times, , drop = FALSE])
    stop("'group_var' combinations must be specified either once for all splits or once for each split.")
  }
  
  just_once <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>% 
    dplyr::filter_(~ n() == 1) %>%
    dplyr::select_(~ - dplyr::one_of(split_on))
  
  just_once <- 
    data.frame(
      temp = rep(unique_splits, nrow(just_once)),
      just_once[rep(seq_len(nrow(just_once)), each = num_splits), ],
      stringsAsFactors = FALSE
    )
  
  names(just_once)[1] <- split_on
  
  more_than_once <- multi_spec %>% 
    dplyr::group_by_(.dots = group_vars) %>%
    dplyr::filter_(~ n() > 1)
  
  if(! all(names(just_once) == names(more_than_once))) {
    different_names <- names(just_once) != names(more_than_once)
    cat("Differing names do not allow rbind:\n")
    cat("Coliding name(s) in part 1:",
        names(just_once)[different_names], "\n")
    cat("Coliding name(s) in part 2:",
        names(more_than_once)[different_names], "\n")
    stop("After splitting the file with states into two, at least one variable name is different. This is sometimes caused by a trailing space in the variable name.")
  }
  
  multi_spec <- rbind(just_once, as.data.frame(more_than_once))
  
  rownames(multi_spec) <- NULL
  list_spec <- split(multi_spec, multi_spec[, split_on])
  ## sort by order of appearance of split variables in multi_spec
  ##   (making first model mentioned the base model, similar to earlier system)
  
  orig_split_order <- unique(multi_spec[, split_on])
  list_spec <- list_spec[orig_split_order]
  ## want to make sure everything comes out sorted
  ##   by order of appearance of values of group_vars
  lapply(list_spec, function(x) {
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
#' Columns named '.comment' are ignored.
#' 
#' @param file_name File name.
#' 
#' @return A \code{data.frame}.
#'  
read_file <- function(file_name) {
  
  have_xls <- is_xls(file_name)
  have_xlsx <- is_xlsx(file_name)
  have_csv <- is_csv(file_name)
  
  if(! have_csv & ! have_xls & ! have_xlsx) {
    stop("file names must be for csv, xls, or xlsx")
  }
  
  if(have_csv)
    tab <- utils::read.csv(
      file_name, header = TRUE,
      stringsAsFactors = FALSE,
      strip.white = TRUE
      
    ) else if(have_xls | have_xlsx) {
      if (! requireNamespace("readxl", quietly = TRUE)) {
        stop("readxl packaged needed to read Excel files")
        
      } else {
        tab <- as.data.frame(
          readxl::read_excel(file_name)
        )
      }
    }
  
  ## get rid of "comment" columns, if any
  tab <- tab[! grepl("^\\.comment", names(tab))]
  
  ## get rid of NA rows that may have come from blank rows in file
  tab <- filter_blanks(tab)
  tab
}

#' Remove Blank Rows From Table
#' 
#' Remove rows were all values are \code{NA}.
#' 
#' Some rows can be left blanks in the input table for
#' readability, this function ensures those rows are
#' removed.
#' 
#' @param x A \code{data.frame}.
#'   
#' @return A \code{data.frame} without blank rows.
filter_blanks <- function(x) {
  x[! apply(is.na(x), 1, all), , drop = FALSE]
}

#' File checkers
#' 
#' @param x A file name.
#' @return Whether the file is (respectively)
#'  csv, xlsx, or xls.
#' @rdname file_checkers
#'
is_csv <- function(x) {
  tolower(tools::file_ext(x)) == "csv"
}

#' @rdname file_checkers
is_xlsx <- function(x) {
  tolower(tools::file_ext(x)) == "xlsx"
}

#' @rdname file_checkers
is_xls <- function(x) {
  tolower(tools::file_ext(x)) == "xls"
}
