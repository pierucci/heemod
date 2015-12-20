#' Run one or more Markov Model
#' 
#' Runs one or more unevaluated Markov Models. When more 
#' than one model is provided, all models should have the 
#' same states and state value names.
#' 
#' 
#' A usual situation where more than one model needs to be 
#' run is when comparing different care startegies.
#' 
#' In order to compute comparisons Markov Models must be 
#' similar (same states and state value names). Thus models 
#' should only differ through parameters, transition matrix 
#' cell values and values attached to states (but not state 
#' value names).
#' 
#' The initial number of individuals in each state and the 
#' number of cycle will be the same for all models.
#' 
#' Internally this function does 2 operations: first
#' evaluating parameters, transition matrix, state values
#' and computing individual counts through
#' \code{\link{eval_model}}; and then using individual
#' counts and evaluated state values to compute values at
#' each cycle through \code{compute_values}.
#' 
#' @param ... One or more \code{uneval_model} object.
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param cycles positive integer. Number of Markov Cycles 
#'   to compute.
#' @param count_args Additional arguments passed as a list 
#'   to \code{compute_counts}.
#' @param newdata data.frame. New parameter values.
#'   
#' @return A list of evaluated models with computed values.
#' @export
#' 
#' @examples 

#' # running a single model
#' 
#' mod1 <-
#'   define_model(
#'     transition_matrix = define_matrix(
#'       .5, .5,
#'       .1, .9
#'     ),
#'     states = define_state_list(
#'       define_state(
#'         cost = 543
#'       ),
#'       define_state(
#'         cost = 432
#'       )
#'     )
#'   )
#' 
#' res <- run_model(
#'   mod1,
#'   init = c(100, 0),
#'   cycles = 2
#' )
#' 
#' # running several models
#' mod2 <-
#'   define_model(
#'     transition_matrix = define_matrix(
#'       .5, .5,
#'       .1, .9
#'     ),
#'     states = define_state_list(
#'       define_state(
#'         cost = 789
#'       ),
#'       define_state(
#'         cost = 456
#'       )
#'     )
#'   )
#' 
#' 
#' res2 <- run_model(
#'   mod1, mod2,
#'   init = c(100, 0),
#'   cycles = 10
#' )
#' 
run_models <- function(...,
                       init = c(1L, rep(0L, get_state_number(get_states(list(...)[[1]])) - 1)),
                       cycles = 1,
                       count_args = NULL,
                       newdata = NULL) {
  list_models <- list(...)
  
  stopifnot(
    all(unlist(lapply(list_models,
                      function(x) "uneval_model" %in% class(x))))
  )
  
  model_names <- names(list_models)
  
  if (is.null(model_names)) {
    message("No named model -> generating names.")
    model_names <- LETTERS[seq_along(list_models)]
    names(list_models) <- model_names
  }
  
  if (any(model_names == "")) {
    warning("Not all models are named -> generating names.")
    model_names <- LETTERS[seq_along(list_models)]
    names(list_models) <- model_names
  }
  
  stopifnot(
    all("uneval_model" %in% unlist(lapply(list_models, class))),
    list_all_same(lapply(list_models,
                         function(x) sort(get_state_names(x)))),
    list_all_same(lapply(list_models,
                         function(x) sort(get_state_value_names(x))))
  )

  stopifnot(
    length(init) == get_state_number(list_models[[1]]),
    all(init >= 0)
  )
  
  if (is.null(names(init)))
    names(init) <- get_state_names(list_models[[1]])
  
  stopifnot(
    all(sort(names(init)) == sort(get_state_names(list_models[[1]])))
  )
  
  structure(
    lapply(list_models, eval_model, 
           init = init, 
           cycles = cycles,
           count_args = count_args),
    uneval_model_list = list_models,
    names = model_names,
    class = "eval_model_list",
    init = init,
    cycles = cycles,
    count_args = if (is.null(count_args)) NA else count_args
  )
}



#' @export
#' @rdname run_models
run_model <- run_models

#' @export
print.eval_model_list <- function(x, ...) {
  cat(sprintf(
    "%i Markov model%s, run for %i cycle%s.\n\n",
    length(x),
    plur(length(x)),
    attr(x, "cycles"),
    plur(attr(x, "cycles"))
  ))
  cat("Model names:\n\n")
  cat(names(x), sep = "\n")
}

#' @export
summary.eval_model_list <- function(object, ...) {
  res <- unlist(
    lapply(
      object,
      function(y) colSums((y$values)[- 1])
    )
  )
  
  res <- matrix(
    res,
    nrow = length(object),
    byrow = TRUE,
    dimnames = list(
      names(object),
      get_state_value_names(object[[1]])
    )
  )
  
  structure(
    list(res = res,
         cycles = attr(object, "cycles"),
         init = attr(object, "init"),
         count_args = attr(object, "count_args")),
    class = "summary_eval_model_list"
  )
}

#' @export
print.summary_eval_model_list <- function(x, ...) {
  cat(sprintf(
    "%i Markov model%s run for %i cycle%s.\n\n",
    nrow(x$res),
    plur(nrow(x$res)),
    x$cycles,
    plur(x$cycles)
  ))
  cat("Initial states:\n\n")
  print(matrix(
    x$init,
    dimnames = list(
      names(x$init),
      "N"
    )
  ))
  print(x$res)
}

#' @export
print.eval_model <- function(x, width = Inf, ...) {
  cat(sprintf("A Markov model, run for %i cycle%s.\n\n",
              attr(x, "cycles"),
              plur(attr(x, "cycles"))
  ))
  
  cat("Initial individual counts:\n\n")
  print(data.frame(
    "State names" = names(get_counts(x)),
    "Initial counts" = attr(x, "init"),
    check.names = FALSE
  ))
  
  cat("Individual counts per cycle:\n\n")
  print(get_counts(x), width = width, ...)
  
  cat("State values per cycle:\n\n")
  print(get_state_values(x), width = width, ...)
}


