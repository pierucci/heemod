#' Check Markov Model Transition Matrix
#' 
#' Check whether a matrix fullfills the conditions to be a 
#' transition matrix.
#' 
#' This function is called by [eval_transition()]
#' and should not be used directly.
#' 
#' Checks whether all rows sum to 1 and all probabilities 
#' are between 0 and 1.
#' 
#' @param x a matrix.
#'   
#' @return `NULL`
#'   
#' @keywords internal
check_matrix <- function(x) {
  stopifnot(inherits(x, "array"))
  stopifnot(length(dim(x)) == 3)
  
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    problem_rows <- which(rowSums(x, dims = 2) != 1, arr.ind = TRUE)
    problem_rows <- data.frame(
      cycle = problem_rows[,1], 
      state = get_state_names(x)[problem_rows[,2]])
    problem_rows <- format.data.frame(problem_rows, justify = "left")
    
    stop(sprintf(
      "Not all transition matrix rows sum to 1:\n%s",
      paste(sprintf(
        "cycle: %s, state: %s",
        problem_rows[,1],
        problem_rows[,2]),
        collapse = "\n")
    ))
    
    
  }
  
  if (! all(x >= 0 & x <= 1)) {
    problem <- which(x < 0 | x > 1, arr.ind = TRUE)
    # Use tibble here to avoid potentially confusing warnings about
    # Duplicate rownames
    problem <- tibble::as.tibble(problem)
    names(problem) <- c("cycle", "from", "to")
    states <- get_state_names(x)
    problem$from <- states[problem$from]
    problem$to <- states[problem$to]
    problem <- format.data.frame(problem, justify = "left")
    
    stop(sprintf(
      "Some transition probabilities are outside the interval [0 - 1]:\n%s",
      paste(sprintf(
        "cycle: %s, from: %s, to: %s",
        problem$cycle, problem$from, problem$to),
        collapse = "\n")
    ))
    
  }
}

#' Evaluate Markov Model Transition Matrix
#' 
#' Evaluate a transition matrix using evaluated parameters.
#' 
#' Runs checks on the transition matrix during evaluation.
#' 
#' This functions has been heavily optimized, and thus can
#' be difficult to read. Good luck...
#' 
#' @param x an `uneval_matrix` object.
#' @param parameters an `eval_parameters` object.
#' @param expand A tibble identifying which states
#'   should be expanded.
#'   
#' @return An `eval_matrix` object (actually a list of 
#'   transition matrices, one per cycle).
#'   
#' @keywords internal
eval_transition <- function(x, ...) {
  UseMethod("eval_transition")
}

eval_transition.uneval_matrix <- function(x, parameters, expand = NULL) {
  
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  # Set up time values for which transition probabilities
  # will be evaluated
  time_values <- tibble::tibble(
    model_time = parameters$model_time
  )
  
  # Replace complement with negative pi
  parameters$C <- -pi
  
  # Get number of states + state names
  n_state <- sqrt(length(x))
  state_names <- attr(x, "state_names")
  
  # Fill in expansion table if empty
  if(is.null(expand)) {
    expand <- tibble::tibble(
      .state = state_names,
      .full_state = state_names,
      state_time = 1,
      .expand = F,
      .limit = 1
    )
  }
  
  # Loop through each cell of unexpanded transition matrix and
  # fill out long-form transition table
  # trans_table <- plyr::ldply(seq_len(n_state), function(from) {
  #   plyr::ldply(seq_len(n_state), function(to) {
  #       dplyr::mutate(
  #         time_values,
  #         .from = state_names[from],
  #         .to = state_names[to],
  #         .value = lazyeval::lazy_eval(
  #           x[[(from - 1) * n_state + to]],
  #           data = parameters
  #         )
  #       )
  #   })
  # })
  
  nrow_param = nrow(parameters)
  
  trans_table <- tibble::tibble(
    model_time = rep(parameters$model_time, times = n_state^2),
    state_time = rep(parameters$state_time, times = n_state^2),
    .from = rep(state_names, each = n_state * nrow_param),
    .to = rep(state_names, times = n_state, each = nrow_param),
    .value = unlist(dplyr::mutate_(parameters, .dots = x)[names(x)])
  ) %>%
    dplyr::left_join(
      dplyr::transmute(
        expand,
        .state = .state,
        state_time = state_time,
        .from_e = .full_state,
        .from_lim = .limit
      ),
      by = c(".from" = ".state", "state_time" = "state_time")
    ) %>%
    dplyr::filter(.from_lim >= state_time) %>%
    dplyr::mutate(
      .to_state_time = ifelse(.from == .to, pmin(state_time + 1, .from_lim), 1)
    ) %>%
    dplyr::left_join(
      dplyr::transmute(
        expand,
        .state = .state,
        state_time = state_time,
        .to_e = .full_state
      ),
      by = c(".to" = ".state", ".to_state_time" = "state_time")
    )
  
  # Reshape into 3d matrix and calculate complements
  trans_matrix <- trans_table %>%
    reshape2::acast(
      model_time ~
        factor(.from_e, levels = expand$.full_state) ~
        factor(.to_e, levels = expand$.full_state),
      value.var = ".value",
      fill = 0
    ) %>%
    replace_C
  
  array_res <- structure(
    trans_matrix,
    state_names = expand$.full_state
  )
  
  check_matrix(array_res)
  
  structure(
    split_along_dim(array_res, 1),
    class = c("eval_matrix", "list"),
    state_names = colnames(trans_matrix[1,,]),
    entry = expand$state_time == 1
  )
}

split_along_dim <- function(a, n) {
  # could be maybe optimized?
  setNames(lapply(
    split(a, arrayInd(seq_along(a), dim(a))[, n]),
    array, dim = dim(a)[-n], dimnames(a)[-n]),
    dimnames(a)[[n]])
}

replace_C <- function(x) {
  posC <- x == -pi
  
  if (! all(rowSums(posC, dims = 2) <= 1)) {
    stop("Only one 'C' is allowed per matrix row.")
  }
  
  x[posC] <- 0
  
  valC <- 1 - rowSums(x, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  x[posC] <- valC
  x
}

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}
get_state_names.array <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}
