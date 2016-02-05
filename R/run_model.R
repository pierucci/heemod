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
#' @param cost Names or expression to compute cost on the cost-effectiveness
#'   plane.
#' @param effect Names or expression to compute effect on the cost-effectiveness
#'   plane.
#' @param method Counting method.
#'   
#' @return A list of evaluated models with computed values.
#' @export
#' 
#' @example inst/examples/example_run_models.R
#'   
run_models <- function(...,
                       init = c(1L, rep(0L, get_state_number(get_states(list(...)[[1]])) - 1)),
                       cycles = 1,
                       method = c("end", "beginning", "cycle-tree", "half-cycle"),
                       cost, effect, base_model) {
  list_models <- list(...)
  
  method <- match.arg(method)
  
  stopifnot(
    all(unlist(lapply(list_models,
                      function(x) "uneval_model" %in% class(x)))),
    ! missing(cost),
    ! missing(effect)
  )
  
  list_ce <- list(
    lazyeval::lazy(cost, .follow_symbols = FALSE),
    lazyeval::lazy(effect, .follow_symbols = FALSE)
  )
  names(list_ce) <- c(".cost", ".effect")
  ce <- c(
    lazyeval::lazy_dots(),
    list_ce
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
  
  eval_model_list <- lapply(list_models, eval_model, 
                            init = init, 
                            cycles = cycles,
                            method = method)
  
  list_res <- lapply(eval_model_list, get_total_state_values)
  
  for (n in model_names){
    list_res[[n]]$.model_name <- n
  }
  
  res <- Reduce(dplyr::bind_rows, list_res)
  res <- dplyr::mutate_(res, .dots = ce)
  
  if (missing(base_mode)) {
    base_mode <- get_base_model(res)
  }
  
  structure(
    res,
    eval_model_list = eval_model_list,
    uneval_model_list = list_models,
    class = c("eval_model_list", class(res)),
    init = init,
    cycles = cycles,
    method = method,
    ce = ce,
    base_model = base_model
  )
}

#' @export
#' @rdname run_models
run_model <- run_models

#' @export
print.eval_model_list <- function(x, ...) {
  cat(sprintf(
    "%i Markov model%s, run for %i cycle%s.\n\n",
    nrow(x),
    plur(nrow(x)),
    attr(x, "cycles"),
    plur(attr(x, "cycles"))
  ))
  cat(sprintf("Model name%s:\n\n", plur(length(x$.model_name))))
  cat(x$.model_name, sep = "\n")
}

get_total_state_values <- function(x) {
  res <- as.list(colSums((x$values)[- 1]))
  class(res) <- "data.frame"
  attr(res, "row.names") <- c(NA, -1)
  res
}

get_base_model <- function(x, ...) {
  UseMethod("get_base_model")
}

get_base_model.default <- function(x, ...) {
  x$.model_name[which(x$.effect == min(x$.effect))[1]]
}
get_base_model.probabilistic <- function(x, ...) {
  get_base_model(attr(x, "model"))
}

#' Summarise Markov Model Results
#' 
#' @param object Output from \code{\link{run_models}}.
#'   
#' @return A \code{summary_eval_model_list} object.
#' @export
#' 
summary.eval_model_list <- function(object) {
  
  res <- as.data.frame(object)
  
  res <- dplyr::select(res, - .model_name)
  
  rownames(res) <- object$.model_name
  
  structure(
    list(
      res = res,
      cycles = attr(object, "cycles"),
      init = attr(object, "init"),
      count_args = attr(object, "count_args"),
      frontier = get_frontier(object)
    ),
    class = "summary_eval_model_list"
  )
}
if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".model_name"))

#' Compute ICER
#' 
#' Compute ICER for Markov models.
#' 
#' Models are ordered by effectiveness and ICER are computed sequencially.
#' 
#' @param x Result of \code{\link{run_models}} or
#'   \code{\link{run_probabilistic}}.
#'   
#' @return A \code{data.frame} with computed ICER.
#' @export
#' 
compute_icer <- function(x) {
  UseMethod("compute_icer")
}
compute_icer.eval_model_list <- function(x) {
  tab <- x[order(x[[effect]]), ]
  
  for (i in seq_len(nrow(tab))) {
    if ( i == 1) {
      tab$.icer[i] <- -Inf
    } else {
      tab$.icer[i] <- (tab$.cost[i] - tab$.cost[i-1]) /
        (tab$.effect[i] - tab$.effect[i-1])
    }
  }
  tab
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
  print(x$ce)
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


