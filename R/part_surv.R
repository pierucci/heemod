allowed_fit_distributions <- c("exp", "weibull", "lnorm", "llogis", 
                               "gamma", "gompertz", "gengamma")

#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param pfs,os Either results from 
#'   [flexsurv::flexsurvreg()] or 
#'   [define_survival()].
#' @param state_names named character vector, length 3 or 4.
#'   State names for progression-free state, progression, 
#'   (optionally terminal) and death respectively. Elements 
#'   should be named `"progression_free"`, 
#'   `"progression"`, (optionally `"terminal"`), 
#'   and `"death"`. See examples.
#' @param terminal_state Should a terminal state be 
#'   included? Only used when state names are not provided.
#' @param cycle_length The value of a Markov cycle in
#'   absolute time units.
#'   
#' @return A `part_surv` object.
#' @export
#' 
#' @examples
#' dist_pfs <- define_survival("exp", rate = 1)
#' dist_os <- define_survival("exp", rate = .5)
#' 
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   state_names = c(
#'     progression_free = "A",
#'     progression = "B",
#'     terminal = "C",
#'     death = "D"
#'   )
#' )
#' # identical to:
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os,
#'   terminal_state = TRUE
#' )
#' 
define_part_surv <- function(pfs, os, state_names,
                             terminal_state = FALSE,
                             cycle_length = 1) {
  
  if (missing(state_names)) {
    message("No named state -> generating names.")
    
    if (terminal_state) {
      state_names <- LETTERS[seq_len(4)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "terminal",
        "death"
      )
    } else {
      state_names <- LETTERS[seq_len(3)]
      names(state_names) <- c(
        "progression_free",
        "progression",
        "death"
      )
    }
  }
  
  if (is.null(names(state_names))) {
    if (terminal_state) {
      warning("Argument 'terminal_state' ignored when state names are given.")
    }
    message("Trying to guess PFS model from state names...")
    state_names <- guess_part_surv_state_names(state_names)
  }
  
  define_part_surv_(
    pfs = lazyeval::lazy_(substitute(pfs), env = parent.frame()),
    os = lazyeval::lazy_(substitute(os), env = parent.frame()),
    state_names = state_names,
    cycle_length = cycle_length)
  }




#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(pfs, os, state_names,
                              cycle_length = 1) {

  if (is.null(names(state_names))) {
    state_names <- guess_part_surv_state_names(state_names)
  }
  
  stopifnot(
    inherits(pfs, "lazy"),
    inherits(os, "lazy"),
    
    length(state_names) %in% 3:4,
    ! is.null(names(state_names)),
    all(names(state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(state_names))),
    length(cycle_length) %in% 1:2,
    all(cycle_length > 0)
  )
  
  if (length(cycle_length) == 1) {
    cycle_length <- rep(cycle_length, 2)
  }
  
  res <- list(
    pfs = pfs,
    os = os,
    state_names = state_names,
    cycle_length = cycle_length
  )
  
  structure(
    res,
    class = "part_surv"
  )
}

#' Convert saved fits to partitioned survival objects
#'
#' @param surv_inputs a list of matrices of `flexsurvreg` objects,
#'  for example the first element of the output of `survival_from_data`.
#' @param state_names names of states of the model
#'
#' @details  surv_inputs is a tibble with columns
#'   type (PFS or OS, not case sensitive), treatment, 
#'   set_name (for data subsets),
#'   dist (for survival distribution assumptions),
#'   fit (for the fitted survival object) and set_def
#'   (how the subset of data was defined, just to keep it around)

#' @return a tibble of partitioned survival objects, similar to the
#'   original tibble of survival fits, with all the columns
#'   except type and fit, and a new column part_surv.
#' @export
#'
part_survs_from_surv_inputs <- function(surv_inputs, state_names) {
  
  surv_inputs %>%
    dplyr::group_by_(
      ~ treatment, ~ set_name, ~ dist, ~ set_def) %>%
    dplyr::do_(
      part_surv = ~ make_part_surv_from_small_tibble(
        ., state_names = state_names))
}

get_state_names.part_surv <- function(x) {
  x$state_names
}

eval_transition.part_surv <- function(x, parameters) {
  
  time_ <- c(0, parameters$markov_cycle)
  
  pfs_dist <- lazyeval::lazy_eval(
    x$pfs, 
    data = dplyr::slice(parameters, 1)
  )
  
  pfs_surv <- compute_surv(
    pfs_dist,
    time = time_,
    cycle_length = x$cycle_length[1],
    type = "surv"
  )
  
  os_dist <- lazyeval::lazy_eval(
    x$os, 
    data = dplyr::slice(parameters, 1)
  )
  
  os_surv <- compute_surv(
    os_dist,
    time = time_,
    cycle_length = x$cycle_length[2],
    type = "surv"
  )
  
  structure(
    list(
      pfs_surv = pfs_surv,
      os_surv = os_surv,
      state_names = x$state_names
    ),
    class = "eval_part_surv")
}

compute_counts.eval_part_surv <- function(x, init,
                                          inflow) {
  
  stopifnot(
    length(x$state_names) %in% 3:4,
    all(names(x$state_names) %in% c(
      "progression_free",
      "progression",
      "terminal",
      "death"
    )),
    ! any(duplicated(names(x$state_names))),
    length(init) == length(x$state_names),
    all(init[-1] == 0)
  )
  
  res <- tibble::tibble(
    progression_free = x$pfs_surv,
    progression      = x$os_surv - x$pfs_surv, 
    death            = 1 - x$os_surv
  )
  
  if (length(x$state_names) == 4) {
    res$terminal <- diff(c(0, res$death))
    res$death <- c(0, res$death[-nrow(res)])
  }
  
  if (any(res < 0)) {
    neg_cycles <- which(res < 0, arr.ind = TRUE)[, 1]
    stop("Negative counts in partitioned survival model, at cycle",
         plur(length(neg_cycles)),
         ": ",
         paste(neg_cycles, collapse = ", "))
  }
  
  res <- res * sum(init)
  
  names(res) <- x$state_names[names(res)]
  res <- res[x$state_names]
  
  structure(res, class = c("cycle_counts", class(res)))
  list(counts = res) #cannot compute count diff matrix in partitionned survival
}

guess_part_surv_state_names <- function(state_names) {
  death_state <- c(
    grep("death", state_names, ignore.case = TRUE),
    grep("dead", state_names, ignore.case = TRUE)
  )
  progfree_state <- grep("free", state_names, ignore.case = TRUE)
  progressive_state <- setdiff(
    grep("progress", state_names, ignore.case = TRUE),
    progfree_state)
  terminal_state <- grep("terminal", state_names, ignore.case = TRUE)
  
  if (length(death_state) != 1) {
    stop("State name representing death must contain ",
         "'death' or 'dead' (case insensitive).")
  }
  
  if (length(progfree_state) != 1) {
    stop("Progression free state (only) must have 'free' in its name.")
  }
  
  if (length(progressive_state) != 1) {
    stop("Progression state must have 'progress' ",
         "but not 'free', in its name.")
  }
  
  if (length(state_names) == 3) {
    names(state_names) <- c(
      "progression_free",
      "progression",
      "death")[c(
        progfree_state,
        progressive_state,
        death_state)]
    
  } else if (length(state_names) == 4) {
    if (length(terminal_state) == 0) {
      stop(
        "If there are 4 states, a state must be called 'terminal' ",
        "(not case sensitive)."
      )
    }
    
    names(state_names) <- c(
      "progression_free",
      "progression",
      "terminal",
      "death")[c(
        progfree_state,
        progressive_state,
        terminal_state,
        death_state)]
    
  } else {
    stop("There must be 3 or 4 states.")
  }
  
  message(sprintf(
    "Successfully guessed PFS from state names:\n%s",
    paste(paste0(
      "  ", names(state_names), " = ", state_names),
      collapse = "\n")
  ))
  
  state_names
}

#' construct a survival object from tabular specification
#'
#' @param surv_def a data frame with the specification.  See details.
#' @param ref data frame with information about the fits.
#' @param env an environment
#' @param state_names names of the model states
#' @details  This function is meant to be used only from within
#'   tabular_input.R.   It won't work well otherwise, in that
#'   the environment is unlikely to have what you need.
#' 
#' columns of surv_def:  .strategy, .type, .subset, dist, until
#'   where dist can be either the name of a distribution
#'   along with parameters, or a reference to a fit
#'   for example:  fit('exp') or exp(rate = 0.5)

#' @return a list with one element for each strategy.   Each element
#'   is in turn a `part_surv` object, a list with two elements, 
#'   pfs and os.   And those
#'   elements are survival objects of various kinds, with the
#'   commonality that they can be used in [compute_surv()].
construct_part_surv_tib <-
  function(surv_def, ref,
           state_names,
           env = new.env()) {
    surv_def_names <- c(".strategy", ".type", "dist")
    if (!all(present_names <- surv_def_names %in% names(surv_def))) {
      stop("missing required names in 'surv_def': ",
           paste(surv_def_names[!present_names], collapse = ", "))
    }
    #
    if (!(".subset" %in% names(surv_def))) {
      surv_def$.subset <- "all"
      message("no '.subset' column; defaulting to subset 'all'")
    }
    surv_def <- tibble::as_tibble(surv_def)
    
    ## we handle directly defined distributions
    ##   (those defined with define_survival())
    ##   separately from fits
    with_direct_dist <- dplyr::filter(surv_def, grepl("^define_survival", dist))
    should_be_fits <- dplyr::filter(surv_def, !grepl("^define_survival", dist))
    
    should_be_fits_3 <- should_be_fits
    if (nrow(should_be_fits) > 0) {
      surv_ref_full_file <- ref[ref$data == "tm", "full_file"]
      surv_ref_file <- ref[ref$data == "tm", "file"]
      ## slightly roundabout way of getting the base location back
      location <- gsub(paste0(surv_ref_file, "$"),
                       "",
                       surv_ref_full_file)
      
      survival_specs <-
        read_file(ref[ref$data == "tm", "full_file"])
      ## as compared to previous version, now we are
      ##  going directly to loading the files
      fit_tibble <-
        load_surv_models(location,
                         check_survival_specs(survival_specs),
                         env)[[1]]
      should_be_fits_3 <- join_fits_to_def(should_be_fits,
                                           fit_tibble)
  }
    direct_dist_def_3 <- with_direct_dist
    direct_dist_def_3$fit <- direct_dist_def_3$dist
    direct_dist_def_3$set_def <- rep(NA, nrow(direct_dist_def_3))
    if(!("time_subtract" %in% names(direct_dist_def_3)))
      direct_dist_def_3$time_subtract <- rep(NA, nrow(direct_dist_def_3))
    
    ## and now we can rejoin them and continue
    surv_def_4 <-
      rbind(should_be_fits_3, direct_dist_def_3) %>%
      dplyr::group_by_(~ .strategy, ~ .type) %>%
      dplyr::do_(fit = ~ join_fits_across_time(.)) %>%
      dplyr::ungroup()
    surv_def_5 <-
      surv_def_4 %>%
      dplyr::group_by(.strategy) %>%
      dplyr::rename(type = .type) %>%
      dplyr::do(part_surv = make_part_surv_from_small_tibble(.,
                                                                state_names = state_names))
    surv_def_5
  }

join_fits_across_time <- function(this_part){
if(nrow(this_part) == 1) return(this_part$fit[[1]])
  if ("until" %in% names(this_part)) {
    this_part <-
      dplyr::arrange(this_part, until)
    
    join_(dots = this_part$fit, 
             at= this_part$until[!is.na(this_part$until)])
    
  }  
  else{
      print(this_part)
      stop(
        "can't have more than one distribution for a single ",
        "strategy and type unless 'until' is also specified"
      )
    }
}

make_part_surv_from_small_tibble <- function(st, state_names){
  pfs_row <- grep("pfs", st$type, ignore.case = TRUE)
  os_row <- grep("os", st$type, ignore.case = TRUE)
  stopifnot(length(pfs_row) == 1,
            length(os_row) == 1
  )
  define_part_surv(pfs = st[[pfs_row, "fit"]],
                   os = st[[os_row, "fit"]],
                   state_names = state_names)
}


join_fits_to_def <- function(surv_def, fit_tibble) {
  surv_def_names <- c(".strategy", ".type", "dist")
  if (!all(present_names <- surv_def_names %in% names(surv_def))) {
    stop("missing required names in 'surv_def': ",
         paste(surv_def_names[!present_names], collapse = ", "))
  }
  
  fit_tibble_names <- c("treatment", "type", "dist", "set_name", "fit")
  if(!all(present_names <- fit_tibble_names %in% names(fit_tibble))){
    stop("missing required names in 'fit_tibble': ",
         paste(fit_tibble_names[!present_names], collapse = ", "))
  }
  
    fit_tibble <-
    dplyr::mutate(fit_tibble, type = toupper(type))
  
  ## reduce fit expressions to distribution names
  should_be_fits_2 <- surv_def %>%
    dplyr::mutate(
      dist = gsub("fit\\((.*)\\)", "\\1", dist) %>%
        gsub("'", "", .) %>%
        gsub('"', '', .),
      .type = toupper(.type)
    )
  ok_dist_names <-
    should_be_fits_2$dist %in% c(allowed_fit_distributions, "km")
  wrong_dist_names <-
    unique(should_be_fits_2$dist[!ok_dist_names])
  if(any(!ok_dist_names))
    stop("disallowed distribution name", 
         plur(length(wrong_dist_names)),
         " in use_fits file: ",
         paste(wrong_dist_names, collapse = ", "),
         "\n",
         "allowed distributions: ",
         paste(allowed_fit_distributions, collapse = ", ")
    )
  
  ok_subset_names <-
    should_be_fits_2$.subset %in% fit_tibble$set_name
  missing_set_names <- 
    unique(should_be_fits_2$.subset[!ok_subset_names])
  missing_for_message <- paste0("subset name", 
                        plur(length(missing_set_names)))
  if(length(missing_set_names) > 0)
    stop(missing_for_message,
         " ",
         paste(missing_set_names, collapse = ", "),
         " from specification of fits to use not present in fits;\n",
         " could ", missing_for_message, " be misspelled, ",
         "or might the fits need to be rerun?"
         )
  
  ## and join in the fits and subset definitions
  should_be_fits_3 <- should_be_fits_2 %>%
    dplyr::left_join(
      fit_tibble,
      by = c(
        ".strategy" = "treatment",
        ".type" = "type",
        "dist" = "dist",
        ".subset" = "set_name"
      )
    )
  problem <- sapply(should_be_fits_3$fit, is.null) |
    is.na(should_be_fits_3$fit)
  if (any(problem)) {
    print(surv_def[problem, ])
    stop("fit not found for line",
         plur(sum(problem)),
         " ",
         paste(which(problem), collapse = ", "),
         " (shown above);\n",
         "check that subsets are assigned to the proper ",
         "survival type (PFS or OS), and that fits exist ",
         "(are not NULL)"
    )
  }
  should_be_fits_3
}
