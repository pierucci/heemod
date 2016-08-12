#' Title
#'
#' @param x x
#' @param demographics d
#'
#' @return z
#' @export
#'
run_demographics <- function(x, demographics) {
  if (".weigths" %in% names(demographics)) {
    weights <- demographics$.weigths
    demographics <- dplyr::select_(demographics, quote(- .weight))
  } else {
    weights <- rep(1, nrow(demographics))
  }
  
  list_res <- lapply(
    get_model_names(x),
    function(n) eval_model_newdata(x, model = n, newdata = demographics)
  )
  
  list_res <- list()
  total_weights <- sum(weights)
  
  f <- function(x) {
    sum(x * weights / total_weights)
  }
  
  for (model in list_model) {
    total_values <- model %>% 
      dplyr::rowwise() %>% 
      dplyr::do(get_total_state_values(.$.mod)) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise_all(f) %>% 
      dplyr::mutate_(.dots = attr(x, "ce"))
    
    tab_counts <- model %>% 
      dplyr::rowwise() %>% 
      dplyr::do(.counts = get_counts(.$.mod))
    
    counts <- tab_counts$.counts %>% 
      mapply(
        weights,
        FUN = function(x, y) x * y / total_weights,
        SIMPLIFY = FALSE) %>% 
      Reduce(f = "+")
    
    list_res <- c(
      list_res,
      list(total_values, counts)
    )
  }
  res <- Reduce(dplyr::bind_rows, list_res) %>% 
    dplyr::mutate_(res, .dots = ce)
  xxx
}
