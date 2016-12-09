#' Determines the set of model scenarios (strategies) that are on the 
#' cost effective frontier. 
#'
#' This code impliments the algorithm 
#' described in "Suen S -c., Goldhaber-Fiebert JD. "An Efficient, 
#' Noniterative Method of Identifying the Cost-Effectiveness Frontier". 
#' Med. Decis. Mak. 2016;36:132-6.
#'
#' The algorithm is based on finding the maximum net monitary 
#' benefit (NMB) as a function of willingness to pay (WTP).  The 
#' minimal grid of WTPs to guarantee all frontier strategies are 
#' found is the set of all positive pairwise icers. 
#'
#' @param x An \code{eval_strategy_list} object.
#' @param return.all Return frontier, weak and strongly dominated strategies. 
#'   Default is FALSE
#' @param return.icers Return the ICERs for the frontier strategies. 
#'   Default is FALSE
#'   
#' @return If return.icers and return.all are FALSE then returns a vector of 
#'   strategy names on the efficiency.  If return.icers is true then returns a 
#'   list with \code{list("frontier" = vector_of_strategy_names, "icers" = vector_of_icers)}.
#'   If return.all then the result is a list with \code{list("frontier" = [vector|list]_of_frontier_strategies, "weakly_dominated" = list_of_weakly_dominated_strategies,"dominated" = list_of_strongly_dominated_strategies)}
#'   frontier.
#'   
#' @keywords internal


get_frontier <- function(x, return.all = FALSE, return.icers = FALSE){
  x <- x[order(x$.effect),]
  if(length(x$.strategy_names)<=1){
    stop("Must provide more than one strategy to calculate frontier.")
  }

  icers <- calculate_pairwise_icers(x)
  
  # find all unique finite positive pairwise icers
  # The basis should not include 0 or infinity.
  wtps <- icers[icers>0 & !is.na(icers) & is.finite(icers)]
  
  # If there are no positive wtps then include 0
  # This handles the case when all strategies are 
  # domintated by next neighbor
  if(length(wtps)==0){wtps<-c(0)}
 
   # order them from smallest to largest
  wtps <- sort(unique(wtps))
  
  # calculate the NMP vector for each WTP
  nmps <- x$.effect %*% t(wtps) - x$.cost
  
  # Find column name (strategy) for each WTP 
  # that contains the maximum NMB.
  # The unique elements of this list 
  # are the cost-effective frontier strategies.
  # TODO there's probably a better way to do this. . . 
  # . . .i need all indices matching max values in nmps columns
  maxVals<-c(apply(nmps,2,max))
  frontierIndices<-which(apply(nmps,1,function(y) any(y==maxVals)))
  frontier <- unique(x$.strategy_names[frontierIndices])
  #frontier <- unique(x$.strategy_names[c(max.col(t(nmps),ties.method="first"))])

  # If requested include the frontier strategy icers
  # Note,  "base" strategy has no icer 
  if( return.icers ){
    indices <- which(x$.strategy_names %in% frontier, arr.ind=TRUE)
    ic <- c("-")
    n <- 1
    while(n < length(indices)){
      ic <- append(ic, round(icers[indices[n],indices[n <- n+1]], digits = 2))
    }
    result <- list("frontier" = frontier, "icers" = ic)
  } 
  
  if( return.all ){
    # We can also find the strongly dominated strategies.  
    # These are strategies that have at least one negative pairwise icer
    strong <- x$.strategy_names[c(unique(which(icers < 0 & !is.na(icers), arr.ind=TRUE)[,1]))]
    
    # once we have the frontier and strongly dominated strategies, the
    # remaining strategies are weakly dominated. 
    weak <- x$.strategy_names[!x$.strategy_names %in% c(strong,frontier)]
    
    if( return.icers ){ 
      # TODO  I'm sure there's a better way to do this. . .
      result <- list("frontier" = result$frontier, "icers" = result$icers,  "weakly_dominated" = weak,"dominated" = strong)
    } else {
      # group the results into a list and return
      result <- list("frontier" = frontier, "weakly_dominated" = weak,"dominated" = strong)
    }
  } else {
    result <- frontier
  }
  result
}

# Calculates all unique pairwise icers resulting in an 
# upper triangular matrix
calculate_pairwise_icers <- function(x){
  x_dim   <- length(x$.cost)
  result  <- matrix(data = 0, nrow = x_dim, ncol = x_dim)
  
  # calculate all pairwise icers
  for(i in 1:(x_dim-1)){
    for(j in (i+1):x_dim){
      deffect <- x$.effect[j] - x$.effect[i]
      dcost   <- x$.cost[j] - x$.cost[i]
      if(deffect == 0){
        result[i,j] <- Inf
      } else {
        result[i,j] <- dcost/deffect
      }
    }
  }
  result
}
