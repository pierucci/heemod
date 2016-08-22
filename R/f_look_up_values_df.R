#' Look up values in a data frame
#' @description Read in values from a data frame, typically one imported
#'   using \code{\link{load_df_from_files}}.	For example, could be used
#'   for analysis-specific mortality rates.
#' @param val_source A data frame with attributes having various indices
#' 		and a value (or several values) to be looked up 
#' @param ... Collection of indices to look-up for each attribute.
#' @param numeric_cols names specifying which attributes should be treated as numeric.
#' @param output_col  If specified, the name of the column to get for output.  
#' 
#' @details Columns designated as numeric by \code{numeric_cols} get looked up using
#'    ranges of values rather than by single values. In this case, each attribute index
#' 		is the minimum value for a bin that ends with the next index.
#' 		
#' 		The order of the \code{...} arguments need not match the order 
#' 		of columns in the data frame. 
#'  		
#' 		If specified, \code{output_col} must be the name of a single column to get for 
#' 		output.   If not specified, it
#' 	  will be set to the difference between names(val_source) and the names of index
#' 	  columns, and an error will be thrown if that difference contains more than one column.
#' 		
#' @return A single value or vector of values (depending on how many attribute sets
#' 		were provided).
#' @export
#' @example inst/examples/example_f_look_up_values_df.R

f_look_up_values_df <- function(val_source, ..., numeric_cols = NULL,
                                output_col = NULL) {
	
	specs <- list(...)
	if(length(unique(sapply(specs, length))) > 1)
	     stop("all index collections for attributes must have the same length")

	specs <- data.frame(..., stringsAsFactors = FALSE)
	
	## argument checks and exception handling
	if(is.null(dim(val_source)) | !is.data.frame(val_source))
		stop("val_source must be a non-empty data frame")
	if(any(is.null(names(val_source))))
		stop("data frame must have a name for each column")
	if(!all(names(specs) %in% names(val_source)))
		stop("each attribute must have a corresponding column in val_source")
	
	## extract relevant variable names
	all_names = names(val_source)
	var_names = names(specs)
	non_numeric_cols = setdiff(var_names,numeric_cols)
	if(is.null(output_col)){
	  output_col = setdiff(all_names,var_names)
	}
	
	## make sure exactly one, valid, output_col
	stopifnot(length(output_col) == 1)
	if(!(output_col %in% names(val_source)))
	  stop(paste("output column", output_col, "is not a column name of the data frame"))
	
	## for dimnames meant to be treated as numeric, convert to numeric
	if(!is.null(numeric_cols)) {
		numeric_dims <- which(names(val_source) %in% numeric_cols)
		for(i in numeric_dims) {
			val_source[,i] <- as.numeric(val_source[,i])
		}
	}
	
    ## columns not being treated as numeric require equality to match
	  non_num_equal <- lapply(non_numeric_cols, function(j){
	      
	    valid_val <- specs[,j] %in% val_source[,j]
	    if(!all(valid_val)){
	      this_warning <- 
	        paste("Unmatched value for non-numeric attribute ",
	              j, ": ", 
	              paste(unique(specs[!valid_val,j]), 
	                    collapse = ", "), sep = "")
	      warning(this_warning)
	    }
	    outer(specs[, j], val_source[,j], "==")
	  })
    ## for numeric columns, a numeric value
	  ##   matches the smallest "bin" it is greater than or equal to
	  num_in_bounds <- lapply(numeric_cols, function(j){
	    gte <- outer(specs[, j], val_source[,j], ">=")
	    ## figure out the highest value each spec is >=
         highest <- apply(gte, 1, function(x){max(val_source[x,j])})
         ## and match only those elements
         outer(highest, val_source[,j], "==")
	  })
    ## put the results together and find where we match all criteria	  
	   matching_array <-
      array(unlist(c(non_num_equal, num_in_bounds)), 
            dim = c(nrow(specs), nrow(val_source), 
                    length(non_numeric_cols) + length(numeric_cols)))

	  matching_sums <- rowSums(matching_array, dims = 2)
	 
     inds <- apply(matching_sums == length(non_numeric_cols) + length(numeric_cols), 1, 
                   function(x){y <- which(x); if(length(y) > 0) y else nrow(val_source)+1})
	  return(val_source[inds, output_col])
}
