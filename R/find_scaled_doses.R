#' Title Find the total dose, given a scaling parameter
#' (e.g. find total dose for a 70kg patient for a 2mg/kg drug dosing)
#' 
#' @param doses a vector of doses (e.g. 2, 5, 10)
#' @param dosing_units a vector of units corresponding to the doses
#'   (e.g. "mg/kg", "mg/m2", "mg")
#' @param scaling a vector of scaling numbers (e.g. 70, 85, 1)
#' @param scaling_units a vector of units for the scaling numbers
#'   (e.g. "kg", "m2", "mg")
#' 
#' @return
#' Returns a vector of scale-adjusted doses (e.g. 140mg, 425mg, 10mg)
find_scaled_doses <- function(doses, dosing_units, scaling, scaling_units) {
	
	##argument checks and exception handling
	if(length(doses)!=length(dosing_units))
		stop("length of doses vector does not match that of dosing_units")
	if(length(scaling)!=length(scaling_units))
		stop("length of scaling vector does not match that of scaling_units")
	if(length(doses)!=length(scaling))
		stop("length of doses vectors does not match that of scaling")
	
	#check if units match
	if(!all(scaling_units==sapply(strsplit(as.character(dosing_units),"/"), 
	                              utils::tail, 1)))
		stop("scaling_units does not match denominator of dosing_units")
		
	#return results
	res = doses*scaling
	res
}
