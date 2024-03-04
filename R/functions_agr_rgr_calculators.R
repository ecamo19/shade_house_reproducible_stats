#' @importFrom magrittr %>%

#' @title Relative growth rate calculator   
#' 
#' @author Erick Calderon-Morales 
#' 
#' @description This function is used for calculate plants Relative growth rate
#' in log-log units 
#'
#' @param height column with plants height
#' @param days   column with days. when the the height was made? 
#' 
#' @examples 
#' \dontrun{}
#' @export

# Function RGR -----------------------------------------------------------------

rgr <- function(height, days){

	initial_heigth <- height[days == 0]
	
	# Calculate rgr
	    rgr <- (log(height) - log(initial_heigth)) / days
	    return(rgr)
	     
}

# Function AGR -----------------------------------------------------------------

#' @importFrom magrittr %>%

#' @title Absolute growth rate calculator   
#' 
#' @author Erick Calderon-Morales 
#' 
#' @description This function is used for calculate plants Absolute growth rate
#' in cm-days units 
#'
#' @param height column with plants height
#' @param days   column with days. when the the height was made? 
#' 
#' @examples 
#' \dontrun{}
#' @export

agr <- function(height, days){
	#' This funtion calculates the absulate growth rate 
	#'         (Final Height - Inital height) /
	#'           time final - time initial  
	
	# Select the first mesaure
	initial_heigth <- height[days == 0]
	
	# Calculate rgr
	agr <- (height - initial_heigth) / days
	return(agr)
}

























