#' @title decimalplaces
#' @description How many decimal places is a number rounded to?
#' 
#' @param x number of vector of numbers
#' 
#' @return Number of decimal places to which the number is rounded. 
#' 
#' @examples
#' decimalplaces(1.2) #1
#' decimalplaces(702) #0
#' decimalplaces(3.14159) #5
#'
#' @export
decimalplaces <- function(x){
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}