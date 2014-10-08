#' @title ss
#'
#' @description extract a specific slot from each element of a character vector
#' @param x character vector
#' @param pattern slot separator
#' @param slot slot index to get (default 1)
#' @param ... extra arguments for \code{strsplit}
#' @export
#' @return vector, same length as \code{x}, with elements consisting of only
#' the \code{slot}th slot of the corresponding elements in \code{x}.
#' @examples
#'   fullnames = c('Hilary Clinton', 'Florence Nightingale', 'Rosalind Franklin')
#'   firstnames = ss(fullnames, ' ', slot=1)
#'   lastnames = ss(fullnames, ' ', slot=2)
#'
ss = function(x, pattern, slot=1, ...){
    sapply(strsplit(x, pattern, ...), "[", slot)  
}
