#' @title strip_quotes
#'
#' @description strip off the first and last characters of each element of a
#' character vector
#' @param x character vector
#' @export
#' @details I've always found this useful with character vectors containing 
#' quoted elements (e.g. if quoting was inconsistent in the file you read the
#' vector from). 
#' @return \code{x}, but with first and last characters stripped from all 
#' elements
#' @examples
#' gene_names = c('"OCT4"', '"BRCA1"', '"ZZZ3"')
#' genes_noquote = strip_quotes(gene_names)
strip_quotes = function(x){
    substr(x, 2, nchar(x)-1)
}
