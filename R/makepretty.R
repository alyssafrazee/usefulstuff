#' @title Make a string pretty!
#'
#' @description strips newlines and blocks of 4 spaces from strings. I use this when I want to have
#'   newlines and 4-space tabs in strings *in my script*, but would also like to print them to 
#'   the console in a sensible way. 
#' @param x string (i.e., character vector of length 1)
#' @export
#' @return string that is the same as \code{x} but with all newline (\code{'\n'}) and 4-space blocks
#'   (\code{'    '}) replaced with the empty string.
#' @examples \dontrun{
#' msg = 'Hello, I will type a message to you
#'     like this!'
#' message(msg)
#' message(makepretty(msg))
#'}
makepretty = function(x){
    stopifnot(length(x) == 1 & class(x) == 'character')
    msg = gsub('\n', '', x)
    msg = gsub('    ', '', msg)
    msg
}
