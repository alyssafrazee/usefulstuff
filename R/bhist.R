#' @title overlay histograms!
#'
#' @description base graphics implementation of overlaid histograms
#' @param z numeric, vector of values for desired histogram
#' @param add if TRUE, overlay histogram on current graphics device
#' @param breaks see \code{breaks} in \code{hist()}
#' @param col line color for histogram's bins
#' @param fill fill color for histogram
#' @export
bhist = function(z, add=FALSE, breaks=20, col="black", fill="orange", 
    alpha=0.5, xlim=c(-0.01,1.01), ylim=NULL,...){
    h = hist(z, breaks=breaks, plot=FALSE)
    if(is.null(ylim)) ylim = c(0,max(h$counts))
    if(!add){plot(0, 0, type="n", xlim=xlim, ylim=ylim,...)}
    hb = rep(h$breaks, each=2)[-1]
    x = c(hb,0)
    y = c(rep(h$counts, each=2),0,0)
    fill=makeTransparent(fill, alpha=alpha)
    polygon(x, y, border=col, col=fill, lwd=1)
    for(i in seq_along(y)){
        lines(rep(x[i], 2), c(0, y[i]), col=col)
    }
}
  
## Transparentize colors borrowed from 
## http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color

#' @title get transparent equivalents of R colors 
#'
#' @param ... any number of colors, in any of R's acceptable forms (hex, character, number, etc)
#' @param alpha how transparent do you want it? must be between 0 (totally transparent) and 1 
#'   (totally opaque)
#' @export
#' @return vector of transparentized colors, same length as \code{...}
makeTransparent = function(..., alpha=0.5) {
    if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
    alpha = floor(255*alpha)  
    newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
    .makeTransparent = function(col, alpha) {
        rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
    }
    newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
    return(newColor) 
}