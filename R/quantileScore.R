
#' @title Quantile scoring function for continuous or integer valued data
#' 
#' @description This function sorts a series from beginning to end.  It uses each observations place in the quantile to assign it a score.
#' useful for transforming variables into the same units (0-1).  Handles NAs 
#' 
#' @param x numeric vector to scale
#' @return a vector of attributes that correspond in order with the nodes in your igraph
#' @export
#' @examples
#' 
#' cbind(quantileScore(mtcars$mpg), mtcars$mpg)
#' cbind(quantileScore(mtcars$cyl), mtcars$cyl)

quantileScore <- function(x) {
  xo <- x
  x <- x[is.na(x)==F]
  xs <- data.frame(x=sort(x), id=1:length(x), stringsAsFactors=F)
  xsd <- xs[duplicated(xs$x)==F,]
  xsd$q <- (xsd$id)/length(x)
  ret <- xsd$q[match(xo, xsd[,1])]
  return(ret)
}


#' @title Quantile scoring function for continuous or integer valued data
#' 
#' @description This function combines the quantile function with the basic feature normalization technique
#' useful for getting a stable normalization
#' 
#' @param x numeric vector to scale
#' @return a vector of attributes that correspond in order with the nodes in your igraph
#' @export
#' @examples
#' 
#' cbind(quantileFeatureScore(sort(mtcars$mpg)), sort(mtcars$mpg))
#' cbind(quantileFeatureScore(mtcars$cyl), mtcars$cyl)

quantileFeatureScore <- function(x, wq=0.5, wf=0.5){
  ret <- wq*quantileScore(x) + wf*(x-min(x))/(max(x)-min(x))
  return(ret)
}
