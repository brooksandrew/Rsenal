
#' @title Inspect missing data in a data.frame
#' @description Currently supports \code{NA} and \code{Inf}
#' @param df data.frame to inspect for missing values
#' @param character string, criteria to search for.  \code{'NA'} or \code{'Inf'}
#' @return summary table
#' @export
#' @examples
#' mtcars2 <- mtcars
#' for(i in 1:ncol(mtcars2)) mtcars2[sample(nrow(mtcars2), sample(1:5,1), replace=T),i] <- NA
#' missdf(mtcars2)
#' }

missdf <- function(df, criteria='NA') {
  n <- nrow(df)
  if(criteria=='NA') a <- sapply(df, function(x) sum(is.na(x)))
  if(criteria=='Inf') a <- sapply(df, function(x) sum(x==Inf))
  miss <- unlist(a)
  missdf <- data.frame(name=names(miss), missing=miss)
  missdf$name <- gsub('.TRUE', '', missdf$name)
  missdf$name <- gsub('.NA', '', missdf$name)
  missdf$nomiss <- unlist(lapply(a, function(x) x['FALSE']))
  
  missdf$missing[is.na(missdf$missing)] <- 0
  missdf$nomiss[is.na(missdf$nomiss)] <- 0
  
  missdf$misspct <- missdf$missing/n
  missdf <- missdf[order(missdf$misspct, decreasing=T),]
  return(missdf)
}
