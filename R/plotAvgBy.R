#' @title Plot average of an indicator over bins/categories of another another continuous variable
#' 
#' @description Useful for assessing the relationship between two variables of interest.  There are cases, especially when 
#' outliers are involved, or many obs, that a scatterplot can be difficult to read.  This function bins up one of the continuous
#' variables that would be used in a scatterplot and calculates the mean (or other function) of the continuous variable over 
#' a range (discretized into categories) of the second continuous variable.  It uses an equal depth binning algorithm
#' to compute these bins on the \code{by} variable.  
#' 
#' It can be used to assess the average of a binary target variable/prediction
#' over a range of levels of a continuous or categorical variable.
#' 
#' @param indv vector.  This the variable whose mean will be calculated over the categories of the \code{byv} vector binned up
#' @param byv vector. This is variable to be binned up, by which the \code{indv} variable will be averaged
#' @param nbins numeric. Number of bins to create when discretizing \code{byv}.  Passed to \code{depthbin} function.
#' @param data logical. TRUE returns the aggregated data.table. FALSE returns noting.  TRUE is default.
#' @param plotNbin logical.  TRUE plots the count of obs in each bin on top of each bar.  TRUE is default.
#' @param ... additional barplot arguments.

#' @return prints a barplot unless data==T, in which case the aggregated data.table is returned
#' @import data.table
#' @export
#' @examples
#' 
#' plotAvgBy(mtcars[,'mpg'], mtcars[,'drat'], nbins=8)
#' plotAvgBy(mtcars[,'mpg'], mtcars[,'drat'], nbins=5, plotNbin=F)
#' plotAvgBy(mtcars[,'mpg'], mtcars[,'drat'], nbins=5, plotNbin=F, data=T)
#'
#' ## Example with missing data
#' df <- mtcars
#' df$mpg[sample(1:nrow(mtcars), 5)] <- NA
#' df$drat[sample(1:nrow(mtcars), 5)] <- NA
#' 
#' plotAvgBy(df[,'mpg'], df[,'drat'], nbins=5, plotNbin=F, data=T)

plotAvgBy <- function(indv, byv, nbins=5, data=F, plotNbin=T, ...){
  dt <- data.table(indv, byv)
  warning(sprintf('removing %s observations where indv is NA', sum(is.na(dt$indv))))
  dt <- dt[is.na(indv)==F,]
  tmp <- dt[, .(N=.N, hits=sum(as.numeric(as.character(indv)))), keyby=depthbin(byv, nbins=nbins)][,hitrate:=hits/N]
  plt <- tmp[,barplot(hitrate, names=depthbin,las=2, col='navy', ...)]
  if(plotNbin==T) text(plt, tmp$hitrate-abs(diff(range(tmp$hitrate)))/20, labels=tmp$N, col='white')
  if(data==T) return(tmp)
}



