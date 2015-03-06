#' @title Transform association rule results into data.frame
#' 
#' @description Note this function only currently works when the itemsets are of size 1 on the LHS and RHS
#'
#' @param rules list of association rules (S4 arules object).  Output of \code{apriori} function.
#' @param list logical \code{TRUE} or \code{FALSE}.  sets LHS items in a list, rather than one character string
#' @return association rules in a data.frame
#' @export
#' @examples
#' require('arules')
#' data("Adult")
#' ar <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
#' df<-rules2df(ar)

rules2df <- function(rules, list=F){  
  df <- as(rules, 'data.frame')
  df[,1] <- as.character(df[,1])
  df$lhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][1])
  df$rhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][2])
  df$lhs <- gsub(pattern='\\{', replacement='', x=df$lhs)
  df$lhs <- gsub(pattern='}', replacement='', x=df$lhs)
  df$rhs <- gsub(pattern='\\{', replacement='', x=df$rhs)
  df$rhs <- gsub(pattern='}', replacement='', x=df$rhs)
  
  if(list==T){
    df$lhs <- strsplit(as.character(df$lhs), split=',')
    df$rhs <- strsplit(as.character(df$rhs), split=',')
  }
  return(df)
}
