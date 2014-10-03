#' @title Transform association rule results into data.frame
#' 
#' @description Note this function only currently works when the itemsets are of size 1 on the LHS and RHS
#'
#' @param rules list of association rules (S4 arules object).  Output of \code{apriori} function.
#' @return association rules in a data.frame
#' @export
#' @examples
#' require('arules')
#' data("Adult")
#' ar <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
#' df<-rules2df(ar)

rules2df <- function(rules){
  if(length(setdiff(arules::size(items(rules)), 2)) > 0) print('Warning!!! Picked up some rules with more than item on LHS or RHS. Need 1 item on LHS and RHS.')
  
  df <- as(rules, 'data.frame')
  df[,1] <- as.character(df[,1])
  df$lhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][1])
  df$rhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][2])
  
  df$lhs <- gsub(pattern='\\{', replacement='', x=df$lhs)
  df$lhs <- gsub(pattern='}', replacement='', x=df$lhs)
  df$rhs <- gsub(pattern='\\{', replacement='', x=df$rhs)
  df$rhs <- gsub(pattern='}', replacement='', x=df$rhs)
  
  return(df)
}
