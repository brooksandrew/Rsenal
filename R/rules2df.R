#' @title Transform association rule results into data.frame
#' 
#' @description Note this function only currently works when the itemsets are of size 1 on the LHS and RHS
#'
#' @param rules list of association rules (S4 arules object).  Output of \code{apriori} function.
#' @param list logical \code{TRUE} or \code{FALSE}.  sets LHS items in a list, rather than one character string
#' @return association rules in a data.frame
#' @export
#' @examples
#' library('arules')
#' data("Adult")
#' ar <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.6, target = "rules", minlen=2))
#' df <- rules2df(ar, list=T)

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
    p <- rules@lhs@data@p
    i <- rules@lhs@data@i+1
    lhsItems <- unlist(rules@lhs@itemInfo@.Data)
    lhsL <- list()
    for(j in 2:length(p)) lhsL[[j-1]] <- lhsItems[i[(p[j-1]+1):(p[j])]]
    df$lhs <- lhsL
    
    p <- rules@rhs@data@p
    i <- rules@rhs@data@i+1
    rhsItems <- unlist(rules@rhs@itemInfo@.Data)
    rhsL <- list()
    for(j in 2:length(p)) rhsL[[j-1]] <- rhsItems[i[(p[j-1]+1):(p[j])]]
    df$rhs <- rhsL
  }
  return(df)
}
