#' @title Add quality measures to association rules
#' @description Adds measures of rule quality (conviction, hyperConfidence, cosine, chiSquare, coverage, doc, gini, hyperlift) to a set of
#' association rules mined from \code{apriori}.  Usually used before converting ruleset to data.frame and exporting to some sort of text file.
#' @param trans transaction set (s4 class from arules package)
#' @param rules set of rules from \code{apriori}
#' @param include character vector specifying which quality measures to include.  Default is to include everything.
#' @param exclude character vector specifying which quality measures to exclude. Default is to exclude 'improvement' because it seems to be slow.
#' @return ruleset with additional quality measures
#' @import arules
#' @export
#' @examples
#' library('arules')
#' data("Adult")
#' ar <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
#' ar <- addRuleQuality(trans=Adult, rules=ar)
#' df <- Rsenal::rules2df(ar) 

addRuleQuality <- function(trans, rules, include=NULL, exclude='improvement') {
  allMeasures <- c("support", "confidence", "lift", "conviction", "hyperConfidence", "cosine", "chiSquare", "coverage", "doc",    
                   "gini", "hyperLift", "fishersExactTest", "improvement", "leverage", "oddsRatio", "phi", "RLD")
  if(is.null(include)==F) allMeasures <- include
  if(is.null(exclude)==F) allMeasures <- setdiff(allMeasures, exclude)
  for(i in allMeasures) quality(rules)[i] <- interestMeasure(rules, method=i, transactions=trans)
  return(rules)
}
  
  
  
