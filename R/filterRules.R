#' @title Post process association rules
#' 
#' @description This function filters rules that have already been mined.  
#'
#' @param rules list of association rules (S4 arules object).  Output of \code{apriori} function.
#' @param oneSideOnly list of character vectors.  Each character vector of the list contains variables that are likely very similar
#' and will generate uninteresting rules.  So the filtering algorithm will prune rules where variables within this list appear on
#' both the RHS & LHS
#' @return association rules 
#' @import arules
#' @export
#' @examples
#' 
#' library('arules')
#' data('Adult')
#' rules <- apriori(Adult, parameter = list(supp=0.01, conf=0.9, target = "rules"))
#' oneSideOnly <- list(c('age', 'workclass', 'education'), c('marital-status', 'occupation', 'race'))
#' f2 <- filterRules(rules, oneSideOnly=oneSideOnly)
#' length(rules)
#' length(f2)

filterRules <- function(rules, oneSideOnly=oneSideOnly) {
  frules <- rules
  for(i in 1:length(oneSideOnly)) {
    cond <- paste0(unlist(oneSideOnly[i]), sep='=')
    comb <- expand.grid(cond, cond, stringsAsFactors=F)
    for(j in 1:nrow(comb)){
      frules <- subset(frules, subset=(!(lhs %pin% comb[j,1] & (rhs %pin% comb[j,2]))))
    }
  }
  return(frules)
}




