#' @title Get vertex names from edge IDs
#' 
#' @description Returns the vertex names associated with the specified edge IDs (FROM or TO)
#' 
#' @param g igraph object to mine vertex names from
#' @param eid edge IDs referenced by a numeric vector or the edge object themselves
#' @param mode "to" or "from". Indicates whether to return vertex names for nodes going to (in) or from (out) from the edge.
#' @param unique "TRUE" or "FALSE".  TRUE simply removes duplicate vertex names and returns a unique list.
#' @return vertex names associated with the specified edge IDs (FROM or TO)
#' @export
#' @examples
#' require('igraph')
#'
#' ## build sample network
#' from <- c('A','A','B','B','C','C','D','D','E','E','E','F','F','H','L','L','O')
#' to <- c('B','C','D','E','F','G','H','I','J','J','K','L','G','M','N','O','P')
#' relations<-cbind(from,to)
#' g <- graph.data.frame(relations)
#' tiers <- c(1,2,3,4,1,3,4,2,1,2,3,4,0,0,1,2)
#' V(g)$tier <- tiers
#'
#'getVfromE(g, E(g)[1:5])
#'getVfromE(g, 1:5)

getVfromE <- function(g, eid, mode='to', unique=T) {
  eid <- unique(eid[!is.na(eid)])
  df <- get.edgelist(subgraph.edges(g, eid))
  col <- ifelse(mode=='from', 1, 2)
  retdf <- df[,col]
  if(unique==T) retdf <- unique(df[,col])
  return(retdf)
}
