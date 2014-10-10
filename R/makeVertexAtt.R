#' @title merge data.frame attributes to an igraph (network) object
#' 
#' @description Enriches an igraph object with node/vertex attributes from a data.frame
#' 
#' @param g igraph object to which you want to add a node/vertex attribute
#' @param df data.frame containing the attribute you want to add to igraph
#' @param vname name of column in data.frame \code{df} that you will merge into igraph
#' @param by.df unique key in data.frame that you will use to merge attribute into igraph
#' @param by.g unique key in igraph that you will use to merge attribute from data.frame
#' @return a vector of attributes that correspond in order with the nodes in your igraph
#' @import igraph
#' @export
#' @examples
#' require('igraph')
#' actors <- data.frame(names=c("Alice", "Bob", "Cecil", "David","Esmeralda"),
#' age=c(48,33,45,34,21),
#' gender=c("F","M","F","M","F"))
#' relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
#'                               "David", "Esmeralda"),
#'                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#'                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
#'                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
#' g <- igraph::graph.edgelist(as.matrix(relations[,c('from', 'to')]), directed=T)
#' 
#' V(g)$age <- makeVertexAtt(g, df=actors, vname='age', by.df='names', by.g='name')


makeVertexAtt <- function(g, df, vname, by.df, by.g='name') {
  gdf <- data.frame(id=igraph::get.vertex.attribute(g, by.g), stringsAsFactors=F)
  mdf <- merge(gdf, df[,c(by.df, vname)], by.x='id', by.y=by.df, all.x=T, all.y=F) 
  check <- igraph::get.vertex.attribute(g, by.g)
  mdf <- mdf[match(check, mdf$id),]
  
  ## checking that order of assets is preserved
  mdf$check01 <- check == mdf$id
  if(sum(check!=mdf$id)>0) cat(paste('Warning: Order of Network Nodes might be misaligned!!! \n', 
                                        sum(mdf$check01), ' out of ', nrow(mdf), ' nodes are misaligned', sep='')) 
  if(sum(is.na(mdf[,vname]))>0) print(paste('Warning: ', sum(is.na(mdf[,vname])), ' NAs out of ', nrow(mdf), ' node attributes in graph.', sep=''))
  
  return(mdf[,vname])
}

  
