#' @title Get 1 degree away vertexes
#' 
#' @description Returns a list of all vertex names that touch (going IN or OUT) from the
#' specified vertex list.
#' 
#' @param g igraph object
#' @param vlist list of starting node/vertex names
#' @param mode 'out' or 'in'.  'out' returns all the vertex/node names 1 degree out from \code{vlist}. 'in' returns all 
#'         vertex/node names directed into vlist nodes.
#' @return vector of node names that are 1 degree away from \code{vlist}
#' @export
#' @examples
#' ## build sample network
#' from <- c('A','A','B','B','C','C','D','D','E','E','E','F','F','H','L','L','O')
#' to <- c('B','C','D','E','F','G','H','I','J','J','K','L','G','M','N','O','P')
#' relations<-cbind(from,to)
#' g <- graph.data.frame(relations)
#' tiers <- c(1,2,3,4,1,3,4,2,1,2,3,4,0,0,1,2)
#' V(g)$tier <- tiers
#' 
#' getV1fromVlist(g, c('A', 'D'), 'to')

getV1fromVlist <- function(g, vlist, mode='out'){
  if(!mode %in% c('out', 'in', 'all')) stop("mode argument must be equal to 'in', 'out' or 'all'")
  eidList <- array()
  for (i in vlist) {
    eidList <- c(eidList, igraph::incident(g, i, mode))
  }
  eidList <- unique(eidList[!is.na(eidList)])
  mode2 <- ifelse(mode=='out', 'to', 'from')  
  v <- getVfromE(g, eidList, mode2)
  return(v)
}


#' @title Get vertex names from edge IDs
#' @description Returns the vertex names associated with the specified edge IDs (FROM or TO) 
#' @param g igraph object to mine vertex names from
#' @param eid edge IDs referenced by a numeric vector or the edge object themselves
#' @param mode "to" or "from". Indicates whether to return vertex names for nodes going to (in) or from (out) from the edge.
#' @param unique \code{TRUE} or \code{FALSE}.  TRUE simply removes duplicate vertex names and returns a unique list.
#' @return vertex names associated with the specified edge IDs (FROM or TO)
#' @import igraph
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



#' @title Get edge IDs from vertices
#' @description  returns the edge IDs that go OUT or IN from the specified list of vertices
#' @param g igraph object
#' @param vlist list of node/vertex names
#' @param mode character string: 'in', 'out' or 'all'
#' @return edge IDs that go OUT or IN from the specified list of vertices
#' @import igraph
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
#' getEfromVlist(g, c('A', 'D'), 'to')

getEfromVlist <- function(g, vlist, mode='out'){
  eidList <- array()
  for (i in vlist) {
    eidList <- c(eidList, igraph::incident(g, i, mode))
  }
  eidList <- unique(eidList[!is.na(eidList)])
  return(eidList)
}

#' @title Prune edges of igraph
#' @description This function takes a very connected network graph and prunes the edges down 
#' so that it focuses on depth from the root node to the end nodes (inter-connective 
#' edges are deleted).
#' 1. Find all nodes 1 step out from root.  These edges must stay. 
#' 2. Find all nodes 2 steps from root that have more than 2 edges in. 
#'        Keep only one edge (the one that leads to the shortest path back to the root). 
#' 3. Repeat for the nodes one more degree away from root.
#' @param g igraph object
#' @param root character string: name of root node
#' @return igraph object, pruned subgragh of \code{g}
#' @import igraph
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
#' prungeEdge(g, 'A')

pruneEdge <- function(g, root){
  gF <- g
  d <- degree(gF, mode='in')
  
  nhood <- V(g)[neighbors(gF, root, 'out')]$name
  nhood <- c(nhood, root)
  d <- d[!names(d) %in% nhood]   
  dP <- names(d[d>1])
  
  for(v in dP){
    spv <- shortest.paths(gF, v, root)
    eid <- getEfromVlist(gF, v, 'in')
    
    df <- t(shortest.paths(gF, root, getVfromE(gF, eid, 'from'), 'out'))
    df <- df[order(df[,1]),1]
    shortPv <- names(df)[1]
    eidKeep <- as.numeric(E(gF)[shortPv %->% v])
    
    if(length(eid)>1){	
      eidDel <- eid[eid!=eidKeep]
      gF <- delete.edges(gF, eidDel)
    }	
  }
  return(gF)
}

#' @title Find all nodes connected to root node
#' @description Returns a subset of the original graph (all edges and vertices that a
#' directed path can take from from the root node.  OrderN limits the growth of these paths.
#' @param g igraph object
#' @param root character string: name of root node
#' @param orderN number: # of degrees away from root node to search.
#' @seealso Simpler version: \code{\link{travCount}}
#' @return igraph object, subgragh of \code{g}
#' @import igraph
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
#' plot(g) ## full network 
#' plot(travOut(g, 'D')) ## sub network

travOut <- function(g, root, orderN=-1) {
  eid <- incident(g, root, 'out')
  noEdgesTest <- identical(eid, numeric(0))
  
  if(noEdgesTest==F){
    v <- getVfromE(g, eid, 'to')
    
    eidKids <- array()
    v2<-v
    spv <- shortest.paths(g, root)
    sp <- max(spv[spv!=Inf])
    n <- ifelse(orderN==-1, sp, orderN)
    for(i in 1:n) {
      eidIter <- getEfromVlist(g, v2)
      v2 <- getV1fromVlist(g, v2, 'out')
      eidKids <- c(eidKids, eidIter)
    }
    
    reteid  <- unique(c(eid, eidKids))
    reteid <- reteid[!is.na(reteid)]
    ret <- subgraph.edges(g, reteid)
    
  } else {ret <- graph.neighborhood(g, 1, root, 'out')[[1]]}
  if(noEdgesTest==F & orderN==1) ret <- subgraph.edges(g, eid)
  return(ret)
}


#' @title Count # of nodes connected to root node
#' @description Count doesn't include root node.  Simplier version of  \code{\link{travOut}}.  
#' Might be faster?  Developed for a different project.
#' @param g igraph object
#' @param root character string: name of root node
#' @param orderN number: # of degrees away from root node to search.  Default is -1 which searches all degrees
#' @param vmode character string: 'out', 'in' or 'all',  determines how to subgraph \code{g} from \code{root}. Default is 'out'.
#' @seealso \code{\link{travOut}}
#' @return number: count of nodes connected to root node.
#' @import igraph
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
#' plot(g)
#' travCount(g, 'B')
#' travCount(g, 'L', vmode='out')

travCount <- function(g, root, orderN=-1, vmode='out') {
  #eid <- which(!E(g)$type %in% vtype)
  #g <- subgraph.edges(g, eid, delete.vertices=T)
  
  vb <- root
  v <- getV1fromVlist(g, root, vmode)
  vv <- root
  if(orderN==-1){
    spv <- igraph::shortest.paths(g, root)
    sp <- max(spv[spv!=Inf])
    n <- sp
  } else {n <- orderN}
  for(i in 1:n) {
    vtmp <- getV1fromVlist(g, vb, vmode)
    v <- unique(c(v, vtmp))
    vb <- vtmp
  }
  return(length(v))
}





