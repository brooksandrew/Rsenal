\name{pruneEdge}
\alias{pruneEdge}
\title{Prune edges of igraph}
\usage{
  pruneEdge(g, root)
}
\arguments{
  \item{g}{igraph object}

  \item{root}{character string: name of root node}
}
\value{
  igraph object, pruned subgragh of \code{g}
}
\description{
  This function takes a very connected network graph and
  prunes the edges down so that it focuses on depth from
  the root node to the end nodes (inter-connective edges
  are deleted). \cr \cr 1. Find all nodes 1 step out from
  root.  These edges must stay.  \cr 2. Find all nodes 2
  steps from root that have more than 2 edges in.  Keep
  only one edge (the one that leads to the shortest path
  back to the root).\cr 3. Repeat for the nodes one more
  degree away from root.
}
\examples{
require('igraph')

## build sample network
from <- c('A','A','B','B','C','C','D','D','E','E','E','F','F','H','L','L','O')
to <- c('B','C','D','E','F','G','H','I','J','J','K','L','G','M','N','O','P')
relations<-cbind(from,to)
g <- graph.data.frame(relations)
tiers <- c(1,2,3,4,1,3,4,2,1,2,3,4,0,0,1,2)
V(g)$tier <- tiers

prungeEdge(g, 'A')
}

