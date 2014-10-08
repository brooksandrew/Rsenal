\name{getV1fromVlist}
\alias{getV1fromVlist}
\title{Get 1 degree away vertexes}
\usage{
  getV1fromVlist(g, vlist, mode = "out")
}
\arguments{
  \item{g}{igraph object}

  \item{vlist}{list of starting node/vertex names}

  \item{mode}{'out' or 'in'.  'out' returns all the
  vertex/node names 1 degree out from \code{vlist}. 'in'
  returns all vertex/node names directed into vlist nodes.}
}
\value{
  vector of node names that are 1 degree away from
  \code{vlist}
}
\description{
  Returns a list of all vertex names that touch (going IN
  or OUT) from the specified vertex list.
}
\examples{
## build sample network
from <- c('A','A','B','B','C','C','D','D','E','E','E','F','F','H','L','L','O')
to <- c('B','C','D','E','F','G','H','I','J','J','K','L','G','M','N','O','P')
relations<-cbind(from,to)
g <- graph.data.frame(relations)
tiers <- c(1,2,3,4,1,3,4,2,1,2,3,4,0,0,1,2)
V(g)$tier <- tiers

getV1fromVlist(g, c('A', 'D'), 'to')
}

