\name{travCount}
\alias{travCount}
\title{Count # of nodes connected to root node}
\usage{
  travCount(g, root, orderN = -1, vmode = "out")
}
\arguments{
  \item{g}{igraph object}

  \item{root}{character string: name of root node}

  \item{orderN}{number, # of degrees away from root node to
  search.  Default is -1 which searches all degrees}

  \item{vmode}{character string: 'out', 'in' or 'all',
  determines how to subgraph \code{g} from \code{root}.
  Default is 'out'.}
}
\value{
  number, count of nodes connected to root node.
}
\description{
  Count doesn't include root node.  Simpler version of
  \code{\link{travOut}}. Might be faster?  Developed for a
  different project.
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

plot(g)
travCount(g, 'B')
travCount(g, 'L', vmode='out')
}
\seealso{
  \code{\link{travOut}}
}

