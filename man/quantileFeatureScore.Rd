\name{quantileFeatureScore}
\alias{quantileFeatureScore}
\title{Quantile scoring function for continuous or integer valued data}
\usage{
  quantileFeatureScore(x, wq = 0.5, wf = 0.5)
}
\arguments{
  \item{x}{numeric vector to scale}
}
\value{
  a vector of attributes that correspond in order with the
  nodes in your igraph
}
\description{
  This function combines the quantile function with the
  basic feature normalization technique useful for getting
  a stable normalization
}
\examples{
cbind(quantileFeatureScore(sort(mtcars$mpg)), sort(mtcars$mpg))
cbind(quantileFeatureScore(mtcars$cyl), mtcars$cyl)
}

