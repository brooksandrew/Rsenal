\name{plotBestglm}
\alias{plotBestglm}
\title{All-subsets logistic regression}
\usage{
  plotBestglm(bglm, rc = 2)
}
\arguments{
  \item{bglm}{\code{bestglm} object from
  \code{\link{bestglm}}}

  \item{rc}{decimal places to display on y-axis of plot}
}
\value{
  plot
}
\description{
  Plots the output of the bestglm BestModels object.
  Similar to the visual output of plotting a regsubsets
  object
}
\examples{
require('bestglm')
b <- bestglm(Xy=mtcars[,c('mpg', 'hp', 'drat', 'cyl', 'wt', 'qsec', 'vs')], family=binomial(logit), IC='BIC', nvmax=4)
plotBestglm(b$BestModels, rc=3)
}
\seealso{
  \code{\link{bestglm}}, \code{\link{leaps}}
}

