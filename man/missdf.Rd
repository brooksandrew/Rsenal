\name{missdf}
\alias{missdf}
\title{Inspect missing data in a data.frame}
\usage{
  missdf(df, criteria = "NA")
}
\arguments{
  \item{df}{data.frame to inspect for missing values}

  \item{character}{string, criteria to search for.
  \code{'NA'} or \code{'Inf'}}
}
\value{
  summary table
}
\description{
  Currently supports \code{NA} and \code{Inf}
}
\examples{
mtcars2 <- mtcars
for(i in 1:ncol(mtcars2)) mtcars2[sample(nrow(mtcars2), sample(1:5,1), replace=T),i] <- NA
missdf(mtcars2)
}
}

