\name{shuffleAnova}
\alias{shuffleAnova}
\title{Variable importance from iteratively shuffled orders of predictor variables}
\usage{
  shuffleAnova(form, df, n = 5, test = "Chisq")
}
\arguments{
  \item{form}{formula to be passed to
  \code{\link{travCount}}.}

  \item{df}{data.frame of data to be used for analysis}

  \item{n}{number of iterations for shuffled ANOVA
  analysis}

  \item{test}{character string, statistical test to run.
  Default is 'Chisq'.}
}
\value{
  data.frame of results
}
\description{
  Runs multiple anova analyses to assess deviance explained
  by each predictor in shuffled orders, iteratively.
  Currently using only logistic regression.  Could be
  generalized.
}
\examples{
form <- as.formula('am~wt+gear+carb+cyl+hp+drat+qsec')
shuffleAnova(form, mtcars, n=50)
}

