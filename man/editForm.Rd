\name{editForm}
\alias{editForm}
\title{Edit a formula}
\usage{
  editForm(form, add = NULL, sub = NULL)
}
\arguments{
  \item{form}{formula, the formula you will be editing}

  \item{sub}{character vector, predictor variables to
  subtract (remove) from formula}

  \item{add}{character vector, predictor variables to add
  to formula}
}
\value{
  formula, edited formula object
}
\description{
  Easy way to add or subtract predictor variables to a
  formula
}
\examples{
form <- as.formula('mpg~cyl+hp+drat+qsec')
editForm(form, add=c('wt', 'gear', 'carb'))
editForm(form, sub=c('cyl', 'hp', 'qsec', 'variableNotInFormula'))
editForm(form, sub=c('cyl', 'hp', 'qsec'), add=c('wt'))
}

