\name{rules2df}
\alias{rules2df}
\title{Transform association rule results into data.frame}
\usage{
  rules2df(rules)
}
\arguments{
  \item{rules}{list of association rules (S4 arules
  object).  Output of \code{apriori} function.}
}
\value{
  association rules in a data.frame
}
\description{
  Note this function only currently works when the itemsets
  are of size 1 on the LHS and RHS
}
\examples{
require('arules')
data("Adult")
ar <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
df<-rules2df(ar)
}

