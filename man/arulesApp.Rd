\name{arulesApp}
\alias{arulesApp}
\title{Assocation Rules Visualization Shiny App}
\usage{
  arulesApp(dataset, bin = T)
}
\arguments{
  \item{dataset}{data.frame, this is the dataset that
  association rules will be mined from.  Each row is
  treated as a transaction.}

  \item{bin}{logical, \code{TRUE} will automatically
  discretize/bin numerical data into categorical features
  that can be used for association analysis.}
}
\value{
  Shiny App
}
\description{
  Launches a Shiny App that provides an interative
  interface to the visualizations of the \code{arulesViz}
  package. The app allows users to mine rules based on all
  or just subsets of features, sort by criteria (lift,
  support, confidence) and visualize using network graph,
  grouped bubble and scatter plots. \cr Users filter rules
  to target only those with a certain variable on the RHS
  or LHS of the rule. Rule mining is computed using the
  \link{apriori} algorithm from \code{arules}.
}
\examples{
## creating some data
n <- 10000 # of obs
d <- data.frame(
eye = sample(c('brown', 'green', 'blue', 'hazel'), n, replace=T),
gender = sample(c('male', 'female'), n, replace=T),
height = sort(sample(c('dwarf', 'short', 'average', 'above average', 'giant'), n, replace=T)),
wealth = sort(sample(c('poor', 'struggling', 'middle', 'uppermiddle', 'comfortable', 'rich', '1\%', 'millionaire', 'billionaire'), n, replace=T)),
favoriteAnimal = sample(c('dog', 'cat', 'bat', 'frog', 'lion', 'cheetah', 'lion', 'walrus', 'squirrel'), n, replace=T),
numkids = abs(round(rnorm(n, 2, 1)))
)

## adding some pattern
d$numkids[d$gender=='male'] <- d$numkids[d$gender=='male'] + sample(0:3, sum(d$gender=='male'), replace=T)
d$numkids <- factor(d$numkids)

## calling Shiny App to visualize association rules
arulesApp(d)
}
\seealso{
  \code{arulesViz}, \code{arules}
}

