\name{isKey}
\alias{isKey}
\title{Determine strength of linking variables}
\usage{
  isKey(...)
}
\arguments{
  \item{dfL}{list of data.frames.  easily generated from
  \code{\link{dir2dfList}}}

  \item{xvar}{character string, name of the variable to
  calculate strength for across all tables in \code{dfL}}

  \item{printdf}{prints progress of flat file loads to R
  console.}
}
\value{
  list of data.frames
}
\description{
  This function computes the percentage of unique values of
  a column \code{x} from \code{table1} that appear in in a
  \code{table2}.  It is called and computed on the fly in
  \code{\link{tableNet}}.  However, these computations can
  be slow on large datasets, so it is provided a standalone
  function that can be run once to store the output and fed
  into the \code{\link{tableNet}} app to prevent repetitive
  slow computations on the fly.
}
\examples{
1==1
}
\seealso{
  \code{tableNet} \code{dir2dfList}
}

