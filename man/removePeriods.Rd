\name{removePeriods}
\alias{removePeriods}
\title{Removes duplicative periods in column names of data.frame}
\usage{
  removePeriods(x)
}
\arguments{
  \item{df}{input data.frame to be aggregated}
}
\value{
  vector of strings without duplicate periods
}
\description{
  When reading in a flat file (csv, txt, etc) that was
  converted from an Excel spreadsheet, column names often
  have spaces between words.  R reads these spaces as
  periods.  If there are multiple spaces or special
  characters that are not valid in column names, R replaces
  with a period.  When the data is messy, you sometimes get
  several consecutive periods or traling periods at the end
  of column names.  I use this function as a coarse tool to
  standardize this ugliness Periods can then be easily
  replaced (\code{gsub}'ed) with a single character if
  periods aren't your thing.
}
\examples{
## making some messed up data to fix
data(mtcars)
names(mtcars)[1] <- paste(names(mtcars)[1], '..', sep='')
names(mtcars)[3] <- paste(names(mtcars)[3], '.', sep='')
names(mtcars)[4] <- paste(names(mtcars)[3], '..also.known..as.horsepower', sep='')
removePeriods(names(mtcars))
}

