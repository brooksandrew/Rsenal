\name{balPanel}
\alias{balPanel}
\title{Create balanced panel dataset}
\usage{
  balPanel(df, datev, id, freq = "month")
}
\arguments{
  \item{df}{data.frame of unbalanced panel data}

  \item{datev}{name of data variable in data.frame
  \code{df}}

  \item{id}{name of individual/entity variable.  Company
  ID, country or people's names, for example.}

  \item{freq}{desired frequency of the time-series'}
}
\value{
  balanced panel dataset as a data.frame.  Values for dates
  that are missing in the input dataset show up as NAs
}
\description{
  Takes an unbalanced panel dataset and makes it balanced
  by inserting rows for missing time periods and
  initializing with NAs for other columns that are not the
  time or individual variable.
}
\details{
  Doesn't have any special treatment for duplicate values
  in the input time panel dataset. Currently handles
  time/date objects of type "Date" or "numeric".  Can add
  POSIX and other date/time classes as needed.
}
\examples{
df <- data.frame(date=sort(sample(seq(Sys.Date()-20, Sys.Date(), by=1), 30, replace=T)),
                x1=runif(30),
                id=rep(c('A', 'B', 'C'), 10), stringsAsFactors=F)

balPanel(df, datev='date', id='id', freq='days')
}

