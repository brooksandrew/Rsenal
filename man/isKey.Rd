\name{isKey}
\alias{isKey}
\title{Determine strength of linking variables}
\usage{
  isKey(dfL, xvar)
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
\dontrun{
## download some baseball data. NOTE This will download 30MB of data (25 csv files) into a temporary directory
temp <- tempfile()
localDataDir <- paste0(tempdir(), '\\\\lahman2012-csv-onYourComp.zip')
download.file('http://seanlahman.com/files/database/lahman2012-csv.zip', localDataDir)
unzip(localDataDir, exdir=paste0(tempdir(), '\\\\lahman2012-csv-onYourComp')) ## may not be necessary

## create a list of data.frames from .CSVs
dfL <- dir2dfList(paste0(tempdir(), '\\\\lahman2012-csv-onYourComp'), ext='.csv', exclude=NULL, sep=',', stringsAsFactors=F)
isKey(dfL, 'playerID')
}
}
\seealso{
  \code{tableNet} \code{dir2dfList}
}

