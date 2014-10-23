\name{dir2dfList}
\alias{dir2dfList}
\title{Turn a directory of flat files into a list of data.frames}
\usage{
  dir2dfList(dfdir, ext = ".txt", exclude = NULL,
    printdf = T, ...)
}
\arguments{
  \item{dfdir}{character string of the directory where you
  want to load flat files}

  \item{ext}{file extention on the type of files to load.
  Usually \code{.csv} or \code{.txt}}

  \item{exclude}{character string of variables to be
  excluded from app.  Can also exclude variables
  interactively from app.}

  \item{printdf}{logical \code{TRUE} or \code{FALSE}.
  Prints progress of flat file loads to R console.}

  \item{...}{parameters to pass to
  \code{\link{read.delim}}.  Commonly \code{nrow},
  \code{sep},}
}
\value{
  list of data.frames
}
\description{
  Useful to prepare data for \code{\link{tableNet}}
}
\examples{
1==1
}
\seealso{
  \code{tableNet} \code{isKey}
}

