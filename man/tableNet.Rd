\name{tableNet}
\alias{tableNet}
\title{Shiny app to visualize schema of relational tables}
\usage{
  tableNet(dfL)
}
\arguments{
  \item{dfL}{list of data.frames used to generate schema.
  This is easily generated from \code{\link{dir2dfList}}}
}
\value{
  Shiny App
}
\description{
  Launches a Shiny App that provides a visual
  representation of the relationships between a collection
  of tables (data.frames) with some relational structure.
  If given a dump of dozens of flat files and without a
  formal schema or documentation on your data, this app
  will help explore and understand the underlying schema -
  which tables can be joined which other tables, which
  variables can be used to join which tables, etc, It also
  gives a read on how strongly each variable with the same
  name in multiple tables actually connects tables (how
  many of the values of the linking variable \code{x} that
  are in \code{table1} are also in \code{table2}.  \cr \cr
}
\details{
  Note, the Key-strength tables can be slow to display
  because these computations actually dive into the
  contents of the data.frames and perform set operations on
  every row of the variable of interest.  It is possible to
  use the \code{\link{isKey}} function to compute these
  similarity matrices ahead of time to prevent the Shiny
  app from doing these computations each time.
}
\seealso{
  \code{dir2dfList} \code{isKey}
}

