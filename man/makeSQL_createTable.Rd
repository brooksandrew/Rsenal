\name{makeSQL_createTable}
\alias{makeSQL_createTable}
\title{generate SQL script to create table in SQL database from data.frame}
\usage{
  makeSQL_createTable(df,
    tablename = deparse(substitute(df)), saveQuery = NULL,
    wordy = T)
}
\arguments{
  \item{df}{data.frame we will generate a SQL create table
  script for}

  \item{tablename}{(optional) name of the table we want to
  create in a sql database}

  \item{saveQuery}{(optional) name and filepath of the .sql
  script that will create the table.}

  \item{wordy}{(optional) prints the sql query out to the
  console.}
}
\value{
  sql query which the create table script
}
\description{
  Creates a sql Query that will create a table in a SQL
  Database from an R dataframe. It intializes with variable
  types (naively), but prevents the user from manually
  writing the query, which can be especially cumbersome
  when there are many columns. Initialized with basic SQL
  Server variable types.
}
\examples{
makeSQL_createTable(mtcars, saveQuery='mySQL_query.sql')
}

