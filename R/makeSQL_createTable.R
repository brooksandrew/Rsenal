#' generate SQL script to create table in SQL database from data.frame
#' 
#' Creates a sql Query that will create a table in a SQL Database from an R dataframe.
#' It intializes with variable types (naively), but prevents the user from manually writing the query,
#' which can be especially cumbersome when there are many columns.
#' Initialized with basic SQL Server variable types.
#' 
#' @param df data.frame we will generate a SQL create table script for
#' @param tablename (optional) name of the table we want to create in a sql database
#' @param saveQuery (optional) name and filepath of the .sql script that will create the table.
#' @param wordy (optional) prints the sql query out to the console.
#' @return sql query which the create table script
#' @export
#' @examples
#' \dontrun{
#'  makeSQL_createTable(mtcars, saveQuery='mySQL_query.sql')
#' }


makeSQL_createTable <- function(df, tablename=deparse(substitute(df)), saveQuery=NULL, wordy=T) {

  types <- list(
    'numeric'='decimal',
    'integer'='int',
    'character'='varchar(40)',
    'logical'='varchar(40)',
    'factor'='varchar(40)',
    'POSIXct'='datetime'
    )
 
  sql <- paste('create table ', tablename, '\n', '(', sep='')
  for(i in 1:ncol(df)) {
    k <- names(df)[i]
    vclass <- ifelse(class(df[,k]) %in% names(types), types[[class(df[,k])]], 'varchar(40)')
    if(i<ncol(df))  sql <- paste(sql, paste(k, ' ', vclass, ',', sep=''), sep='\n')
    else sql <- paste(sql, paste(k, ' ', vclass, '\n', ')', sep=''), sep='\n')
  }
  
  if(wordy==T) cat(sql)
  if(is.null(saveQuery)==F) writeLines(sql, saveQuery)
  
  return(sql)
}


