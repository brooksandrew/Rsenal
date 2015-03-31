#' @title Aggregate multiple columns using different functions harnessing data.table efficiency
#' 
#' @description aggregates columns of a data.frame on 1 or multiple dimensions using 1 or multiple functions for different columns.
#' It is equivalent to the base R \code{aggregate} function, except that it allows the user to 
#' aggregate sets of columns (referred to by name or column #) with different functions... and it's fast!
#' 
#' @param df input data.frame to be aggregated
#' @param by variable name of data.frame \code{df} to aggregate on.  Same as \code{by} in base R \code{aggregate} function
#' @param ... method to identify the variables to aggregate and the functions used to do so.
#'            Specify the function first as a string argument and then a vector of the column names to aggregate using that function.
#'            You can specify as many different functions as necessary, but every function must follow a vector of column names.
#' @param catN adds a column named "countPerBin" with the # of observations aggregated in each row of the output data.frame.
#' @param printAgg prints the line of code used to 
#' @return  aggregated data.frame with columns corresponding to the grouping variables in by followed by aggregated columns from df.
#' @import data.table
#' @export
#' @examples
#' 
#' require('data.table')
#' 
#' ## establishing variables to aggregate on
#' lengthVs <- c('Sepal.Length', 'Petal.Length')
#' widthVs <- c('Sepal.Width', 'Petal.Width')
#' 
#' ## aggregating using 2 different functions and identifying columns to aggregate by variable names
#' irisAgg1 <- smartAgg(df=iris, by='Species', 'mean', lengthVs, 'sum', widthVs)
#'
#' ## aggregating using 2 dimensions ("Specied" and "randthing")
#' iris$randthing <- as.character(sample(1:5, nrow(iris), replace=T))
#' irisAgg2 <- smartAgg(df=iris, by=c('Species', 'randthing'), 'mean', lengthVs, 'sum', widthVs, catN=T, printAgg=T)
#' 
#' ## aggregating variables by column number
#' irisAgg3 <- smartAgg(df=iris, by=c('Species', 'randthing'), 'mean', 1:2, 'sum', 3:4, catN=T, printAgg=T)
#'
#' ## use anonymous functions
#' data(mtcars)
#' smartAgg(mtcars, by='cyl', function(x) sum(x*100), c('drat', 'mpg', 'disp'))
#' 
#' ## use anonymous functions with more than 1 argument.  Uses the provided variables for all unassigned arguments in anonymous function
#' smartAgg(mtcars, by='cyl', function(x,y='carb') sum(x*y), c('drat', 'mpg', 'disp'))
#' with(mtcars[mtcars$cyl==6,], c(sum(drat*carb), sum(mpg*carb), sum(disp*carb))) 
#' 
#' ## with anonymous functions with more than 1 argument.  Example of possible unintended behavior - the user-provided variable is used for both and x and y in this example.
#' smartAgg(mtcars, by='cyl', function(x,y) sum(x*y), c('drat', 'mpg', 'disp'))
#' with(mtcars[mtcars$cyl==6,], c(sum(drat*drat), sum(mpg*mpg), sum(carb*carb))) 
#' 
#' ## demonstrating speed gain of smartAgg using data.table over \code{aggregate}
#' n <- 300000
#' df <- data.frame(x1=rnorm(n), x2=rbinom(n,5,0.5), x3=sample(letters, n, replace=T))
#' system.time(aggFast <- smartAgg(df, by='x3', 'mean', c('x1', 'x2')))
#' system.time(aggSlow <- aggregate(df[,c('x2', 'x1')], by=list(df$x3), FUN='mean'))

smartAgg <- function(df, by, ..., catN=T, printAgg=F) {
  args <- list(...)
  dt <- as.data.table(df)
  
  ## organizing agg Methods and variable names into 2 separate lists
  aggMethod <- list()
  vars <- list()
  j<-1
  for(i in seq(1,length(args),2)) {
    aggMethod[[j]] <- args[[i]]
    vars[[j]] <- args[[i+1]]
    if(class(vars[[j]]) %in% c('integer', 'numeric')) vars[[j]] <- names(df)[vars[[j]]]
    j<-j+1
  }
  
  ## creat line to exec
  k<-0
  varL <- vector()
  for(j in 1:length(aggMethod)){
    for(i in 1:length(vars[[j]])){
      if(vars[[j]][i] %in% names(df)){
        if(class(aggMethod[[j]])=='function') {
          afun <- paste0('af',j)
          assign(afun, aggMethod[[j]])
          laf2 <- as.list(formals(get(afun)))
          laf2[which(lapply(laf2, nchar)==0)] <- vars[[j]][i] 
          rhstmp <- paste(unlist(lapply(seq_along(laf2), function(y,n,i) paste0(n[[i]], '=', y[[i]]), n=names(laf2), y=laf2)), collapse=',')
          tmp <- paste(vars[[j]][i], '=', afun, '(', rhstmp, ')', sep='') # anonymous functions
        } else {         
          tmp <- paste(vars[[j]][i], '=', aggMethod[[j]], '(', vars[[j]][i], ')', sep='') #non-anonymous functions
        }
        k <- k+1
        varL[k] <- tmp
      } else {print(paste('WARNING: ', vars[[j]][i], ' not in dataframe', sep=''))}
    }
  }
  varL <- paste(varL, collapse=', ')
  if(catN==T) varL <- paste(varL, ',countPerBin=length(', vars[[1]][1], ')', sep='')  
  
  ## actually creating aggregation command and executing it
  line2exec <- paste('dtAgg <- dt[,list(', varL, '), by=list(', paste(by,collapse=','), ')]', sep='')
  if(printAgg==T) print(line2exec)
  eval(parse(text=line2exec))
  dfAgg <- data.frame(dtAgg)
  
  return(dfAgg)
}


