#' @title Removes duplicative periods in column names of data.frame
#' 
#' @description When reading in a flat file (csv, txt, etc) that was converted from an Excel spreadsheet,
#' column names often have spaces between words.  R reads these spaces as periods.  If there are multiple spaces or special characters that
#' are not valid in column names, R replaces with a period.  When the data is messy, you sometimes get several consecutive periods
#' or traling periods at the end of column names.  I use this function as a coarse tool to standardize this ugliness
#' Periods can then be easily replaced (\code{gsub}'ed) with a single character if periods aren't your thing. 
#' 
#' @param df input data.frame to be aggregated

#' @return vector of strings without duplicate periods
#' @export
#' @examples
#' ## making some messed up data to fix
#' data(mtcars)
#' names(mtcars)[1] <- paste(names(mtcars)[1], '..', sep='')
#' names(mtcars)[3] <- paste(names(mtcars)[3], '.', sep='')
#' names(mtcars)[4] <- paste(names(mtcars)[3], '..also.known..as.horsepower', sep='')

#' removePeriods(names(mtcars))

removePeriods <- function(x) {
  ret <- gsub('\\.+', '\\.', x)
  lastE <- sapply(ret, function(z) substr(z,nchar(z),nchar(z)))
  ret[which(lastE=='.')] <- sapply(ret[which(lastE=='.')], function(z) substr(z, 0, nchar(z)-1))
  return(unlist(ret))
}