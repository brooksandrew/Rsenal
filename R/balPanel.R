#' @title Create balanced panel dataset
#' 
#' @description Takes an unbalanced panel dataset and makes it balanced by inserting rows for missing time periods 
#' and initializing with NAs for other columns that are not the time or individual variable.
#' 
#' @details Doesn't have any special treatment for duplicate values in the input time panel dataset.
#' Currently handles time/date objects of type "Date" or "numeric".  Can add POSIX and other date/time classes as needed. 
#' 
#' @param df data.frame of unbalanced panel data
#' @param datev name of data variable in data.frame \code{df}
#' @param id name of individual/entity variable.  Company ID, country or people's names, for example.
#' @param freq desired frequency of the time-series'
#' @return balanced panel dataset as a data.frame.  Values for dates that are missing in the input dataset show up as NAs
#' @export
#' @examples
#' df <- data.frame(date=sort(sample(seq(Sys.Date()-20, Sys.Date(), by=1), 30, replace=T)),
#'                 x1=runif(30),
#'                 id=rep(c('A', 'B', 'C'), 10), stringsAsFactors=F)
#'
#' balPanel(df, datev='date', id='id', freq='days')


balPanel <- function(df, datev, id, freq='month') {
  if(class(df[,datev])=='Date') {dateRange <- seq(min(df[,datev]), max(df[,datev]), by=freq)
  } else if(class(df[,datev]) %in% c('numeric', 'integer')) {dateRange <- seq(min(df[,datev]), max(df[,datev]), by=freq)
  } else {stop('type of date variable not Date, numeric or integer')}
  
  assetList <- unique(df[, id])
  
  master <- data.frame(
    v1 = rep(dateRange, length(assetList)),
    v2 = sort(rep(assetList, length(dateRange)))
  )
  names(master) <- c(datev, id)
  
  bP <- merge(master, df, by=c(datev, id), all.x=T, all.y=F)
  return(bP)
}