#' @title Equal depth binning
#' @description Simple equal depth binning algorithm.  
#' 
#' @param ser numeric vector to bin
#' @param nbins number of bins desired
#' @param qtype an integer between 1 and 9 selecting one of the nine quantile algorithms detailed below to be used.  See \code{\link{quantile}} for more details.  Default is 7.
#' @param digits number, number of digits to display in bin categories
#' @param labelRange logical: \code{TRUE} assigns a numeric score/ranking (ex. 1/3, 2/3, or 3/3 if 3 bins) to each bin. Can be combined with \code{labelOrder} and \code{labelPct}
#' @param labelPct logical: \code{TRUE} appends the percent of observations assigned to the bin to the factor level (name). Can be combined with \code{labelOrder} and \code{labelRange}
#' @param labelOrder logical: \code{TRUE} appends the ordinal position of the bin to the factor level (name).  Can be combined with \code{labelPct} and \code{labelRange}
#' 
#' @seealso \code{\link{quantile}}
#' @return ordered factor vector with bins
#' @export
#' 
#' @examples
#' ## perfect equal depth bins
#' x1 <- rnorm(1000, 0, 20)
#' binned1 <- depthbin(x1, nbins=10)
#' table(binned1)
#' 
#' ## slightly uneven bins with integer data
#' x2 <- rpois(1000, 3)
#' binned2 <- depthbin(x2, nbins=5)
#' summary(binned2)
#' 
#' ## as good as we can get with skewed integer data
#' x3 <- round(abs(log(abs(rnorm(1000)))))
#' binned3 <- depthbin(x3, nbins=5)
#' summary(binned3)
#' 
#' ## including more information in category names (levels of factor variable)
#' x4 <- round(abs(log(abs(rnorm(1000)))))
#' binned4 <- depthbin(x4, nbins=3, labelRange=T, labelPct=T, labelOrder=T)
#' summary(binned4)

depthbin <- function(ser, nbins=10, qtype=7, digits=10, labelRange=T, labelPct=F, labelOrder=F) {
  cutpts <- quantile(ser, probs=seq(0, 1, 1/nbins), na.rm=T, type=qtype)
  if(length(unique(cutpts))==nbins+1) {
    returnser <- cut(ser, breaks=cutpts, right=T, include.lowest=T)  
  } else {
    alldup <- vector()
    while(length(unique(cutpts))+length(alldup) < nbins+1) {
      dup <- cutpts[duplicated(cutpts)]
      dups <- unique(dup)
      alldup <- c(alldup, dups)
      dupL <- length(alldup) + length(dups)
      ser2 <- ser[which(!ser %in% alldup)]
      cutpts <- quantile(ser2, probs=seq(0, 1, 1/(nbins-length(dups))), na.rm=T, type=qtype)
    }
    cutpts <- c(unique(cutpts), alldup)
    returnser <- cut(ser, breaks=cutpts, include.lowest=T, dig.lab=digits, right=F)
  }
  if(sum(labelRange, labelPct, labelOrder)==0) {
    labelRange <- T
    warning('arguments labelRange, labelOrder, labelPct should not all be set to FALSE. Setting labelRange to TRUE.')
  }
  rawlev <- levels(returnser)
  if (labelRange==T) levels(returnser) <- paste0(levels(returnser), rawlev)
  if (labelOrder==T) levels(returnser) <- paste0(levels(returnser), ' ', 1:length(rawlev), '/', length(rawlev))
  if (labelPct==T) levels(returnser) <- paste0(levels(returnser), ' ', paste0('(', as.character(round(table(returnser)/length(returnser)*100, 1)), '%)'))
  for(i in 1:length(levels(returnser))) levels(returnser)[i] <- substr(levels(returnser)[i], nchar(rawlev[i])+1, nchar(levels(returnser)[i]))
  return(returnser)
}

#' @title Round numbers in interval
#' @description Formats an interval of form \code{(5.234,11.783]} to something like \code{(5.2,11.8]}.
#' Used for formatting only, mainly with binning functions like \code{\link{depthbin}}.  Intervals can be opened or closed with 
#' \code{(} and \code{[} respectively and are maintained as such when formatted.  Useful for prettifying graphs and reports.
#' 
#' @param x character vector of bins to format
#' @param r number, 0 to 10 (or higher I suppose) indicating how many decimals to display
#' 
#' @seealso \code{\link{depthbin}}
#' @return formatted character vector with length of input vector.
#' @export
#' @examples
#' x1 <- cut(quantile(rnorm(100)), breaks=4)
#' roundCut(x1, 1)

roundCut <- function(x, r=1){
  x <- as.character(x)
  b <- substr(x,0,1)
  e <- substr(x, nchar(x), nchar(x))
  xx <- substr(x, 2, nchar(x)-1)
  xx1 <- round(as.numeric(sapply(xx, function(z) strsplit(z, ',')[[1]][1])), r)
  xx2 <- round(as.numeric(sapply(xx, function(z) strsplit(z, ',')[[1]][2])), r)
  return(paste(b, xx1, ', ', xx2, e, sep=''))
}

#' @title categorical data binning by collapsing
#' @description Bins categorical variables into a smaller number of bins.  Useful when modeling with variables that have many small categories.
#' The largest categories are taken as is and the smaller categories are collapsed into a new field named 'other.'
#' There are two options for determining the number of bins: \cr 
#' 1. Specify the exact number of bins desired (\code{ncat}) \cr
#' 2. Specify how the share of your variable that will be represented with actual categories before naming everything else 'other' (\code{maxp}) \cr
#' @details It is advisable to use only the \code{ncat} OR \code{maxp} parameters.  When both used together, they will return whichever 
#' criteria yields the smaller number of bins. \cr
#' Possible unexpected behavior when setNA=NA and keepNA=T.  To keep NAs as standalone category, need to make setNA something that is not NA.
#' 
#' @param x vector to bin.  It is transformed to a character, so any type is acceptable
#' @param ncat number 0 to 100 (or higher I suppose).  Number of bins to collapse data to
#' @param maxp number 0 to 1.  Percentage of data that will be represented "as is" before categories are collapsed to "other"
#' @param results logical \code{TRUE} or \code{FALSE}.  Prints a frequency table of the new categories.
#' @param setNA value to set NAs to.  default is to keep NA.  Can set to a character string to make NAs a category
#' @param keepNA logical. \code{TRUE} keeps NAs as their own character.  \code{FALSE} bundles NAs into 'other' category.
#' @return vector of binned data
#' @export
#' @examples
#' d <- rpois(1000, 20)
#' d[d>26] <- sample(1:26, length(d[d>26]), replace=T)
#' dl <- letters[d]
#' barplot(table(dl))
#' table(binCat(dl, results=F, ncat=5))
#' table(binCat(dl, results=F, maxp=0.5))
#' table(binCat(dl, results=F, maxp=0.9))
#' 
#' ## With missings
#' ff <- sample(letters[1:15], 100, replace=T)
#' ff[sample(100, 10)] <- NA
#' binCat(ff, ncat=7, setNA='missing')


binCat <- function(x, ncat=NULL, maxp=NULL, results=F, setNA=NA, keepNA=F) {
  if(is.null(maxp)==F & is.null(ncat)==F) warning("Parameters 'ncat' and 'maxp' are both specified.  It is advisable to only specify one of these criteria.  Algorithm will stop at the first criteria met.")
  if(is.na(setNA)==F) x[is.na(x)] <- setNA
  
  ncat <- min(ncat, length(unique(x)))
  x <- as.character(x)
  n <- length(x)
  if(is.null(maxp)) maxp <- 1
  
  for(i in 1:length(unique(x))){
    xc <- x
    x1 <- sort(table(xc, exclude=NULL), decreasing=T)[1:i]
    catp <- sum(x1)/n
    if(i==ncat | catp>maxp)  {
      x2 <- sort(table(xc, exclude=NULL), decreasing=T)[1:(i+1)]
      if(keepNA==T) {xc[which(!xc %in% c(names(x2), setNA))] <- 'other'
      } else {xc[which(!xc %in% names(x2))] <- 'other'}
      returnser <- xc
      break
    }
  }
  if(results==T) print(sort(table(returnser)/n, decreasing=T))
  return(returnser)  
}
  



