#' @title Equal depth binning
#' @description Simple equal depth binning algorithm.  
#' 
#' @param ser numeric vector to bin
#' @param nbins number of bins desired
#' @param qtype an integer between 1 and 9 selecting one of the nine quantile algorithms detailed below to be used.  See \code{\link{quantile}} for more details.  Default is 7.
#' @param digits number, number of digits to display in bin categories
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

depthbin <- function(ser, nbins=10, qtype=7, digits=10) {
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
#' @description 
#' 
#' @param x character vector of bins to format
#' @param r number, 0 to 10 (or higher I suppose) indicating how many decimals to display

#' @examples
#' d <- rpois(1000, 20)
#' d[d>26] <- sample(1:26, length(d[d>26]), replace=T)
#' dl <- letters[d]
#' barplot(table(dl))
#' table(binCat(dl, results=F, ncat=5))
#' table(binCat(dl, results=F, maxp=0.5))
#' table(binCat(dl, results=F, maxp=0.9))


binCat <- function(x, ncat=NULL, maxp=NULL, results=F) {
  if(is.null(maxp)==F & is.null(ncat)==F) warning("Parameters 'ncat' and 'maxp' are both specified.  It is advisable to only specify one of these criteria.  Algorithm will stop at the first criteria met.")
  
  ncat <- min(ncat, length(unique(x)))
  x <- as.character(x)
  n <- length(x)
  if(is.null(maxp)) maxp <- 1
  
  for(i in 1:length(unique(x))){
    xc <- x
    x1 <- sort(table(xc, exclude=NULL), decreasing=T)[1:i]
    catp <- sum(x1)/n
    if(i==ncat | catp>maxp)  {
      print(i)
      x2 <- sort(table(xc, exclude=NULL), decreasing=T)[1:(i+1)]
      xc[which(!xc %in% names(x2))] <- 'other'
      returnser <- xc
      break
    }
  }
  if(results==T) print(sort(table(returnser)/n, decreasing=T))
  return(returnser)  
}
  
  




