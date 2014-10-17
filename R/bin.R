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



