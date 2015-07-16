#' @title Assess overlap in vectors
#' @description Useful for learning the overlap of absolute and unique values between two vectors of the same type.  They
#' do not have to be the same length, and are allowed to contain NAs.
#' @param x1 vector of same class as \code{x2}
#' @param x2 vector of same class as \code{x1}
#' @param na.rm logical, remove NAs from analysis
#' @param wordy logical, prints convenience information
#' @return summary data.frame
#' @export
#' @examples
#' x1 <- sample(1:50, 40, replace=T)
#' x2 <- sample(c(1:60, rep(NA, 30)), 55, replace=T)
#' overlap(x1, x2, na.rm=T)
#' overlap(x1, x2)
#' overlap(mtcars$gear, mtcars$cyl, x1name='gear', x2name='cyl')

overlap <- function(x1, x2, na.rm=F, wordy=T, x1name=NULL, x2name=NULL) {
  
  if(class(x1) != class(x2)) {
    warning('classes of x1 and x2 are not equal.  Coercing both to character')
    if(class(x1)!='character') x1 <- as.character(x1)
    if(class(x2)!='character') x2 <- as.character(x2) 
  }
  
  x1name <- ifelse(is.null(x1name), 'x1', x1name)
  x2name <- ifelse(is.null(x2name), 'x2', x2name)
  
  if(wordy==T) {
    cat(paste0('x1 is ', deparse(substitute(x1))))
    cat('\n')
    cat(paste0('x2 is ', deparse(substitute(x2))))
    cat('\n\n')
  }
  
  if(na.rm==T){
    x1 <- x1[is.na(x1)==F]
    x2 <- x2[is.na(x2)==F]
  }

  ret <- list()
  
  ret[[sprintf('%s exist in %s', x1name, x2name)]] <- c(
      length(x1), 
      length(x2),
      sum(x1 %in% x2), 
      sum(x1 %in% x2)/length(x1)
    )
  ret[[sprintf('%s exist in %s', x2name, x1name)]] <- c(
      length(x2),
      length(x1),
      sum(x2 %in% x1), 
      sum(x2 %in% x1)/length(x2)
      )
  ret[[sprintf('unique(%s) exist in unique(%s)', x1name, x2name)]] <- c(
      length(unique(x1)),
      length(unique(x2)),
      sum(unique(x1) %in% unique(x2)), 
      sum(unique(x1) %in% unique(x2))/length(unique(x1))
      )
  ret[[sprintf('unique(%s) exist in unique(%s)', x2name, x1name)]] <- c(
      length(unique(x2)),
      length(unique(x1)),
      sum(unique(x2) %in% unique(x1)), 
      sum(unique(x2) %in% unique(x1))/length(unique(x2))
      )
  

  df <- t(data.frame(ret))
  row.names(df) <- names(ret)
  colnames(df) <- c('LHS_count', 'RHS_count', 'count', 'percent')
  
  return(df)
}


