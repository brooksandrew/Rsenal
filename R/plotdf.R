
#' @title Plot data.frame to PDF
#' @description Plots every column of a data.frame as an individual plot (one plot per page)
#'  in a PDF file.  
#' 
#' @param df data.frame to plot.
#' @param file string - name of the PDF that will be created
#' @param wordy - boolean \code{TRUE} or \code{FALSE}.  Sequentially prints status of each chart to the console.
#' Could be useful for large data.frames.
#'
#' @return PDF file of plots
#' @export
#' 
#' @examples
#' \dontrun{
#' plotdf(df=mtcars, file='mtcars_plots.pdf')
#' }

plotdf <- function(df, file='output.pdf', wordy=F){
  pdf(file)
  for(i in 1:ncol(df)){
    if(wordy==T) print(i)
    if((class(df[,i]) %in% c('numeric', 'integer')) & length(unique(df[,i]))>15) { plotNum(df[,i], vn=names(df)[i])
    } else { plotChar(df[,i], vn=names(df)[i])}
  }
  graphics.off()
  print(paste0('charts saved in ', getwd(), '/', file))
}


##################################################################
## Helper functions for plotdf (not included in Rsenal package)
##################################################################

plotNum <- function(x, ...) {
  par(mfrow=c(3, 2))
  par(oma=c(1,2,2,2))
  layout(matrix(c(1,1,2,2,2,2), 3, 2, byrow=T), widths=c(1,1), heights=c(1,1,1))
  ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
  
  p1<-plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='', main=ptitle)
  text(x=0.2, y=0.8, label='type:', pos=4, font=2)
  text(x=0.2, y=0.7, label='# of unique values:', pos=4, font=2)
  text(x=0.2, y=0.6, label='% NA:', pos=4, font=2)
  text(x=0.2, y=0.5, label='min:', pos=4, font=2)
  text(x=0.2, y=0.4, label='median:', pos=4, font=2)
  text(x=0.2, y=0.3, label='mean:', pos=4, font=2)
  text(x=0.2, y=0.2, label='max:', pos=4, font=2)
  
  text(x=0.4, y=0.8, label=class(x), pos=4)
  text(x=0.4, y=0.7, label=length(unique(x)), pos=4)
  text(x=0.4, y=0.6, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4)
  text(x=0.4, y=0.5, label=min(x, na.rm=T), pos=4)
  text(x=0.4, y=0.4, label=median(x, na.rm=T), pos=4)
  text(x=0.4, y=0.3, label=mean(x, na.rm=T), pos=4)
  text(x=0.4, y=0.2, label=max(x, na.rm=T), pos=4)
  
  p2<-hist(x, main=ptitle, breaks=30, xlab=ptitle, col='grey')
  rug(x, col='red')
  
  return(list(p1, p2))
}

## example
## plotNum(rnorm(1000)^2, vn='ssds')


plotChar <- function(x, ...) {
  par(mfrow=c(3, 2))
  layout(matrix(c(1,1,2,2,2,2), 3, 2, byrow=T), widths=c(1,1), heights=c(1,1,1))
  
  ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
  tab <- sort(table(x), decreasing=T)[1:min(length(unique(x)),50)]
  tabmiss <- round(sum(tab, na.rm=T)/length(x)*100, 1)
  
  p1 <- plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='', main=ptitle)
  text(x=0.2, y=0.8, label='type:', pos=4, font=2)
  text(x=0.2, y=0.7, label='# of unique values:', pos=4, font=2)
  text(x=0.2, y=0.6, label='% NA:', pos=4, font=2)
  
  text(x=0.4, y=0.8, label=class(x), pos=4)
  text(x=0.4, y=0.7, label=length(unique(x)), pos=4)
  text(x=0.4, y=0.6, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4)
  
  par(mar=c(10,4.1,4.1,2.1))
  p2 <- barplot(tab, las=2, cex.names=0.6, col='dodgerblue',
                main=paste(ptitle, ': ', tabmiss, '% of data shown', sep=''))
  par(mar=c(5.1,4.1,4.1,2.1))
  
  return(list(p1, p2))
}