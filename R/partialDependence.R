#' @title Partial Dependence (single variable)
#' @description Calculate the partial dependence of a predictor variable on the response variable from a random forest classification model.
#' Rather than sequence through values of the predictor variable of interest and keep the other predictors at their median, this partial dependence 
#' technique creates replicates of the entire dataset for each level of the x variable of interest from it's min to max.  This gives a more 
#' realistic idea of the magnitude and direction of the x variable on the response.
#' 
#' @param model model object used to generate predictions.  Currently only built and tested for random forest.
#' @param df data.frame or data.table used to generate predictions with \code{model}
#' @param xvar character of length one; the x variable in \code{df} to assess for partial dependence with the response variable from \code{model}.
#' @param n numeric of length one; number of values between the min and max of \code{xvar} to score the model. 
#'  Note: this number is also how many replicates of \code{df} must be created and stored in memory.  Default is 10.
#' @param target.class character: Which category (class) of the target variable to use for predictions
#' @param ci numeric: specify any confidence intervals around the median response.  
#' @seealso \code{\link{partialDepPlot}}
#' @return data.table of output.  \code{cnt} refers to how many obs from \code{df} are within the fixed-width bin specified by \code{xvar}.  
#' @import randomForest
#' @import data.table
#' @export
#' 
#' @examples
#' library('randomForest')
#' library('data.table')
#' DF <- mtcars
#' DF$vs <- factor(DF$vs)
#' rf <- randomForest(vs~mpg+cyl+drat+qsec+disp+gear+carb+hp, DF, ntrees=100)
#' pd <- partialDep(model=rf, df=DF, xvar='mpg')
#' pd[ci==0.5,] # median of response when sequenced through 'mpg' 
#' 
#' ## Plotting
#' plot(pd[cilev==0, xvar], pd[cilev==0, pred], type='l', ylim=c(0,1))
#' lines(pd[ci==.95, xvar], pd[ci==.95, pred], type='l', col='red')
#' lines(pd[ci==.05, xvar], pd[ci==.05, pred], type='l', col='green')

partialDep <- function(model, df, xvar, n=10, target.class='1', ci=c(.9,.5,.3)) {
  if(!'data.table' %in% class(df)) df <- data.table(df)
  xv <- df[,get(xvar)] # x vector of interest
  xvr <- seq(min(xv), max(xv), length.out=n) # fixed width break points
  dfb <- df[rep(1:.N, each=n), ] # creating replicate of data for each 
  dfb[,xvar] <- rep(xvr, nrow(df)) # replacing x variable of interest with range of x var
  
  dfb[,cnt:=c(0, table(cut(df[,get(xvar)], breaks=xvr, include.lowest=T)))]
  pred <- predict(model, dfb, type='prob')[,target.class]
  
  retdf <- data.table(xvar=dfb[,get(xvar)], pred=pred, cnt=dfb[,cnt])
  ci <- c(ci, 0)
  ci <- unique(c(0.5+ci/2, 0.5-ci/2))
  ret <- retdf[,.(pred=quantile(pred, ci), cnt=cnt[1]), by='xvar'][,ci:=rep(ci, n)][,cilev:=abs(ci-0.5)*2][]
  return(ret)
}



#' @title Partial Dependence Spark Lines (multiple variables) 
#' @description Calculate the partial dependence of a predictor variable on the response variable from a random forest classification model.
#' Rather than sequence through values of the predictor variable of interest and keep the other predictors at their median, this partial dependence 
#' technique creates replicates of the entire dataset for each level of the x variable of interest from it's min to max.  This gives a more 
#' realistic idea of the magnitude and direction of the x variable on the response.
#' 
#' @param model model object used to generate predictions.  Currently only built and tested for random forest.
#' @param df data.frame or data.table used to generate predictions with \code{model}
#' @param n numeric of length one; number of values between the min and max of \code{xvar} to score the model. 
#' Note: this number is also how many replicates of \code{df} must be created and stored in memory.  Default is 10.
#' @param xvars character vector; the x variables in \code{df} to assess for partial dependence with the response variable from \code{model}.  Defaults to choosing all variables from model.
#' @param target.class character: Which category (class) of the target variable to use for predictions
#' @param ci numeric: specify any confidence intervals around the median response.  
#' @param plot logical: plot sparklines (\code{TRUE}), or no (\code{FALSE})
#' @param data logical: return summary table of output. yes (\code{TRUE}), or no (\code{FALSE})
#' @seealso \code{\link{partialDep}}
#' @return list of output and plot
#' @import randomForest
#' @import data.table
#' @import ggplot2
#' @export
#' 
#' @examples
#' library('randomForest')
#' DF <- mtcars
#' DF$vs <- factor(df$vs)
#' rf <- randomForest(vs~mpg+cyl+drat+qsec+disp+gear+carb+hp, DF, ntrees=100)
#' pda <- partialDepAll(model=rf, df=DF, n=10)

partialDepAll <- function(model, df, n=10, xvars=NULL, target.class='1', ci=c(.9,.5,.3), plot=T, data=T, plot.yaxis.fixed=T) {
  # currently only works with random forest
  if(is.null(xvars)) xvars <- names(model$forest$ncat)
  L <- list()
  for(i in xvars){
    tmpdf <- data.table(partialDep(model=model, df=df, xvar=i, n=n, target.class=target.class, ci=ci))
    tmpdf[,xname:=i]
    L[[i]] <- tmpdf
  }
  ret <- rbindlist(L)
  
  # producing data for sparklines plot
  pdplot <- ret
  pdplot[,x:=as.numeric(factor(xname))] # x is the variable being sequenced through.  Each x only once
  pdplot[,y:=rep(rep(1:n, each=length(unique(pdplot$ci))), length(xvars))] 
  setkey(pdplot,x,ci,y)
  pdplot[,preddiff:=pred-shift(pred), by=.(x,ci)] # getting absolute pred of change
  pdplot[,prednorm:=(pred-min(pred))/(max(pred)-min(pred)), by=.(x,ci)] # normalizing pred size
  pdplot[,cntrug:=cumsum(cnt)/nrow(df)*n, by=.(x,ci)] # creating ticks for rug plot
  
  # plotting spark lines
  if(plot==T){
    wd <- reshape(pdplot, idvar = c('x', 'y'), v.names='pred', timevar="ci", direction = "wide") # reshape wide for geom_ribbons
    gg <- ggplot()
    for(cl in ci) gg <- gg + geom_ribbon(data=wd, aes_string(x='y', ymin=paste0('pred.', 0.5-cl/2), ymax=paste0('pred.', 0.5+cl/2)), alpha=0.75*1/length(ci), fill='gray')
    gg <- gg + geom_line(data=pdplot[ci==0.5,], aes(x=y, y=pred, colour=pred), lwd=5) +
      facet_wrap(~xname, scales=ifelse(plot.yaxis.fixed==T, 'fixed', 'free')) +
      geom_rug(data=pdplot[ci==0.5,], aes(x=cntrug)) + 
      scale_colour_gradientn(colours=colorRampPalette(c('firebrick', 'grey', 'forestgreen'))(n)) +
      theme_bw() 
    plot(gg)
  }
  
  if(data==T){
    retL <- list(mediantab=data.frame(ret),
                 pdplot=data.frame(pdplot),
                 cilev=data.frame(ci),
                 n=n
    )
    return(retL)
  }
}
