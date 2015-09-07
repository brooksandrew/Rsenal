library('randomForest')
df <- mtcars
df$vs <- factor(df$vs)
rf <- randomForest(vs~mpg+cyl+drat+qsec+disp+gear+carb+hp, df, ntrees=10)

if(1==0){
  ## Calculate Partial Dependence Plot for classification.  one X variable at a time.
  model <- rf; df<-df; xvar <- 'qsec'; n<-5; target.class='1'; ci=c(.9,.5,.3)
}

# NEED TO COUNT OBS PER BIN for rug plot
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
  ret <- retdf[,.(pred=quantile(pred, ci), cnt=cnt[1]), by='xvar'][,ci:=rep(ci, n)][,cilev:=abs(ci-0.5)*2]
  return(ret)
}


pd  <- partialDep(rf, df, xvar='qsec', n=10)

plot(pd[cilev==0, xvar], pd[cilev==0, pred], type='l', ylim=c(0,1))
lines(pd[ci==.95, xvar], pd[ci==.95, pred], type='l', col='red')
lines(pd[ci==.05, xvar], pd[ci==.05, pred], type='l', col='green')


# Setting attributes for testing partialDepAll ###############################
if(1==0){
  library('data.table')
  model <- rf; df<-df; xvars<-names(model$forest$ncat) ; n<-5; target.class='1'
  i <- 'mpg'
}

partialDepAll <- function(model, df, n=10, xvars=NULL, target.class='1', ci=c(.9,.5,.3)) {
  # currently only works with random forest
  if(is.null(xvars)) xvars <- names(model$forest$ncat)
  L <- list()
  for(i in xvars){
    tmpdf <- data.table(partialDep(model=model, df=df, xvar=i, n=n, target.class=target.class))
    tmpdf[,xname:=i]
    L[[i]] <- tmpdf
  }
  ret <- rbindlist(L)
    
  # producing data for sparklines plot
  pdplot <- ret
  pdplot[,x:=as.numeric(factor(xname))]
  pdplot[,y:=rep(rep(1:n, each=length(unique(pdplot$ci))), length(xvars))]
  setkey(pdplot, y,x)
  pdplot[,preddiff:=pred-shift(pred), by=y] # getting absolute pred of change
  pdplot[,prednorm:=(pred-min(pred))/(max(pred)-min(pred)), by=y] # normalizing pred size

  retL <- list(mediantab=data.frame(ret),
               pdplot=data.frame(pdplot),
               cilev=data.frame(ci),
               n=n
               )
  
  return(retL)
}

# getting data ready for ggplot bubble plot

pdL <- partialDepAll(model=rf, df=df, n=20)

partialDepSpark <- function(pdL, yaxis.fixed=T, sizeByObs=T) {
  tmpdf <- data.table(pdL$pdplot)
  tmpdf <- tmpdf[ci==0.5,]
  n <- pdL$n
  tmpdf[,setlwd:=if(sizeByObs==T) cnt else 10]
  ggplot(tmpdf) +
    geom_line(aes(x=y, y=pred, colour=pred, lwd=setlwd)) +
    geom_rug(aes(x=n*(pred-min(pred))/(max(pred)-min(pred)))) + 
    scale_colour_gradientn(colours=colorRampPalette(c('firebrick', 'grey', 'forestgreen'))(n)) +
    facet_wrap(~xname, scales=ifelse(yaxis.fixed==T, 'fixed', 'free')) +
    theme_bw()
}

partialDepSpark(pdL, yaxis.fixed=T) + ggtitle('spark lines')



if(1==0){
  ## bubble plot
  ggplot(pdplot, aes(x=x, y=y, colour=abs(prednorm))) +
    geom_point(aes(size=pred*40), shape=19) +
    #scale_colour_gradientn(colours=colorRampPalette(c('gray', 'red'))(10)) +
    scale_colour_gradient2(low='firebrick', mid='gray', high='forestgreen') + 
    scale_size_identity() +
    theme(panel.grid.major=element_line(linetype=2,color="black"),
          axis.text.x=element_text(angle=90,hjust=1,vjust=0))
}



