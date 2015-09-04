library('randomForest')
df <- mtcars
df$vs <- factor(df$vs)
rf <- randomForest(vs~mpg+cyl+drat+qsec+disp+gear+carb+hp, df, ntrees=10)
#reg <- lm(mpg~vs+cyl+drat+qsec, df)

## Calculate Partial Dependence Plot for classification.  one X variable at a time.
model <- rf; df<-df; xvar <- 'qsec'; n<-5; target.class='1'; ci=c(.9,.5,.3)


# NEED TO COUNT OBS PER BIN for rug plot
partialDep <- function(model, df, xvar, n=10, target.class='1', ci=c(.9,.5,.3)) {
  if(!class(df) %in% 'data.table') df <- data.table(df)
  xv <- df[,get(xvar)] # x vector of interest
  xvr <- seq(min(xv), max(xv), length.out=n) # fixed width break points
  dfb <- df[rep(1:.N, each=n), ] # creating replicate of data for each 
  dfb[,xvar] <- rep(xvr, nrow(df)) # replacing x variable of interest with range of x var
  
  #table(cut(df[,xvar], breaks=xvr, include.lowest = T))
  
  pred <- predict(model, dfb, type='prob')[,target.class]
  
  retdf <- data.table(xvar=dfb[,get(xvar)], pred=pred)
  ci = c(0.5+ci/2, 0.5-ci/2)
  ret <- retdf[,.(pred=quantile(pred, ci)), by='xvar'][,ci:=rep(ci, n)][,cilev:=abs(ci-0.5)*2]
  return(ret)
}

pd  <- partialDep(rf, df, xvar='qsec', n=5)

plot(pd[,1], pd[,2], type='l')
library('data.table')
model <- rf; df<-df; xvars<-names(model$forest$ncat) ; n<-5; target.class='1'
i <- 'mpg'

partialDepAll <- function(model, df, n=10, xvars=NULL, target.class='1', ci=c(9,5,3)) {
  # currently only works with random forest
  if(is.null(xvars)) xvars <- names(model$forest$ncat)
  ret <- matrix(nrow=length(xvars), ncol=n)
  row.names(ret) <- xvars
  for(i in xvars){
    pd <- data.table(partialDep(model=model, df=df, xvar=i, n=n, target.class=target.class))
    ret[row.names(ret)==i] <- pd[[2]]
  }
  
  # producing data for sparklines plot
  pdall  <- data.table(ret)
  pdplot <- melt(pdall)
  pdplot[,x:=as.numeric(variable)]
  pdplot[,y:=rep(1:length(xvars), n)]
  pdplot[,varname:=xvars[y]]
  setkey(pdplot, y,x)
  pdplot[,valuediff:=value-shift(value), by=y] # getting absolute value of change
  pdplot[,valuenorm:=(value-min(value))/(max(value)-min(value)), by=y] # normalizing value size

  retL <- list(mediantab=data.frame(ret),
               pdplot=data.frame(pdplot),
               citab=data.frame(ci)
               )
  
  return(retL)
}

# getting data ready for ggplot bubble plot

pdL <- partialDepAll(model=rf, n=5, df=df)

partialDepSpark <- function(df, yaxis.fixed=T) {
  ggplot(df) +
    geom_line(aes(x=x, y=value, colour=value), lwd=4) +
    scale_colour_gradientn(colours=colorRampPalette(c('firebrick', 'grey', 'forestgreen'))(n)) +
    facet_wrap(~varname, scales=ifelse(yaxis.fixed==T, 'fixed', 'free')) +
    theme_bw()
}

partialDepSpark(pdL$pdplot, yaxis.fixed=T) + ggtitle('spark lines')




## bubble plot
ggplot(pdplot, aes(x=x, y=y, colour=abs(valuenorm))) +
  geom_point(aes(size=value*40), shape=19) +
  #scale_colour_gradientn(colours=colorRampPalette(c('gray', 'red'))(10)) +
  scale_colour_gradient2(low='firebrick', mid='gray', high='forestgreen') + 
  scale_size_identity() +
  theme(panel.grid.major=element_line(linetype=2,color="black"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0))




