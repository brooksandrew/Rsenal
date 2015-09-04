library('randomForest')
df <- mtcars
df$vs <- factor(df$vs)
rf <- randomForest(vs~mpg+cyl+drat+qsec, df, ntrees=10)
#reg <- lm(mpg~vs+cyl+drat+qsec, df)

## Calculate Partial Dependence Plot for classification.  one X variable at a time.

model <- rf; df<-df ;xvar <- 'qsec'; n<-3; target.class='1' # parameter assignment for running without function
partialDep <- function(model, df, xvar, n=10, target.class='1') {
  xv <- df[,xvar]
  xvr <- seq(min(xv), max(xv), length.out=n)
  dfb <- df[rep(1:nrow(df), each=n), ]
  dfb[,xvar] <- rep(xvr, nrow(df))
  
  pred <- predict(model, dfb, type='prob')[,target.class]
  
  retdf <- data.frame(xvar=dfb[,xvar], pred=pred)
  ret <- aggregate(pred~xvar, retdf,mean)
  return(ret)
}

pd  <- partialDep(rf, df, xvar='qsec', n=100)
plot(pd[,1], pd[,2], type='l')

library('data.table')
model <- rf; df<-df ;xvars <-  names(model$forest$ncat) ; n<-3; target.class='1' # parameter assignment for running without function
i <- 'mpg'
partialDepAll <- function(model, df, n=10, xvars=NULL, target.class='1') {
  # currently only works with random forest
  if(is.null(xvars)) xvars <- names(model$forest$ncat)
  ret <- matrix(nrow=length(xvars), ncol=n)
  row.names(ret) <- xvars
  for(i in xvars){
    print(i)
    pd <- data.table(partialDep(model=model, df=df, xvar=i, n=n, target.class=target.class))
    agg <- pd[,mean(pred), by=xvar]
    ret[row.names(ret)==i] <- agg[[2]]
  }
  return(data.frame(ret))
}

# getting data ready for ggplot bubble plot
(pdall  <- data.table(partialDepAll(rf, df, n=10)))
pdplot <- melt(pdall)
pdplot[,x:=as.numeric(variable)]
pdplot[,y:=rep(1:length(xvars), n)]
setkey(pdplot, y,x)
pdplot[,valuediff:=value-shift(value), by=y] # getting absolute value of change
pdplot[,valuenorm:=(value-min(value))/(max(value)-min(value)), by=y] # normalizing value size


library('ggplot2')
ggplot(pdplot,aes(x,y))+
  geom_point(aes(size=(valuenorm+.15)*30), shape=21, fill="white")+
  scale_size_identity()+
  theme(panel.grid.major=element_line(linetype=2,color="black"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0))

