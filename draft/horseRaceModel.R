#' @title 
#' @description This function performs repeated sub-sampling cross-validation using 3 different models and
#' saves the AUC and lift for each model at each iteration.  Models used are Random Forest, LASSO Regression, logistic regression (using 
#' LASSO for variable selection).  An ensemble of the LASSO and Random Forest is also included. 
#' @param x data.frame or data.table of predictor variables
#' @param y vector target variable, binary 0 or 1. 
#' @param n number, number of iterations
#' @param trainpct numeric scalar.  percent of data to be used for training at each iteration.
#' @param liftQuantile numeric scalar.  quantile to be used for assessing lift.
#' @return horseRace S3 object.
#' @export
#' @examples
#' mtcars2 <- mtcars
#' for(i in 1:ncol(mtcars2)) mtcars2[sample(nrow(mtcars2), sample(1:5,1), replace=T),i] <- NA
#' missdf(mtcars2)

library('glmnet')
library('randomForest')
library('Rsenal')
library('pROC')


horseRaceModel <- function(x, y, n=10, trainPct=0.75, liftQuantile=.04) {
  
  if(class(y) %in% c('character', 'factor')) yn <- as.numeric(as.character(y)) else yn <- y
  if(max(y)>1 | min(y)<0) stop('y must be between 0 and 1') 
  
  pb <- txtProgressBar(min=0, max=n, style=3, width=n)
  
  # setting up return object
  ret <- replicate(2, data.frame(rf=rep(NA, n), lasso=rep(NA, n), glm=rep(NA, n), ensemble=rep(NA,n)), simplify=F)
  names(ret) <- c('lift', 'auc')
  ret[['lasso_coef']] <- NULL
  
  for(i in 1:n) {
    itrain <- sample(1:nrow(x), round(nrow(x)*trainPct))
    itest <- setdiff(1:nrow(x), itrain)
    xtr <- x[itrain,]
    xte <- x[itest,]
    ytr <- y[itrain]; yntr <- yn[itrain]
    yte <- y[itest]; ynte <- yn[itest]
    
    # if we have factor variables, dummy up columns for LASSO
    if(length(setdiff(sapply(xtr, class), c('numeric', 'integer')))>0) {
      xtr_d <- model.matrix(~.-1, xtr)
      xte_d <- model.matrix(~.-1, xte)
      colnames(xtr_d) <- gsub('\\-', '\\_', colnames(xtr_d))
      colnames(xte_d) <- gsub('\\-', '\\_', colnames(xte_d))
      facvars <- names(attr(xtr_d, "contrasts"))
    }
    
    ## LASSO
    fitlasso <- glmnet(x=as.matrix(xtr_d), y=yntr, alpha=1, family='binomial')
    cvLasso <- cv.glmnet(x=as.matrix(xtr_d), y=yntr, alpha=1, type.measure='deviance')
    predLasso <- predict(fitlasso, s=cvLasso$lambda.min, newx=as.matrix(xte_d), type='response')[,1]
    pqLasso <- predQuantile(ytest=ynte, testPred=predLasso, n=round(1/liftQuantile))
    
    ## Logistic
    xvar_toplasso <- row.names(coef(fitlasso))[which(abs(coef(fitlasso, s=cvLasso$lambda.min))>0)] # 
    df4glm <- data.frame(y=ytr, data.frame(xtr)[,intersect(colnames(xtr), xvar_toplasso), drop=FALSE])
    fitglm <- glm(makeForm('y', intersect(colnames(xtr), xvar_toplasso)), data=df4glm, family=binomial(logit))
    predGlm <- predict(fitglm, newdata=xte, type='response')
    pqGlm <- predQuantile(ytest=ynte, testPred=predGlm, n=round(1/liftQuantile))
    
    ## RANDOM FOREST
    rf <- randomForest(x=xtr, y=factor(ytr), ntree=1000)
    predRf <- predict(rf, newdata=xte, type='prob')[,2]
    pqRf <- predQuantile(ytest=ynte, testPred=predRf, n=round(1/liftQuantile))
    
    ## ENSEMBLE
    predRfLasso <- (predRf+predLasso)/2
    pqRfLasso <- predQuantile(ytest=ynte, testPred=predRfLasso, n=round(1/liftQuantile))
    
    ## Saving Results
    ret$lift[i, 'rf'] <- pqRf$cumLift[1]
    ret$lift[i, 'lasso'] <- pqLasso$cumLift[1]
    ret$lift[i, 'glm'] <- pqGlm$cumLift[1]
    ret$lift[i, 'ensemble'] <- pqRfLasso$cumLift[1]
    ret$lift[i, 'iteration'] <- i
    
    ret$auc[i, 'rf'] <- glmnet::auc(y=ynte, prob=predRf)
    ret$auc[i, 'lasso'] <- glmnet::auc(y=ynte, prob=predLasso)
    ret$auc[i, 'glm'] <- glmnet::auc(y=ynte, prob=predGlm)
    ret$auc[i, 'ensemble'] <- glmnet::auc(y=ynte, prob=predRfLasso)
    ret$auc[i, 'iteration'] <- i
    
    if(i==1) {
      ret$lasso_coef <- as.matrix(coef(fitlasso, s=cvLasso$lambda.min))
    } else {
      ret$lasso_coef <- cbind(ret$lasso_coef, as.matrix(coef(fitlasso, s=cvLasso$lambda.min))) 
    }
    
    setTxtProgressBar(pb, i)
  }
  
  ret$lasso_coef <- as(ret$lasso_coef, 'dgCMatrix')
  
  class(ret) <- 'horseRace'
  attr(ret, 'n') <- n
  
  return(ret)
}


plot.horseRace <- function(object, measure){
  if (!inherits(object, "horseRace")) 
    stop("object not of class horseRace")
  if (is.null(object[[measure]])) 
    stop("Measure argument not recognized")
  
  ggdf <- data.table(melt(object[[measure]], id='iteration'))
  setnames(ggdf, 'variable', 'model')
  ggplot(data=ggdf, aes(x=iteration, y=value, group=model, color=model)) + geom_line() +
    geom_point(size=4, shape=19) + ylab(measure) + ggtitle(measure) + theme_bw()
}

summary.horseRace <- function(object){
  cat('AUC: \n')
  dt <- data.frame(data.table(hr[['auc']])[,.(rf, lasso, glm, ensemble)][,lapply(.SD, function(x) quantile(x)[2:4])])
  row.names(dt) <- c('25%', '50%', '75%')
  print(dt)
  
  cat('\n')
  cat('Lift: \n')
  dt <- data.frame(data.table(hr[['lift']])[,.(rf, lasso, glm, ensemble)][,lapply(.SD, function(x) quantile(x)[2:4])])
  row.names(dt) <- c('25%', '50%', '75%')
  print(dt)
  
  cat('\n')
  cat('variables chosen by LASSO: \n')
  lassoB <- apply(as(hr$lasso_coef, 'matrix'), 1, function(x) sum(x>0))
  lassoB <- lassoB[setdiff(names(lassoB), '(Intercept)')]
  data.frame(N=lassoB[order(lassoB, decreasing=T)])
}
summary(hr)

#' @examples
#' \dontrun{
library('arules') # for grabbing AdultUCI dataset
library('data.table')
library('ggplot2')
data(AdultUCI)
df <- AdultUCI[1:1000,]
df <- df[complete.cases(df),]
names(df) <- gsub('\\-', '_', names(df)) # dashes mess things up
# still need to handle categorical variables
hr <- horseRaceModel(x=df[,c('age', 'education_num', 'capital_gain', 'capital_loss', 'hours_per_week', 'race', 'workclass')], 
                     y=as.numeric(df$sex)-1, n=8, trainPct=.75, liftQuantile=.1)

plot(hr, measure='lift')
summary(hr)

#setdiff(names(df), 'sex')
#c('capital_gain', 'hours_per_week','age')


#' }




