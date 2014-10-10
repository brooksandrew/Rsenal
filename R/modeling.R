#' @title Create formula
#' @description Creates a formula object which can be passed to many R modeling functions from a vector of variable names.  
#' @param y character string, target variable
#' @param xList character vector, predictor variables (Right Hand Side variables) for formula
#' @return formula object
#' @export
#' @examples
#' form <- makeForm('mpg', c('drat', 'wt', 'hp'))
#' summary(lm(form, data=mtcars))

makeForm <- function(y, xList) {
  if(as.character(y)[1]=='as.factor') y<-paste(paste(as.character(y), collapse='('), ')', sep='')
  form <- (as.formula(paste(y, '~', paste(xList, collapse='+'), sep='')))
  return(form)
}

#' @title Edit a formula
#' @description Easy way to add or subtract predictor variables to a formula 
#' @param form formula, the formula you will be editing
#' @param sub character vector, predictor variables to subtract (remove) from formula
#' @param add character vector, predictor variables to add to formula
#' @return formula, edited formula object
#' @export
#' @examples
#' form <- as.formula('mpg~cyl+hp+drat+qsec')
#' editForm(form, add=c('wt', 'gear', 'carb'))
#' editForm(form, sub=c('cyl', 'hp', 'qsec', 'variableNotInFormula'))
#' editForm(form, sub=c('cyl', 'hp', 'qsec'), add=c('wt'))

editForm <- function(form, add=NULL, sub=NULL){
  eform <- form
  X <- unlist(strsplit(as.character(form)[[3]], ' +'))
  X <- X[X!='+']
  Y <- form[[2]]
  
  if(is.null(sub)==F){
    Xsub <- X[!X %in% sub]
    eform <- makeForm(Y,Xsub)
  }
  
  if(is.null(add)==F){
    if('Xsub' %in% ls()) X <- Xsub
    Xadd <- c(X, add)
    eform <- makeForm(Y,Xadd)
  }
  
  return(eform)
}

shuffleForm <- function(form){
  eform <- form
  X <- unlist(strsplit(as.character(form)[[3]], ' +'))
  X <- X[X!='+']
  X <- X[sample(1:length(X), replace=F)]
  Y <- form[[2]]
  shuffledForm <- makeForm(Y, X)
  return(shuffledForm)
}

#' @title Variable importance from iteratively shuffled orders of predictor variables
#' @description Runs multiple anova analyses to assess deviance explained by each predictor in shuffled orders, iteratively.
#' Currently using only logistic regression.  Could be generalized.
#' @param form formula to be passed to \code{\link{travCount}}.
#' @param df data.frame of data to be used for analysis
#' @param n number of iterations for shuffled ANOVA analysis
#' @param test character string, statistical test to run.  Default is 'Chisq'.
#' @return data.frame of results
#' @export
#' @examples
#' form <- as.formula('am~wt+gear+carb+cyl+hp+drat+qsec')
#' shuffleAnova(form, mtcars, n=50)

shuffleAnova <- function(form, df, n=5, test='Chisq'){
  # creating a list of all anova analyses (with shuffled order of x variables)
  anovaL <- list()
  for(i in 1:n){
    sform <- shuffleForm(form)
    anovaL[[i]] <- anova(glm(sform, data=df, family=binomial(logit)), test=test)
  }
  
  # create a summary table from running multiple anova analyses with different subsets of variables 
  pred <- unique(unlist(lapply(anovaL, function(x) unique(rownames(x)))))
  res <- data.frame(xvar=pred)
  for(i in pred){
    res$deviance_mean[res$xvar==i] <- mean(unlist(lapply(anovaL, function(x) x[rownames(x)==i,2])))
    res$deviance_median[res$xvar==i] <- median(unlist(lapply(anovaL, function(x) x[rownames(x)==i,2])))
    res$deviance_sd[res$xvar==i] <- sd(unlist(lapply(anovaL, function(x) x[rownames(x)==i,2])))
    res$deviance_min[res$xvar==i] <- min(unlist(lapply(anovaL, function(x) x[rownames(x)==i,2])))
    res$deviance_max[res$xvar==i] <- max(unlist(lapply(anovaL, function(x) x[rownames(x)==i,2])))
    res$pvalue_median[res$xvar==i] <- median(unlist(lapply(anovaL, function(x) x[rownames(x)==i,5])), na.rm=T)
  }
  p <- cut(res$pvalue_median, breaks=c(0,.001,.01,.05,.1, 1))
  res$pstar <- factor(x=p, levels=levels(p), labels=c('***', '**', '*', '.', ''))
  res <- res[order(res$deviance_mean, decreasing=T),]
  return(res)
}

#' @title Format logistic regression table
#' @description Turns a logistic regression object into a regression table
#' and outsheets it to a csv if you choose
#' @param reg logistic regression object.  output from \code{\link{glm}}
#' @param file filepath of ouput.  \code{txt} or \code{csv} object.
#' @param xvlab data.frame, lookup table for variable names.  First column is codename, second column is the pretty printed name.
#' @param stats include statistics in output
#' @return data.frame of results formatted nicely
#' @export
#' @examples
#'   reg <- glm(am~qsec+hp, data=mtcars, family=binomial(logit))
#' logit2tab(reg)  
#' longnames <- data.frame(short = c('wt', 'mpg', 'cyl', 'drat', 'hp', 'am', 'qsec'),
#' long = c('Weight', 'Miles Per Gallon', 'Cylinder', 'D.R.A.T', 'Horsepower', 'A.M.', 'Q Seconds'))
#' logit2tab(reg, xvlab=longnames, stats=T)

logit2tab <- function(reg, file=NULL, xvlab=NULL, stats=F){
  tab <- summary(reg)$coefficients
  tab <- data.frame(coef=tab[,'Estimate'], oddsRatio=exp(tab[,'Estimate']), stdError=tab[,'Std. Error'], p=tab[,'Pr(>|z|)'], stringsAsFactors=F)
  p <- cut(tab$p, breaks=c(0,.001,.01,.05,.1, 1))
  tab$pstar <- factor(x=p, levels=levels(p), labels=c('***', '**', '*', '.', ''))
  if(is.null(xvlab)==F) {
    tab$Metric <- as.character(xvlab[match(rownames(tab), xvlab[,1]), 2])
    tab$Metric[is.na(tab$Metric)] <- ''
    tab$Metric[rownames(tab)=='(Intercept)'] <- '(Intercept)'
    tab <- tab[, c('Metric', names(tab)[!names(tab) %in% 'Metric'])]
  }
  if(is.null(file)==F) write.csv(tab, file=file) 
  if(stats==T){
    tab[nrow(tab)+1, ] <- ''; rownames(tab)[nrow(tab)] <- ''
    tab[nrow(tab)+1,1:2] <- c('aic', reg$aic)
    tab[nrow(tab)+1,1:2] <- c('n', nrow(reg$model))
    tab[nrow(tab)+1,1:2] <- c('Chi Sq.', reg$null.deviance - reg$deviance)
    tab[nrow(tab)+1,1:2] <- c('P(>|Chi|)', pchisq(reg$null.deviance-reg$deviance, 2, lower.tail=F, df=reg$df.null-reg$df.residual))
    tab[is.na(tab)] <- ''
  }
  return(tab)
}

#' @title All-subsets logistic regression
#' @description Plots the output of the bestglm BestModels object. Similar to the visual output of plotting a regsubsets object
#' @param bglm \code{bestglm} object from \code{\link{bestglm}}
#' @param rc decimal places to display on y-axis of plot
#' @return plot
#' @seealso  \code{\link{bestglm}},  \code{\link{leaps}}
#' @import bestglm
#' @export
#' @examples
#' require('bestglm')
#' b <- bestglm(Xy=mtcars[,c('mpg', 'hp', 'drat', 'cyl', 'wt', 'qsec', 'vs')], family=binomial(logit), IC='BIC', nvmax=4)
#' plotBestglm(b$BestModels, rc=3)

plotBestglm <- function(bglm, rc=2) {
  image(as.matrix(t(bglm[,1:ncol(bglm)-1])), col=c(0:1), xaxt='n', yaxt='n')
  axis(1, at=seq(0,1,length.out=ncol(bglm)-1), labels=colnames(bglm)[1:ncol(bglm)-1], las=2)
  axis(2, at=seq(0,1,length.out=nrow(bglm)), labels=rev(round(bglm$Criterion,rc)), las=2)
}


#' @title Univariate glm regression
#' @description Runs a univariate logistic regression on each predictor variable of interest.
#' @param df data.frame with variables for analysis
#' @param yv character string, target variable
#' @param xv character vector, predictor variables to test univariately
#' @param file character string, filepath to write results out to.  txt or csv file.
#' @param sortby character string, criteria to sort variables by. Default  = 'aic'
#' @param xvlab data.frame, lookup table for variable names.  First column is codename, second column is the pretty printed name.
#' @param test \code{TRUE} or \code{FALSE}.  Includes Chi square test, or not.
#' @return data.frame of results
#' @seealso  \code{\link{bestglm}},  \code{\link{leaps}}
#' @import Hmisc
#' @export
#' @examples
#'
#' require('Hmisc')
#' 
#' ##setting up some data   
#' longnames <- data.frame(long = c('Weight', 'Miles Per Gallon', 'Cylinder', 'D.R.A.T', 'Horsepower', 'A.M.'),
#' short = c('wt', 'mpg', 'cyl', 'drat', 'hp', 'am'), stringsAsFactors=F)
#'
#' glm.out <- uniglm(df=mtcars, yv='vs', xv=c('hp','drat','cyl','mpg','wt'), xvlab=longnames)

uniglm <- function(df, yv, xv, file=NULL, sortby='aic', xvlab=NULL, test=T){
  mat <- data.frame(matrix(nrow=25*length(xv), ncol=11, dimnames=list(NULL, c('Predictor', 'Name','ref', 'coef', 'oddsRatio', 'p', 'pstar', 'aic', 'c', 'Chisq_pvalue', 'Chisq_pstar'))))
  i<-0
  for(x in xv){
    formt <- makeForm(yv, x)
    reg <- glm(formt, data=df, family=binomial(logit))
    for(j in 2:length(reg$coefficients)){
      i<-i+1
      try({
        mat$Predictor[i] <- names(reg$coefficients[j])
        
        ## If we have nice clean labels to replace variable names in code
        if(is.null(xvlab)==F) {mat$Name[i] <- as.character(xvlab[match(x, xvlab[,2]), 1])
        } else {mat=mat[, setdiff(names(mat), 'Name')]}
        
        ## If we have a categorical variable with a reference 
        if(length(reg$coef)>2) {
          regv <- gsub(x, '', names(reg$coef))
          uxv <- unique(df[,x])
          mat$ref[i] <- setdiff(uxv, regv)
        } 
        
        if(test==T){
          av <- anova(reg, test='Chisq')['Pr(>Chi)'][[1]][2]
          mat$Chisq_pvalue[i] <- av
          avp <- cut(mat$Chisq_pvalue[i], breaks=c(0,.001,.01,.05,.1, 1))
          mat$Chisq_pstar[i] <- as.character(factor(x=avp, levels=levels(avp), labels=c('***', '**', '*', '.', '')))
        } else {mat=mat[, setdiff(names(mat), c('Chisq_pvalue', 'Chisq_pstar'))]}
        
        mat$coef[i] <- as.numeric(reg$coefficients[j])
        mat$oddsRatio[i] <- as.numeric(exp(reg$coefficients[j]))
        mat$p[i] <- as.numeric(summary(reg)$coefficients[j,4])
        p <- cut(mat$p[i], breaks=c(0,.001,.01,.05,.1, 1))
        mat$pstar[i] <- as.character(factor(x=p, levels=levels(p), labels=c('***', '**', '*', '.', '')))
        mat$aic[i] <- reg$aic
        mat$c[i] <- round(somers2(x=reg$fitted.values, y=reg$y)['C'],3)
      })
    }
    
  }
  mat<-mat[1:i,]
  mat$ref[is.na(mat$ref)] <- ''
  if(sum(mat$ref=='')==nrow(mat)) mat <- mat[, setdiff(names(mat), 'ref')]
  decreasingTF <- ifelse(sortby %in% c('c'), T, F)
  mat <- mat[order(mat[,sortby], decreasing=decreasingTF),] 
  if(is.null(file)==F) write.csv(mat, file=file)
  return(mat)
}

#' @title Analyze predictions of supervised model in quantiles
#' @description Analyzing results of supervised model using test data and quantiles.  Note only
#' \code{model} or \code{testPred} are necessary, not both. \code{testPred} is advised.
#' @param model the model to get predictions from.  Uses \code{predict} method.  If \code{testPred}
#' is specified, we don't need this parameter,  Probably safer to just use testPred, unless the model you have
#' has a predict method that will work as intended without any other arguments.
#' @param xtext data.frame of test data (predictor variables only)
#' @param ytest vector of test data (target variable)
#' @param n number of quantiles
#' @param roundText decimals to print
#' @param testPred vector of predictions from test data.  Do not use if using \code{model} parameter.
#' @param fw numeric vector, of bins.  Defaults to 
#' @return data.frame of results
#' @seealso  \code{\link{predSortPlot}}
#' @export
#' @examples
#' require('randomForest')
#' rf <- randomForest(x=mtcars[1:25,1:8], y=as.factor(mtcars[1:25, 'am']), ntree=5)
#' mtcarsTestPred <- predict(rf, mtcars[26:32, 1:8], type='prob')[,2]
#' pq2 <- predQuantile(xtest=mtcars[26:32, 1:8], ytest=mtcars[26:32, 'am'], n=3, roundText=4, testPred=mtcarsTestPred)
#' pq <- predQuantile(xtest=mtcars[26:32, 1:8], ytest=mtcars[26:32, 'am'], fw=seq(.1,1,length.out=10), roundText=4, testPred=mtcarsTestPred)
#'
#' barplot(pq$actual, names=pq$predRange, ylab='True Positives', las=2, cex.names=.7)
#' barplot(pq$hitRate, names=pq$predRange, ylab='True Positive Hit Rate', las=2, cex.names=.7)
#' barplot(pq$predMax-pq$predMin, pq$hitRate, names=pq$predRange, ylab='prediction', las=2, cex.names=.7)

predQuantile <- function(model=NULL, xtest=NULL, ytest, n=5, roundText=3, testPred=NULL, fw=NULL){
  if(is.null(testPred)) {pred <- predict(model, xtest)
  } else {pred <- testPred}
  
  tf <- data.frame(pred=pred, actual=ytest)
  tf <- tf[order(tf$pred),]
  if(is.null(fw)) {tf$cat <- sort(rep(1:n, ceiling(nrow(tf)/n))[1:nrow(tf)])
  } else {tf$cat <- cut(tf$pred, breaks=fw)}
  
  aggSum <- aggregate(actual~cat, data=tf, sum)
  aggLength <- aggregate(actual~cat, data=tf, length)
  aggRange <- aggregate(pred~cat, data=tf, range)
  aggRange <- cbind(aggRange[,1], data.frame(aggRange[,2]))
  aggRangeText <- aggregate(pred~cat, data=tf, function(x) paste(round(range(x), roundText), collapse=' to '))
  
  names(aggLength)[2] <- 'obs'
  names(aggRange) <- c('cat', 'predMin', 'predMax')
  names(aggRangeText)[2] <- 'predRange'
  
  agg <- merge(aggSum, aggLength, by='cat')
  agg$hitRate <- agg$actual/agg$obs
  agg <- merge(agg, aggRange, by='cat')
  agg <- merge(agg, aggRangeText, by='cat')
  
  agg$cumHitsPct <- rev(cumsum(rev(agg$actual)/sum(agg$actual)))
  agg$cumHitRate <- rev(cumsum(rev(agg$actual))/cumsum(rev(agg$obs)))
  agg$cumObsPct <- rev(cumsum(rev(agg$obs)/sum(agg$obs)))
  
  ##correcting for cutoffs with zero
  if(!is.null(fw)) {
    a<-paste(levels(tf$cat)[!levels(tf$cat) %in% tf$cat], collapse=', ')
    print(paste('no scores in these ranges',a))
    agg$cutoff <- fw[which(levels(tf$cat) %in% tf$cat)]
  }
  return(agg)
}

#' @title Devil's Horn
#' @description Visualize results of supervised model predictions on test data.  Currently supports binary target variable.  
#' Red dots represent observations where target variable = 1, black dots where target variable = 0.
#' @param pred vector of predictions for target variable on test data
#' @param ytest, vector of the target variable from test data. (0s and 1s)
#' @param jitterPlot \code{TRUE} or \code{FALSE}. Jitters points on plot when \code{TRUE}
#' @return plot
#' @seealso  \code{\link{predQuantile}}
#' @export
#' @examples
#' ## Setting up some data and building a basic model on training data.
#' mylogit <- glm(vs~drat+hp+mpg, family=binomial('logit'), data=mtcars[1:25,])
#' mtcarsTestPred <- predict(mylogit, mtcars[26:32, ], type='response')
#' predSortPlot(pred=mtcarsTestPred, ytest=mtcars$vs[26:32])

predSortPlot <-  function(pred, ytest, jitterPlot=NULL) {
  tf <- data.frame(pred=pred, actual=ytest)
  tf <- tf[order(tf$pred),]
  #return(tf)
  if(is.null(jitterPlot)==T) plot(tf$pred, col=ifelse(tf$actual==1, 'red', 'black'), cex=ifelse(tf$actual==1, 3, 2), ylab='Prediction')
  if(is.null(jitterPlot)==F) plot(jitter(tf$pred, jitterPlot), col=ifelse(tf$actual==1, 'red', 'black'), pch=ifelse(tf$actual==1, 19, 21), ylab='Prediction')
}


#' @title Optimize weight on ensemble of 2 supervised models
#' @description This function creates a weighted average of predictions from two models
#' and evaluates F1, precision, recall, auc or c for each combination of the 
#' models to determine the best weights for each.
#' @param pred1 numeric vector of probabilities, prediction from model 1
#' @param pred2 numeric vector of probabilities, prediction from model 2
#' @param actual vector of 1s and 0s.  The target variable test data
#' @param steps number, high numbers compute a more exhaustive combination of model weights
#' @param cutoff Cutoff used to demarcate predictions into positive or negative class.
#' @param ytest, vector of the target variable from test data. (0s and 1s)
#' @param jitterPlot \code{TRUE} or \code{FALSE}. Jitters points on plot when \code{TRUE}
#' @return data.frame of results
#' @import Hmisc  
#' @export
#' @examples
#' require('Hmisc')
#' fit_glm1 <- glm(am~cyl, data=mtcars, family=binomial(logit))
#' fit_glm2 <- glm(am~disp, data=mtcars, family=binomial(logit))
#' ow <- optimizeModelWeight(fit_glm1$fitted.values, fit_glm2$fitted.values, actual=fit_glm1$model$am)  
#' plot(ow$weights, ow$precision, type='l', xlab='weight on model 1')


optimizeModelWeight <- function(pred1, pred2, actual, steps=50, cutoff=.5) {
  s <- seq(0,1,length.out=steps)
  df <- data.frame(weights=s, c=NA, precision=NA, recall=NA, f1=NA, f.5=NA, stringsAsFactors=F)
  j <- 1
  for(i in s){
    mds <- (i)*pred1 + (1-i)*pred2
    
    df$c[j] <- somers2(mds,actual)['C']
    df$precision[j] <- sum(actual[mds>cutoff])/(length(actual[mds>cutoff]))
    df$recall[j] <- sum(actual[mds>cutoff])/(sum(actual))
    df$f1[j] <- (2*df$precision[j]*df$recall[j])/(df$precision[j]+df$recall[j])
    df$f.5[j] <- ((1+.5^2)*df$precision[j]*df$recall[j])/(((.5^2)*df$precision[j])+df$recall[j])
    j<-j+1
  }
  return(df)
}







