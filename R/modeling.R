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

