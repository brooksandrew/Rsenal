\name{predQuantile}
\alias{predQuantile}
\title{Analyze predictions of supervised model in quantiles}
\usage{
  predQuantile(model = NULL, xtest = NULL, ytest, n = 5,
    roundText = 3, testPred = NULL, fw = NULL)
}
\arguments{
  \item{model}{the model to get predictions from.  Uses
  \code{predict} method.  If \code{testPred} is specified,
  we don't need this parameter, Probably safer to just use
  testPred, unless the model you have has a predict method
  that will work as intended without any other arguments.}

  \item{xtext}{data.frame of test data (predictor variables
  only)}

  \item{ytest}{vector of test data (target variable)}

  \item{n}{number of quantiles}

  \item{roundText}{decimals to print}

  \item{testPred}{vector of predictions from test data.  Do
  not use if using \code{model} parameter.}

  \item{fw}{numeric vector, of bins.  Defaults to}
}
\value{
  data.frame of results
}
\description{
  Analyzing results of supervised model using test data and
  quantiles.  Note only \code{model} or \code{testPred} are
  necessary, not both. \code{testPred} is advised.
}
\examples{
require('randomForest')
rf <- randomForest(x=mtcars[1:25,1:8], y=as.factor(mtcars[1:25, 'am']), ntree=5)
mtcarsTestPred <- predict(rf, mtcars[26:32, 1:8], type='prob')[,2]
pq2 <- predQuantile(xtest=mtcars[26:32, 1:8], ytest=mtcars[26:32, 'am'], n=3, roundText=4, testPred=mtcarsTestPred)
pq <- predQuantile(xtest=mtcars[26:32, 1:8], ytest=mtcars[26:32, 'am'], fw=seq(.1,1,length.out=10), roundText=4, testPred=mtcarsTestPred)

barplot(pq$actual, names=pq$predRange, ylab='True Positives', las=2, cex.names=.7)
barplot(pq$hitRate, names=pq$predRange, ylab='True Positive Hit Rate', las=2, cex.names=.7)
barplot(pq$predMax-pq$predMin, pq$hitRate, names=pq$predRange, ylab='prediction', las=2, cex.names=.7)
}
\seealso{
  \code{\link{predSortPlot}}
}

