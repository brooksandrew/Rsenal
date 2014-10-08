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




