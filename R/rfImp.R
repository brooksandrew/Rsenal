#' @title Sort Random Forest Variable Importance Features
#' @description Simple utility function to sort random forest variable importance features.  Very simple.
#' @param rf randomForest object from randomForest package
#' @return data.frame
#' @export
#' @examples
#' library('randomForest')
#' myrf <- randomForest(iris[1:4], iris$Species)
#' rfImp(myrf)

rfImp <- function(rf) {
  df <- data.frame(rf$importance[order(rf$importance, decreasing=T),,drop=F])
  df$variable <- row.names(df)
  df <- df[,c(2,1)]
  row.names(df) <- NULL
  return(df)
}