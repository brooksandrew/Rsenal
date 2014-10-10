#' @title Merge edgelists => Master edgelist => igraph
#' 
#' @description This function turns a list of edgelists (data.frames) into a single "master" edgelist.
#' It maintains the edge attributes of each individual edge list
#' 
#' @param edgeLists list of edgelists (stored as data.frames)
#' @param from name of the column that specifies the "from" node in each data.frame in \code{edgeLists}
#' @param to name of the column that specifies the "to" node in each data.frame in \code{edgeLists}
#' @param keepDups deletes duplicate from-to relations when set to FALSE
#' @export
#' @examples
#' require('igraph')
#' cars <- data.frame(mtcars, to=sample(rownames(mtcars), replace=T), from=sample(rownames(mtcars), replace=T))
#' df1<-cars[sample(1:32,30), c('to', 'from', 'cyl', 'mpg')]
#' df2<-cars[sample(1:32,15), c('to', 'from', 'cyl', 'qsec')]
#' df3<-cars[sample(1:32,32), c('to', 'from', 'hp', 'drat')]
#' 
#' df1$cyl[1:10] <- df1$cyl[1:10]+2
#' el <- list(df1, df2, df3)
#' 
#' mel <- mergeEdgeLists(el, from='from', to='to', keepDups=T)

mergeEdgeLists <- function(edgeLists, from='from.ID', to='to.ID', keepDups=F){
  n <- length(edgeLists)
  
  mel <- edgeLists[[1]]
  for(i in 2:n){
    mel <- merge(mel, edgeLists[[i]], by=c(to, from), all=T, suffixes=c(paste('.',i-1, sep=''), paste('.',i, sep='')))
    dupVars <- intersect(setdiff(names(edgeLists[[i]]), c(from, to)), setdiff(names(edgeLists[[i-1]]), c(from, to)))
    
    ## if we have to merge columns with repeat names
    for(d in dupVars){
      print(paste('merging columns with same name in multiple edgelists: ',d))
      dupVarName1 <- paste(d, '.',i-1, sep='')
      dupVarName2 <- paste(d, '.',i, sep='')
      mel[, d] <- mel[,dupVarName1]
      mel[is.na(mel[,d]), d] <- mel[is.na(mel[,d]), dupVarName2]
      nonmissingRows <- is.na(mel[,dupVarName1])==F & is.na(mel[,dupVarName2])==F
      if(sum(mel[nonmissingRows, dupVarName1]!=mel[nonmissingRows, dupVarName2])>0) print(paste('Warning!!! Some values of ', dupVarName1, ' and ', dupVarName2, ' do not match.', sep=''))
      if(keepDups==F) mel <- mel[,setdiff(names(mel), c(dupVarName1, dupVarName2))] 
    }
  }
  return(mel)
}
