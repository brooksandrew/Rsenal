#' @title Shiny app to visualize schema of relational tables
#' @description Launches a Shiny App that provides a visual representation of the relationships between a collection of tables 
#' (data.frames) with some relational structure.  If given a dump of dozens of flat files and without a formal schema or 
#' documentation on your data, this app will help explore and understand the underlying schema - which tables can be joined
#' which other tables, which variables can be used to join which tables, etc,  It also gives a read on how strongly each variable with 
#' the same name in multiple tables actually connects tables (how many of the values of the linking variable \code{x} that are in
#' \code{table1} are also in \code{table2}.  \cr \cr
#' @details Note, the Key-strength tables can be slow to display because these computations actually dive into the contents of the 
#' data.frames and perform set operations on every row of the variable of interest.  It is possible to use the \code{\link{isKey}} function
#' to compute these similarity matrices ahead of time to prevent the Shiny app from doing these computations each time.
#' @param dfL list of data.frames used to generate schema.  This is easily generated from \code{\link{dir2dfList}}
#' @seealso \code{\link{dir2dfList}} \code{\link{isKey}}
#' @return Shiny App
#' @import memoise shiny gplots igraph
#' @export
#' @examples
#' \dontrun{
#' ## download some baseball data. NOTE This will download 30MB of data (25 csv files) into a temporary directory
#' temp <- tempfile()
#' localDataDir <- paste0(tempdir(), '\\lahman2012-csv-onYourComp.zip')
#' download.file('http://seanlahman.com/files/database/lahman2012-csv.zip', localDataDir)
#' unzip(localDataDir, exdir=paste0(tempdir(), '\\lahman2012-csv-onYourComp')) ## may not be necessary
#' 
#' ## create a list of data.frames from .CSVs
#' dfL <- dir2dfList(paste0(tempdir(), '\\lahman2012-csv-onYourComp'), ext='.csv', exclude=NULL, sep=',', stringsAsFactors=F)
#' 
#' ## launch app
#' tableNet(dfL)
#' }


tableNet <- function(dfL) {
  
  ###############################################################################
  ## takes list of Data frames and makes igraph network  ########################
  ###############################################################################
  dfL2network <- function(dfL, islands=T){
    # create adjacency list
    edgeL <- data.frame(v1=as.character(), v2=as.character(), v3=as.character(), stringsAsFactors=F)
    k<-1
    for(i in 1:length(dfL)){
      for(j in 1:length(dfL)){
        commonv <- intersect(names(dfL[[i]]), names(dfL[[j]]))
        if(length(commonv)>0){
          for(cv in 1:length(commonv)){
            edgeL[k,1] <- names(dfL)[i]
            edgeL[k,2] <- names(dfL)[j]
            edgeL[k,3] <- commonv[cv]
            k<-k+1
          }
        }    
      }
    }
    
    ## creating graph from adjacency
    edgeL <- edgeL[edgeL[,1]!=edgeL[,2],]
    g <- graph.data.frame(edgeL[,c(1,2)], directed=F)
    if(islands==T) {
      xvars <- unlist(lapply(dfL, names),use.names=F)
      keys <- unique(xvars[duplicated(xvars)==T])
      islandtab <- names(dfL)[sapply(dfL, function(x) is.na(sum(names(x) %in% keys)))]
      g <- g + vertices(islandtab)
    }
    E(g)$color <- 'gray'
    E(g)$name <- edgeL[,3]
    
    return(g)
  }
 
  ## create schema network graph
  g <- dfL2network(dfL)
  
  ## create memo'ized version of isKey to has
  isKeym <- memoise(isKey)
    
  #######################################################
  ## ACTUALLY RUN APP ###################################
  #######################################################
  
  shinyApp(ui = shinyUI(pageWithSidebar(
    
    headerPanel("Database Schema Visualization"),
    
    ## NETWORK 
    sidebarPanel(
      conditionalPanel(
        condition="input.mytab=='network'",
        
        sliderInput("vlabcex", "Vertex label size:", value=1, min=.05, max=5, step=0.05),
        sliderInput("sizeNodesSlider", "Vertex size", value=10, min=0.1, max=30, step=0.1),
        sliderInput("ewidth", "Edge width:", value=1, min=1, max=10, step=1),
        selectInput("lay2", "Choose a layout", choices = c('circle', 'fruchterman.reingold')),
        selectInput("vcolor", "Color vertices by:", choices = c('# of keys', '# of connections', 'strength of keys')),
        checkboxInput(inputId='sizeNodes', label='Size vertices by connections', value=T),
        checkboxInput(inputId='curved', label='Curve edges', value=F),
        checkboxInput(inputId='islands', label='Remove unconnected Tables', value=T),
        radioButtons(inputId='subEdges', label='Select Edges:', choices=c('all', 'some')),
        
        conditionalPanel("input.subEdges=='some'",
                         checkboxInput(inputId='delE', label='Delete un-selected edges', value=F),
                         selectInput(inputId='edgev', label='Variables', choices=sort(unique(E(g)$name)), multiple=T)
        )
        
      ),
      
      ## STRENGTH TABLE
      conditionalPanel(
        condition="input.mytab=='strength'",
        checkboxInput(inputId='keyList', label='Optional: Use pre-computed key strength matrix', value=FALSE),
        
        conditionalPanel("input.keyList==true",
                         selectInput(inputId='keyListObject', label='Pick pre-computed key strength matrix object', choices=ls(name='.GlobalEnv')) 
        ),
        
        selectInput(inputId='key', label='Choose a key', choices=sort(unique(E(g)$name))),
        sliderInput("keylab", "Variable labels size", value=1, min=0.1, max=10, step=0.1)
     
      ),
      
      
      ## KEY-TABLE HEATMAP
      conditionalPanel(
        condition="input.mytab=='keyTab'",
        sliderInput("colLabCex", "Table Label Size", value=1, min=0.1, max=10, step=0.1),
        sliderInput("rowLabCex", "Key Label Size", value=1, min=.1, max=10, step=.1),
        sliderInput("marginSize", "Margin Size", value=15, min=0, max=100, step=1),
        sliderInput("wsize", "width size", value=800, min=0, max=5000, step=50)
      ),
      
      ## ADJACENCY LIST
      conditionalPanel(
        condition="input.mytab=='adjlist'",
        selectInput(inputId='adjcommonvar', label='Choose a key', choices=sort(unique(E(g)$name)), multiple=T)
      )

      
    ),
    
    ## PANELS TO SHOW
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Network', value='network', plotOutput('circle', height='150%')),
                  tabPanel('Key Strength', value='strength', plotOutput('strengthPlot', height='150%')),
                  tabPanel('Key-Table Matrix', value='keyTab', plotOutput('keyTabMat', height='150%')),
                  tabPanel('Adjacency List', value='adjlist', dataTableOutput('adjlisttab'))
      )
    )
    
  )),
  
  server = function(input, output) {

    ################################################
    ## NETWORK
    ################################################
    
    ## DEFINING COMMON VARIABLES
    commonv <- unlist(lapply(dfL, names),use.names=F)
    commonv <- unique(commonv[duplicated(commonv)==T])
    
    output$circle <- renderPlot({
      
      ## delete edges
      if(input$delE==T){
        eid <- which(E(g)$name %in% input$edgev)
        g <- subgraph.edges(g, eids=eid, delete.vertices=T)
      }
      
      ## remove island vertices
      if(input$islands==T){
        islandTabs <- which(degree(g)==0)
        g <- delete.vertices(g, v=islandTabs)
      }
      
      ## setting defaul graphic parameters
      E(g)$width <- input$ewidth
      E(g)$color <- 'gray'
      
      ## edge colors
      if(input$subEdges=='some'){
        E(g)$color[E(g)$name %in% input$edgev] <- 'red'
        E(g)$width[E(g)$name %in% input$edgev] <- input$ewidth + 3
      }
      
      ## vertex size
      if(input$sizeNodes==T) {
        deg <- igraph::degree(g, V(g)$name, mode='total')
        V(g)$size <- (deg/mean(deg)*input$sizeNodesSlider) + 5
      } else {
        V(g)$size <- 5 + input$sizeNodesSlider}
      
      ## vertex colors
      if(input$vcolor=='# of keys') {
        namesL <- lapply(dfL, names)
        keysInTab <- unlist(lapply(namesL, function(x) sum((x %in% commonv)==T)))
        keysInTab[is.na(keysInTab)] <- 0
        dfn <- data.frame(keysInTab)
        dfn$vnames <- row.names(dfn)
        V(g)$nkeys <- as.numeric(makeVertexAtt(g, df=dfn, vname='keysInTab', by.df='vnames', by.g='name'))
        vcolPal <- colorRampPalette(c("white", "purple"))(n = max(V(g)$nkeys)+1)
        V(g)$color <- vcolPal[V(g)$nkeys+1]
        
      } else if(input$vcolor=='# of connections') {
        vcolPal <- colorRampPalette(c("white", "purple"))(n = max(degree(g))+1)
        V(g)$color <- vcolPal[degree(g)+1]  
      }
      
      ## layout
      lay <- get(paste('layout.', as.character(input$lay2), sep=''))  
      
      ## ACTUALLY PLOTTING NETWORK
      plot(g, vertex.label.cex=input$vlabcex, layout=lay, edge.curved=input$curved, vertex.label.color='black') 
    }, width=800, height=800)
    
    
    ################################################
    ## STRENGTH CHART 
    ################################################
    
    output$strengthPlot <- renderPlot({
      
      ## defining keyL object
      if(input$keyList==TRUE) {keyL <- get(input$keyListObject)[[input$key]]
      } else {
        keyL <- isKeym(dfL, input$key)
      }

      myPalette <- colorRampPalette(c("white", "firebrick"))(n=20)
      checklab <- round(keyL,2)
      heatmap.2(keyL, trace='none', dendrogram='none', Rowv=F, Colv=F, margins=c(18,18), col=myPalette, 
                cellnote=checklab, notecol='black', na.rm=T, cexRow=input$keylab, cexCol=input$keylab)
      text(x=.5, y=.9, '1. Take unique values of the \n key variable in each table. \n
           2. Look at the share of these unique \n values from table 1 (right) \n that appear in table 2 (bottom) \n
           3. So variables with high row scores are strong keys \n'
           , cex=1.2)
      
    }, width=800, height=800)
    
    
    ################################################
    ## STRENGTH TABLE
    ################################################
    
    plotSize <- reactive({
      return(input$wsize)
    })
    
    output$keyTabMat <- renderPlot({
      ## setting up matrix
      tabv <- names(dfL)
      mat <- matrix(nrow=length(commonv), ncol=length(tabv))
      colnames(mat) <- tabv
      rownames(mat) <- commonv
      
      for(i in 1:nrow(mat)){
        colindx <- which(sapply(lapply(dfL, names), function(x) rownames(mat)[i] %in% x))
        mat[i, colindx] <- 1
      }
      mat[is.na(mat)] <- 0
      mat <- mat[rev(order(rowSums(mat))),]
      
      myPalette <- colorRampPalette(c("white", "firebrick"))(n = 2)
      checklab <- mat
      heatmap.2(mat, trace='none', dendrogram='none', Rowv=F, Colv=F, key=F, 
                margins=c(input$marginSize, input$marginSize), lhei = c(0.1,0.9), 
                main='Linking variables (rows) in Tables (columns)',
                col=myPalette, cellnote=checklab, notecol='black', cexRow=input$rowLabCex, cexCol=input$colLabCex)
    }, width=plotSize, height=plotSize, units='px')
    
    ################################################
    ## ADJACENCY LIST
    ################################################    
    output$adjlisttab <- renderDataTable({
      df <- get.data.frame(g)[,c('from', 'to', 'name')]
      names(df) <- c('table1', 'table2', 'commonVariable')
      if(length(input$adjcommonvar)>0) df <- df[df$commonVariable %in% input$adjcommonvar,]
      df
    })
    
  }
  )
  
}



#' @title Turn a directory of flat files into a list of data.frames
#' @description Useful to prepare data for \code{\link{tableNet}}
#' @param dfdir character string of the directory where you want to load flat files
#' @param ext file extention on the type of files to load.  Usually \code{.csv} or \code{.txt}
#' @param exclude character string of table names to be excluded from app.  Needs to be specified to \code{NULL} or a character
#' vector or else \code{...} arguments will not be handled properly.
#' @param ... parameters to pass to \code{\link{read.delim}}.  Commonly \code{nrow}, \code{sep},
#' @seealso \code{\link{tableNet}} \code{\link{isKey}}
#' @return list of data.frames
#' @export
#' 
#' @examples
#' \dontrun{
#' ## download some baseball data. NOTE This will download 30MB of data (25 csv files) into a temporary directory
#' temp <- tempfile()
#' localDataDir <- paste0(tempdir(), '\\lahman2012-csv-onYourComp.zip')
#' download.file('http://seanlahman.com/files/database/lahman2012-csv.zip', localDataDir)
#' unzip(localDataDir, exdir=paste0(tempdir(), '\\lahman2012-csv-onYourComp')) ## may not be necessary
#' 
#' ## create a list of data.frames from .CSVs
#' dfL <- dir2dfList(paste0(tempdir(), '\\lahman2012-csv-onYourComp'), ext='.csv', exclude=NULL, sep=',', stringsAsFactors=F)
#' }

dir2dfList <- function(dfdir, ext='.txt', exclude=NULL, ...) {
  # get list of .txt text files in directory
  setwd(dfdir)
  tables <- list.files()[sapply(list.files(), function(x) substr(x,nchar(x)-3, nchar(x)))==ext]
  tableNames <- sapply(tables, function(x) substr(x,0, nchar(x)-4), USE.NAMES=F)
  
  # create list of dfs from directory
  dfL <- list()
  for(i in 1:length(tables)) {
    dfL[[tableNames[i]]] <- read.delim(tables[i], ...)
    dfL[[tableNames[i]]] <- dfL[[tableNames[i]]][,!names(dfL[[tableNames[i]]]) %in% exclude]
    print(paste(tableNames[i], nrow(dfL[[tableNames[i]]]), Sys.time()))
  }
  
  return(dfL)
}

#' @title Determine strength of linking variables  
#' @description This function computes the percentage of unique values of a column \code{x} from \code{table1} that appear in
#' in a \code{table2}.  It is called and computed on the fly in \code{\link{tableNet}}.  However, these computations can be 
#' slow on large datasets, so it is provided a standalone function that can be run once to store the output and fed into the 
#' \code{\link{tableNet}} app to prevent repetitive slow computations on the fly.
#' @param dfL list of data.frames.  easily generated from \code{\link{dir2dfList}}
#' @param xvar character string, name of the variable to calculate strength for across all tables in \code{dfL}
#' @param printdf prints progress of flat file loads to R console.
#' @seealso \code{\link{tableNet}} \code{\link{dir2dfList}}
#' @return list of data.frames 
#' @export
#' 
#' @examples
#' \dontrun{
#' ## download some baseball data. NOTE This will download 30MB of data (25 csv files) into a temporary directory
#' temp <- tempfile()
#' localDataDir <- paste0(tempdir(), '\\lahman2012-csv-onYourComp.zip')
#' download.file('http://seanlahman.com/files/database/lahman2012-csv.zip', localDataDir)
#' unzip(localDataDir, exdir=paste0(tempdir(), '\\lahman2012-csv-onYourComp')) ## may not be necessary
#' 
#' ## create a list of data.frames from .CSVs
#' dfL <- dir2dfList(paste0(tempdir(), '\\lahman2012-csv-onYourComp'), ext='.csv', exclude=NULL, sep=',', stringsAsFactors=F)
#' isKey(dfL, 'playerID')
#' }


isKey <- function(dfL, xvar) {
  
  tabNames <- lapply(dfL, names)
  tabs <- names(which(lapply(tabNames, function(x) xvar %in% x)==T))
  mat <- matrix(nrow=length(tabs), ncol=length(tabs))
  ii <- 1; 
  for(i in tabs){
    iivar <- dfL[[i]][,xvar]
    jj <- 1
    for(j in tabs){
      jjvar <- dfL[[j]][,xvar]
      stop
      mat[jj,ii] <- sum(jjvar %in% iivar)/length(jjvar)
      jj<-jj+1
    }
    ii<-ii+1
  }
  
  mat[is.na(mat)] <- 0
  colnames(mat) <- tabs
  rownames(mat) <- tabs
  return(mat)
}

