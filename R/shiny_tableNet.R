tableNet <- function(dfL, keyL) {
  
  
  shinyApp(ui = shinyUI(pageWithSidebar(
    
    headerPanel("Database Schema Visualization"),
    
    ## NETWORK 
    sidebarPanel(
      conditionalPanel(
        condition="input.mytab=='network'",
        
        sliderInput("vlabcex", "Vertex label size:", value=1, min=.05, max=5, step=0.05),
        
        sliderInput("sizeNodesSlider", "Vertex size", value=10, min=0.1, max=30, step=0.1),
        
        sliderInput("ewidth", "Edge width:", value=1, min=1, max=10, step=1),
        
        selectInput("lay2", "Choose a layout", 
                    choices = c('circle', 'fruchterman.reingold')),
        
        selectInput("vcolor", "Color vertices by:", 
                    choices = c('# of keys', '# of connections', 'strength of keys')),
        
        checkboxInput(inputId='sizeNodes', label='Size vertices by connections', value=T),
        
        checkboxInput(inputId='curved', label='Curve edges', value=F),
        
        checkboxInput(inputId='islands', label='Remove unconnected Tables', value=T),
        
        radioButtons(inputId='subEdges', label='Select Edges:', choices=c('all', 'some')),
        
        conditionalPanel("input.subEdges=='some'",
                         checkboxInput(inputId='delE', label='Delete un-selected edges', value=F),
                         checkboxGroupInput(inputId='edgev', label='Variables', choices=sort(unique(E(g)$name)))
        )
        
      ),
      
      ## STRENGTH TABLE
      conditionalPanel(
        condition="input.mytab=='strength'",
        selectInput(inputId='key', label='Choose a key', choices=sort(unique(E(g)$name))),
        sliderInput("keylab", 
                    "Variable labels", value=1, min=0.1, max=10, step=0.1)
      ),
      
      ## KEY-TABLE HEATMAP
      conditionalPanel(
        condition="input.mytab=='keyTab'",
        sliderInput("colLabCex", 
                    "Table Label Size", 
                    value = 1,
                    min = .1, 
                    max = 10,
                    step = .1),
        sliderInput("rowLabCex", 
                    "Key Label Size", 
                    value = 1,
                    min = .1, 
                    max = 10,
                    step = .1),
        sliderInput("marginSize", 
                    "Margin Size", 
                    value = 15,
                    min = 0, 
                    max = 100,
                    step = 1),
        sliderInput("test", 
                    "width size", 
                    value = 800,
                    min = 0, 
                    max = 5000,
                    step = 50)
      ),
      
      ## TABLE VIEW 
      conditionalPanel(
        condition="input.mytab=='adjlist'",
        
        selectInput("tab", "Choose a Table", 
                    choices = c('all', unique(V(g)$name))),
        
        selectInput("edgev", "Choose a Variable", 
                    choices = c('all', unique(E(g)$name)))
      )
    ),
    
    ## PANELS TO SHOW
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Network', value='network', plotOutput('circle', height='150%')),
                  tabPanel('Key Strength', value='strength', plotOutput('strengthPlot', height='150%')),
                  tabPanel('Key-Table Matrix', value='keyTab', plotOutput('keyTabMat', height='150%')),
                  tabPanel('Adjacency List', value='adjlist', tableOutput('adjlist'))
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
      
      ## edge colors
      if(input$subEdges=='some'){
        E(g)$color <- 'gray'
        E(g)$color[E(g)$name %in% input$edgev] <- 'red'
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
        dfn <<- data.frame(keysInTab)
        dfn$vnames <<- row.names(dfn)
        V(g)$nkeys <- as.numeric(makeVA(g, df='dfn', id='vnames', att='keysInTab'))
        vcolPal <- colorRampPalette(c("white", "purple"))(n = max(V(g)$nkeys)+1)
        V(g)$color <- vcolPal[V(g)$nkeys+1]
        
      } else if(input$vcolor=='# of connections') {
        vcolPal <- colorRampPalette(c("white", "purple"))(n = max(degree(g))+1)
        V(g)$color <- vcolPal[degree(g)+1]  
      }
      
      ## layout
      lay <- get(paste('layout.', as.character(input$lay2), sep=''))  
      
      ## ACTUALLY PLOTTING NETWORK
      plot(g, vertex.label.cex=input$vlabcex, layout=lay, edge.curved=input$curved, vertex.label.color='black', 
           edge.width=input$ewidth)
    }, width=800, height=800)
    
    ################################################
    ## STRENGTH CHART 
    ################################################
    
    output$strengthPlot <- renderPlot({
      myPalette <- colorRampPalette(c("white", "firebrick"))(n = 20)
      checklab <- round(keyL[[input$key]],2)
      heatmap.2(keyL[[input$key]], trace='none', dendrogram='none', Rowv=F, Colv=F, margins=c(18,18), col=myPalette, 
                cellnote=checklab, notecol='black', na.rm=T, cexRow=input$keylab, cexCol=input$keylab)
      text(x=.5, y=.9, '1. Take unique values of the \n key variable in each table. \n
           2. Look at the share of these unique \n values from table 1 (right) \n that appear in table 2 (bottom) \n
           3. So variables with high row scores are strong keys'
           , cex=1.2)
      
    }, width=800, height=800)
    
    
    ################################################
    ## STRENGTH TABLE
    ################################################
    
    plotSize <- reactive({
      return(input$test)
    })
    
    output$keyTabMat <- renderPlot({
      ## setting up matrix
      tabv <- names(dfL)
      mat <- matrix(nrow=length(commonv), ncol=length(tabv))
      colnames(mat) <- tabv
      k <- lapply(keyL, rownames)
      rownames(mat) <- names(k)
      
      for(i in 1:length(k)) {
        indx <- match(k[[i]], colnames(mat))
        mat[i,indx] <- 1
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
    adjdf <- reactive({
      df <- get.data.frame(g)[,1:3]
      #names(df) <- c('table1', 'table2', 'commonVariable')
      #df[(df$table1 == input$tab | df$table2 == input$tab) ,]
      return(df)
    })
    
    output$adjlist <- renderTable({
      adjdf()
    })
    
    
    
  }
  )
  
}

