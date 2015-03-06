#' @title Assocation Rules Visualization Shiny App
#' @description Launches a Shiny App that provides an interactive interface to the visualizations of the \code{arulesViz} package.
#' The app allows users to mine rules based on all or just subsets of features, sort by criteria (lift, support, confidence) and visualize
#' using network graph, grouped bubble and scatter plots. \cr
#' Users filter rules to target only those with a certain variable on the RHS or LHS of the rule.
#' Rule mining is computed using the \link{apriori} algorithm from \code{arules}.
#' 
#' @param dataset data.frame, this is the dataset that association rules will be mined from.  Each row is treated as a transaction.
#' @param bin logical, \code{TRUE} will automatically discretize/bin numerical data into categorical features that can be used for association analysis.
#' @seealso \code{arulesViz}, \code{arules}
#' @return Shiny App
#' @import shiny arulesViz arules
#' @export
#' 
#' @examples
#' ## creating some data 
#' n <- 10000 # of obs
#' d <- data.frame(
#' eye = sample(c('brown', 'green', 'blue', 'hazel'), n, replace=T),
#' gender = sample(c('male', 'female'), n, replace=T),
#' height = sort(sample(c('dwarf', 'short', 'average', 'above average', 'giant'), n, replace=T)),
#' wealth = sort(sample(c('poor', 'struggling', 'middle', 'uppermiddle', 'comfortable', 'rich', '1%', 'millionaire', 'billionaire'), n, replace=T)),
#' favoriteAnimal = sample(c('dog', 'cat', 'bat', 'frog', 'lion', 'cheetah', 'lion', 'walrus', 'squirrel'), n, replace=T),
#' numkids = abs(round(rnorm(n, 2, 1)))
#' )
#' 
#' ## adding some pattern
#' d$numkids[d$gender=='male'] <- d$numkids[d$gender=='male'] + sample(0:3, sum(d$gender=='male'), replace=T)
#' d$numkids <- factor(d$numkids)
#' 
#' ## calling Shiny App to visualize association rules
#' arulesApp(d)

arulesApp <- function (dataset, bin=T) {
  
  ## binning numeric data
  for(i in 1:ncol(dataset)) {
    if(class(dataset[,i]) %in% c('numeric', 'integer')) dataset[,i] <- depthbin(dataset[,i], nbins=10)
  }
  
  ## calling Shiny App
  shinyApp(ui = shinyUI(pageWithSidebar(
  
    headerPanel("Association Rules"),
    
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.samp=='Sample'",
        numericInput("nrule", 'Number of Rules', 5), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab=='graph'",
        radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
      ),
      
      conditionalPanel(
        condition = "input.lhsv=='Subset'", 
        uiOutput("choose_lhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.rhsv=='Subset'", 
        uiOutput("choose_rhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'itemFreq')", 
        radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
        uiOutput("choose_columns"), br(),
        sliderInput("supp", "Support:", min = 0, max = 1, value = 0.1 , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.5 , step = 1/10000), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(), 
        numericInput("maxL", "Max. items per set::", 10), br(),
        radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
        radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
        downloadButton('downloadData', 'Download Rules as CSV')
      )
      
    ),
    
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                  tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                  tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                  tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                  tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                  tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
      )
    )
    
   )),
   
   server = function(input, output) {
     
     output$choose_columns <- renderUI({
       checkboxGroupInput("cols", "Choose variables:", 
                          choices  = colnames(dataset),
                          selected = colnames(dataset)[1:5])
     })
     
     
     output$choose_lhs <- renderUI({
       checkboxGroupInput("colsLHS", "Choose LHS variables:", 
                          choices  = input$cols,
                          selected = input$cols[1])
     })
     
     output$choose_rhs <- renderUI({
       checkboxGroupInput("colsRHS", "Choose RHS variables:", 
                          choices  = input$cols,
                          selected = input$cols[1])
     })
     
     ## Extracting and Defining arules
     rules <- reactive({
       arAll <- apriori(dataset[,input$cols], parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
       
       if(input$rhsv=='Subset' & input$lhsv!='Subset'){
         varsR <- character()
         for(i in 1:length(input$colsRHS)){
           tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
           varsR <- c(varsR, tmp)
         }
         ar <- subset(arAll, subset=rhs %in% varsR)
         
       } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
         varsL <- character()
         for(i in 1:length(input$colsLHS)){
           tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
           varsL <- c(varsL, tmp)
         }
         ar <- subset(arAll, subset=lhs %in% varsL)
         
       } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
         varsL <- character()
         for(i in 1:length(input$colsLHS)){
           tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
           varsL <- c(varsL, tmp)
         }
         varsR <- character()
         for(i in 1:length(input$colsRHS)){
           tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
           varsR <- c(varsR, tmp)
         }
         ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
         
       } else {
         ar <- arAll
       }
       ar
     })
     
     # Rule length
     nR <- reactive({
       nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
     })
     
     ## Grouped Plot #########################
     output$groupedPlot <- renderPlot({
       ar <- rules()
       plot(sort(ar, by=input$sort)[1:nR()], method='grouped')
     }, height=800, width=800)
     
     ## Graph Plot ##########################
     output$graphPlot <- renderPlot({
       ar <- rules()
       plot(sort(ar, by=input$sort)[1:nR()], method='graph', control=list(type=input$graphType))
     }, height=800, width=800)
     
     ## Scatter Plot ##########################
     output$scatterPlot <- renderPlot({
       ar <- rules()
       plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
     }, height=800, width=800)
     
     ## Item Frequency Plot ##########################
     output$itemFreqPlot <- renderPlot({
       trans <- as(dataset[,input$cols], 'transactions')
       itemFrequencyPlot(trans)
     }, height=800, width=800)
     
     ## Rules Data Table ##########################
     output$rulesDataTable <- renderDataTable({
       ar <- rules()
       rulesdt <- rules2df(ar)
       rulesdt
     })
     
     ## Rules Printed ########################
     output$rulesTable <- renderPrint({
       #hack to disply results... make sure this match line above!!
       #ar <- apriori(dataset[,input$cols], parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
       ar <- rules()
       inspect(sort(ar, by=input$sort))
     })
     
     ## Download data to csv ########################
     output$downloadData <- downloadHandler(
       filename = 'arules_data.csv',
       content = function(file) {
         write.csv(rules2df(rules()), file)
       }
     )
     
     
   }
  )
}



   
   
   
   
   