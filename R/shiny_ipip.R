#' @title IPIP-NEO visualization in Shiny
#' @description The IPIP-NEO (International Personality Item Pool) is a comprehensive personality test of 5 human dimensions: Extraversion,
#' Agreeableness, Conscientiousness, Neuroticism, Openness to New Experience. \cr
#' This is a wrapper around a relatively large application built in \code{\link{shiny}}.
#' The \code{ui.R} and \code{server.R } components are built into the \code{shinyApp} function which allows the user to call
#' the shiny app as a function with whatever input dataset is desired.
#' @param ipip35 data.frame of ipip-neo results.  Should be 35 rows.  
#' First column should be the personality traits from the IPIP-NEO Personality test. Each additional column
#' should be the corresponding results from the test for a person. Frist row is treated is as a header (Person's names). \cr
#' @source \link{http://www.personal.psu.edu/j5j/IPIP/}
#' @return Shiny App
#' @imports shiny, ggplot2, vcd, fmsb, MASS, arulesViz, RcolorBrewer
#' @export
#' @examples
#'## creating some fake ipip data
#'ipipdf <- data.frame(
#'  X=c('Extraversion','Friendliness','Gregariousness','Assertiveness','Activity Level','Excitement Seeking','Cheerfulness',
#'      'Agreeableness','Trust','Morality','Altruism','Cooperation','Modesty','Sympathy',
#'      'Conscientiousness','Self Efficacy','Orderliness','Dutifulness','Achievement-Striving','Self Discipline','Cautiousness',
#'      'Neuroticism','Anxiety','Anger','Depression','Self Consciousness','Immoderation','Vulnerability',
#'      'Openness to Experience','Imagination','Artistic Interests','Emotionality','Adventurousness','Intellect','Liberalism'),
#'  Baelish=runif(35, 0, 100),
#'  Tyrion=runif(35, 0, 100),
#'  Cersei=runif(35, 0, 100),
#'  Ned=runif(35, 0, 100),
#'  Robert=runif(35, 0, 100),
#'  John=runif(35, 0, 100),
#'  Varys=runif(35, 0, 100),
#'  Arya=runif(35, 0, 100),
#'  stringsAsFactors=F
#' )
#' 
#' ## actually run the Shiny App
#' ipipApp(ipipdf)


ipipApp <- function (ipip35) {
  
  #library('shiny')
  #library('ggplot2')
  #library('vcd')
  #library('fmsb')
  #library('MASS')
  #library('arulesViz')
  #library('RColorBrewer')
    
  row.names(ipip35) <- as.character(ipip35[,1])
  ipip35[,1] <- NULL
  
  ipip5 <- ipip35[c(1,8,15,22,29),]
  ipip30 <- ipip35[-c(1,8,15,22,29),]
  
  ipip5 <- ipip5[,order(names(ipip5))]
  ipip30 <- ipip30[,order(names(ipip30))]
  
  ## functions ######################################
  mSS <- function(ipip, p='SS'){
    c = dim(ipip)[2]
    sumSA = matrix(NA, nrow = c, ncol = c)
    
    for (i in 1:(c-1)) {
      for (j in (i+1):c) {
        if(p=='SS'){
          sumSA[i, j] = sum((ipip[, i] - ipip[, j])^2)/nrow(ipip)
          sumSA[j, i] = sum((ipip[, i] - ipip[, j])^2)/nrow(ipip)
        } else if(p=='abs'){
          sumSA[i, j] = sum(abs(ipip[, i] - ipip[, j]))/nrow(ipip)
          sumSA[j, i] = sum(abs(ipip[, i] - ipip[, j]))/nrow(ipip)
        }
      }
    }
    
    output = data.frame(sumSA)
    colnames(output) <- names(ipip)
    rownames(output) <- names(ipip)
    
    return(output)
  }
  
  #matching pairs
  plist <- function(x, adecr=T) {
    xout <- unlist(x)
    names(xout) <- paste(rep(names(x), each=ncol(x)), rep(names(x), ncol(x)), sep=' - ')
    xout<-sort(xout[!is.na(xout)], decreasing=adecr)
    return(xout)
  }
  
  shinyApp(ui = shinyUI(navbarPage("IPIP Data Visualization", id='mytab',
                                    
                                    tabPanel("Radar",
                                             fluidRow(
                                               column(2,    
                                                      #condition = "input.mytab =='radar' | input.mytab =='pc' | input.mytab == 'avg' ",  
                                                      #uiOutput("choose_columns"), 
                                                      checkboxGroupInput("columns", "Choose people:", 
                                                                         choices  = colnames(ipip30),
                                                                         selected = colnames(ipip30)[1:2]),
                                                      hr(),
                                                      radioButtons(inputId='cat5', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr() 
                                                      
                                               ),
                                               column(10,
                                                      plotOutput("radarPlot", width='100%', height='100%'))
                                             )
                                             
                                    ),
                                    tabPanel("Parallel Coord",
                                             fluidRow(
                                               column(2,
                                                      # uiOutput("choose_columns"), 
                                                      checkboxGroupInput("columns6", "Choose people:", 
                                                                         choices  = colnames(ipip30),
                                                                         selected = colnames(ipip30)[1:2]),
                                                      hr(),
                                                      radioButtons(inputId='cat6', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr() 
                                                      
                                                      
                                               ),
                                               column(10,
                                                      plotOutput("pcPlot", width='100%', height='100%'))
                                             )
                                             
                                    ),
                                    tabPanel("Stars",
                                             fluidRow(
                                               column(2,
                                                      radioButtons(inputId='draw', label='Fill Semi-Circles',
                                                                   choices = list('Fill'=T, 'No Fill'=F)    
                                                                   
                                                      ),
                                                      hr(),
                                                      radioButtons(inputId='cat14', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr() 
                                               ),
                                               column(10,
                                                      plotOutput("starPlot", width='100%', height='100%'))
                                             )
                                    ),
                                    
                                    tabPanel("Ternary",
                                             fluidRow(
                                               column(2,
                                                      selectInput("tp1",label="Ternary Plot: Person 1",choices=names(ipip5), multiple=FALSE),
                                                      selectInput("tp2",label="Ternary Plot: Person 2",choices=names(ipip5), selected=names(ipip5)[2], multiple=FALSE),
                                                      selectInput("tp3",label="Ternary Plot: Person 3",choices=names(ipip5), selected=names(ipip5)[3], multiple=FALSE),
                                                      hr(),
                                                      radioButtons(inputId='cat8', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr() 
                                                      # actionButton("go", "GO")
                                               ),
                                               
                                               column(10,
                                                      plotOutput("ternPlot", width="100%", height='100%'))
                                             )),
                                    
                                    tabPanel("Distances",
                                             fluidRow(
                                               column(2,
                                                      selectInput("cp1",label="Choose a Person:", choices=names(ipip5)), 
                                                      hr(),
                                                      radioButtons(inputId='dmet', label='Choose a distance metric:',
                                                                   choices = list('Absolute Distance'='metabs', 'Sum of Squares'='metss', 'Correlation'='metcor')),
                                                      hr(),
                                                      radioButtons(inputId='cat9', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()                    # actionButton("go", "GO")
                                               ),
                                               column(10,
                                                      plotOutput("corPlot", width="100%", height="100%"))
                                             )),
                                    
                                    tabPanel("Pairs",
                                             fluidRow(
                                               column(2,
                                                      radioButtons(inputId='pdist', label='Choose a distance metric:',
                                                                   choices = list('Absolute Distance'='abs', 'Sum of Squares'='SS')), br(),
                                                      radioButtons(inputId='sortdist', label='How to sort pairs:',
                                                                   choices = list('Most similar'='msim', 'Most different'='mdiff')), br(),
                                                      numericInput("npair", "Number of pairs:", 15), 
                                                      hr(),
                                                      radioButtons(inputId='cat10', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()  
                                               ),
                                               column(10,
                                                      plotOutput("pairPlot", width="100%", height="100%"))
                                             )),
                                    
                                    tabPanel("Uniqueness",
                                             fluidRow(
                                               column(2,
                                                      radioButtons(inputId='sdist', label='Choose a distance metric:',
                                                                   choices = list('Absolute Distance'='abs', 'Sum of Squares'='SS')), 
                                                      hr(),
                                                      radioButtons(inputId='cat11', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()  
                                               ),
                                               column(10,
                                                      plotOutput("simPlot", width="100%", height="100%"))
                                             )),
                                    
                                    tabPanel("Extremeness",
                                             fluidRow(
                                               column(2,
                                                      radioButtons(inputId='edist', label='Choose a distance metric:',
                                                                   choices = list('Absolute Distance'='abs', 'Sum of Squares'='SS')), 
                                                      hr(),
                                                      radioButtons(inputId='cat12', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()  
                                               ),
                                               column(10,
                                                      plotOutput("extrPlot", width="100%", height="100%"))
                                             )),
                                    
                                    tabPanel("Averages",
                                             fluidRow(
                                               column(2,
                                                      # uiOutput("choose_columns"), 
                                                      checkboxGroupInput("columns7", "Choose people:", 
                                                                         choices  = colnames(ipip30),
                                                                         selected = colnames(ipip30)[1:2]),
                                                      hr(),
                                                      radioButtons(inputId='cat7', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()
                                                      
                                               ),
                                               column(10,
                                                      plotOutput("avgPlot", width="100%", height="100%"))
                                             )),
                                    
                                    tabPanel("Scatter",
                                             fluidRow(
                                               column(2,
                                                      selectInput("p1", label="Scatter Plot: Person 1", choices=names(ipip5)),
                                                      selectInput("p2", label="Scatter Plot: Person 2", choices=names(ipip5), selected=names(ipip5)[2]),
                                                      hr(),
                                                      radioButtons(inputId='cat0', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()       
                                               ),
                                               column(10,
                                                      plotOutput('scatterPlot', width='100%', height='100%'))
                                             )),
                                    
                                    tabPanel("Clusters",
                                             fluidRow(
                                               column(2,
                                                      radioButtons(inputId='clustmeth', label='Choose a clustering distance metric:',
                                                                   choices = list('Complete Link'='complete', 'Average Link'='average', 'Single Link'='single')), 
                                                      hr(),
                                                      radioButtons(inputId='cat13', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr()  
                                                      
                                               ),
                                               column(10,
                                                      plotOutput('clustPlot', width='100%', height='100%'))
                                             )),
                                    
                                    tabPanel("Rules",
                                             fluidRow(
                                               column(2,
                                                      numericInput("nrule", "Number of Rules:", 8), 
                                                      hr(),
                                                      selectInput('rulespar', label='Association Rule Criteria', choices = c('lift', 'confidence', 'support')),
                                                      hr(),
                                                      radioButtons(inputId='cat15', label="Choose IPIP category for details:", choices=c('Big 5', 'Sub 30', rownames(ipip5))), 
                                                      hr() 
                                               ),
                                               column(10,
                                                      plotOutput("rulesPlot", width="100%", height="100%"))
                                             ))
                                    
  )), server = function(input, output) {
    
    radiop0 <- reactive({
      if(input$cat0 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat0 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat0 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat0 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat0 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat0 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat0 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop <- reactive({
      if(input$cat5 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat5 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat5 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat5 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat5 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat5 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat5 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop6 <- reactive({
      if(input$cat6 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat6 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat6 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat6 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat6 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat6 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat6 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop7 <- reactive({
      if(input$cat7 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat7 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat7 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat7 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat7 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat7 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat7 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop8 <- reactive({
      if(input$cat8 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat8 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat8 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat8 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat8 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat8 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat8 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop9 <- reactive({
      if(input$cat9 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat9 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat9 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat9 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat9 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat9 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat9 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop10 <- reactive({
      if(input$cat10 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat10 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat10 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat10 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat10 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat10 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat10 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop11 <- reactive({
      if(input$cat11 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat11 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat11 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat11 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat11 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat11 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat11 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop12 <- reactive({
      if(input$cat12 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat12 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat12 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat12 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat12 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat12 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat12 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop13 <- reactive({
      if(input$cat13 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat13 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat13 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat13 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat13 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat13 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat13 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop14 <- reactive({
      if(input$cat14 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat14 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat14 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat14 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat14 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat14 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat14 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiop15 <- reactive({
      if(input$cat15 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat15 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat15 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat15 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat15 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat15 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat15 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol0 <- reactive({
      if(input$cat0 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat0 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat0 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat0 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat0 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat0 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat0 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol <- reactive({
      if(input$cat5 == 'Big 5'){ Value = seq(5)
      } else if(input$cat5 == 'Sub 30') { Value = rep(seq(5), each=6)                
      } else if(input$cat5 == rownames(ipip5)[1]) { Value = rep(1,6)
      } else if(input$cat5 == rownames(ipip5)[2]) { Value = rep(2,6)
      } else if(input$cat5 == rownames(ipip5)[3]) { Value = rep(3,6) 
      } else if(input$cat5 == rownames(ipip5)[4]) { Value = rep(4,6)  
      } else if(input$cat5 == rownames(ipip5)[5]) { Value = rep(5,6)  
      }
    })
    radiopcol6 <- reactive({
      if(input$cat6 == 'Big 5'){ Value = seq(5)
      } else if(input$cat6 == 'Sub 30') { Value = rep(seq(5), each=6)                
      } else if(input$cat6 == rownames(ipip5)[1]) { Value = rep(1,6)
      } else if(input$cat6 == rownames(ipip5)[2]) { Value = rep(2,6)
      } else if(input$cat6 == rownames(ipip5)[3]) { Value = rep(3,6) 
      } else if(input$cat6 == rownames(ipip5)[4]) { Value = rep(4,6)  
      } else if(input$cat6 == rownames(ipip5)[5]) { Value = rep(5,6)  
      }
    })
    radiopcol7 <- reactive({
      if(input$cat7 == 'Big 5'){ Value = seq(5)
      } else if(input$cat7 == 'Sub 30') { Value = rep(seq(5), each=6)                
      } else if(input$cat7 == rownames(ipip5)[1]) { Value = rep(1,6)
      } else if(input$cat7 == rownames(ipip5)[2]) { Value = rep(2,6)
      } else if(input$cat7 == rownames(ipip5)[3]) { Value = rep(3,6) 
      } else if(input$cat7 == rownames(ipip5)[4]) { Value = rep(4,6)  
      } else if(input$cat7 == rownames(ipip5)[5]) { Value = rep(5,6)  
      }
    })
    radiopcol8 <- reactive({
      if(input$cat8 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat8 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat8 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat8 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat8 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat8 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat8 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol9 <- reactive({
      if(input$cat9 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat9 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat9 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat9 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat9 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat9 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat9 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol10 <- reactive({
      if(input$cat10 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat10 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat10 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat10 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat10 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat10 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat10 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol11 <- reactive({
      if(input$cat11 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat11 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat11 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat11 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat11 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat11 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat11 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    radiopcol12 <- reactive({
      if(input$cat12 == 'Big 5'){ Value = c(1,8,15,22,29)
      } else if(input$cat12 == 'Sub 30') { Value = setdiff(1:35, c(1,8,15,22,29))                         
      } else if(input$cat12 == rownames(ipip5)[1]) { Value = c(2,3,4,5,6,7)
      } else if(input$cat12 == rownames(ipip5)[2]) { Value = c(9,10,11,12,13,14)
      } else if(input$cat12 == rownames(ipip5)[3]) { Value = c(16,17,18,19,20,21)
      } else if(input$cat12 == rownames(ipip5)[4]) { Value =  c(23,24,25,26,27,28)
      } else if(input$cat12 == rownames(ipip5)[5]) { Value =  c(30,31,32,33,34,35)
      }
    })
    
    ## Plot: Scatter
    output$scatterPlot <- renderPlot({
      
      ipipd <- data.frame(cbind(ipip35[,input$p2], ipip35[,input$p1]))[radiop0(),]
      rownames(ipipd) <- rownames(ipip35) [radiop0()]
      names(ipipd) <- c('p1', 'p2')
      
      a <- ggplot(data=ipipd, aes(p1, p2), label=rownames(ipipd)) +
        geom_point(size=4) +
        geom_text(label = rownames(ipipd), size=4, col=radiopcol0(), hjust = -.1) + 
        labs(title=paste('Scatterplot: ', input$cat0, sep='')) + 
        xlab(input$p2) +
        ylab(input$p1) +
        scale_x_continuous(limits = c(0, 100)) + 
        scale_y_continuous(limits = c(0, 100)) +
        geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100), lty=1, alpha=1/5) + 
        geom_smooth(method = "lm", fill='none', se=F, lty=2, fullrange=T)
      print(a)
    }, height=600, width=800)
    
    ## Plot: Radar
    output$radarPlot <- renderPlot({
      tipip5 <- rbind(rep(100,length(radiop())), rep(0,length(radiop())), data.frame(t(ipip35[radiop(),input$columns, drop=F])))
      fmsb::radarchart(tipip5, axistype=2, seg=4, maxmin=F, vlabels=row.names(ipip35)[radiop()], caxislabels=c(25,50,75,100), 
                 cglcol=rep("gray"), plwd=2, plty=1, pcol=c('gray', 'gray', 1:nrow(tipip5)), axislabcol=radiopcol(), 
                 title=paste('Radar Plot: ', input$cat5, sep=''))
      
      legend(x='bottomright', legend=row.names(tipip5[-c(1,2),]), col=1:nrow(tipip5),pch=15) 
    }, height=800, width=800)
    
    ## Plot: Parallel Coordinates
    output$pcPlot <- renderPlot({
      tipip52 <- rbind(rep(100,length(radiop6())), rep(0,length(radiop6())), data.frame(t(ipip35[radiop6(),input$columns6, drop=F])))
      parcoord(tipip52, col=c('gray', 'gray', 1:nrow(tipip52)), pch=15, main=paste('Parallel Coordinates: ', input$cat6, sep=''), var.label=T)
      legend(x='bottomright', legend=row.names(tipip52[-c(1,2),]), col=1:nrow(tipip52),  pch=15) 
    }, height=600, width=800)
    
    ## Plot: Ternary
    output$ternPlot <- renderPlot({
      
      tp_ipip<- ipip35[radiop8(), names(ipip35) %in% c(input$tp1, input$tp2, input$tp3)]
      rownames(tp_ipip) <- rownames(ipip35)[radiop8()]
      ternaryplot(tp_ipip, pch=21, id=rownames(tp_ipip), id_color=radiopcol8(),  
                  col=radiopcol8(), prop_size=T, main=paste('Ternary Plot: ', input$cat8, sep=''))
      # })
    }, height=800, width=800)
    
    ## Plot: Association Rules
    output$rulesPlot <- renderPlot({
      ipip5r <- apply(ipip35[radiop15(),], 2,  function(x) cut(x, breaks= c(0,25,50,75,100), labels=c('low', 'medium', 'high', 'very high')))
      rownames(ipip5r) <- rownames(ipip35)[radiop15()]
      ipip5r <- t(ipip5r)
      rules <- apriori(data.frame(ipip5r), parameter=list(supp=0.2, conf=0.2, minlen=2))
      rules_lift <- head(sort(rules, by=input$rulespar), 100)
      
      
      plot(rules_lift[1:input$nrule], method='graph')
    }, height=600, width=600)
    
    ## Plot: Distance 
    output$corPlot <- renderPlot({
      
      #if(input$go == 0) return(NULL)
      
      if(input$dmet=='metcor') {
        mydist <- data.frame(sort(cor(ipip35[radiop9(),])[,input$cp1]))
        mydist <- t(mydist)
        barcol <- cut(mydist, breaks=seq(-1,1,.2))
        maxv = 1; minv= -1
        barbrew <- brewer.pal(10, "Spectral")
      } else if(input$dmet=='metss'){
        mydist<-na.omit(data.frame(t(mSS(ipip35[radiop9(),])[input$cp1,])))
        mydist<-data.frame(mydist[order(mydist, decreasing=T), , drop=F])
        mydist <- t(mydist)
        maxv <- max(mSS(ipip35[radiop9(),],p='SS'), na.rm=T)
        minv <- 0
        barcol <- cut(mydist, breaks=seq(0,maxv,maxv/10))
        barbrew <- rev(brewer.pal(10, "Spectral"))
      } else if(input$dmet=='metabs'){
        mydist<-na.omit(data.frame(t(mSS(ipip35[radiop9(),],p='abs')[input$cp1,])))
        mydist<-data.frame(mydist[order(mydist, decreasing=T), , drop=F])
        mydist <- t(mydist)
        maxv <- max(mSS(ipip35[radiop9(),],p='abs'), na.rm=T)
        minv <- 0
        barcol <- cut(mydist, breaks=seq(0,maxv,maxv/10))
        barbrew <- rev(brewer.pal(10, "Spectral"))
      } 
      
      barplot(as.numeric(mydist), las=2, col=barbrew[barcol], names.arg=colnames(mydist), ylim=c(minv, maxv),
              ylab='Average distance', main=paste('Likeness on ', input$cat9, ': ', input$cp1, sep=''))
      
      
    }, height=600, width=600)
    
    ## Plot: Pairs
    output$pairPlot <- renderPlot({
      par(mar=c(10, 4.1, 4.1, 2.1))
      sdist <- input$sortdist == 'mdiff'
      ptitle <- ifelse(sdist==F, 'Similar', 'Different')
      pairipip <- plist(mSS(ipip35[radiop10(),], p=input$pdist), adecr=sdist)
      pairipip <-pairipip[seq(1,length(pairipip),2)]
      barcol <- cut(pairipip, breaks=10)
      barbrew <- rev(brewer.pal(10, "Spectral"))
      
      barplot(pairipip[1:input$npair], las=2, col=barbrew[barcol], ylab='Distance',
              main=paste('Most ', ptitle ,' Pairs on: ', input$cat10, sep=''))
    }, height=600, width=600)
    
    ## Uniqueness
    output$simPlot <- renderPlot({
      simipip <- sort(colMeans(mSS(ipip35[radiop11(),], p=input$sdist), na.rm=T))
      barcol <- cut(simipip, breaks=10)
      barbrew <- rev(brewer.pal(10, "Spectral"))
      barplot(simipip, las=2, col=barbrew[barcol], ylab='Average distance', main=paste('Uniqueness:', input$cat11))
    }, height=600, width=600)
    
    ## Clusters
    output$clustPlot <- renderPlot({
      d = dist(t(ipip35[radiop13(),]), method = "euclidean")
      x = hclust(d, method = input$clustmeth)
      plot(x, xlab = "", ylab = "", sub = "", yaxt = "n", main = "Cluster Dendrogram")
      
    }, height=600, width=600)
    
    ## Extreme Plots
    output$extrPlot <- renderPlot({
      
      if(input$edist=='abs') {
        mydata <- sapply(ipip35[radiop12(),], function(x) mean(abs(x-50)))
        ipipmean <- sapply(ipip35[radiop12(),], mean)
        ipipmean <- ipipmean[order(mydata)]
        mydata <- mydata[order(mydata)]
        
        ylabt <- 'Average Distance from 50' 
        myylim <- c(0,50)
        maxv <- 100
        minv <- 0
        barbrew <- c(rev(brewer.pal(9,"Reds")), brewer.pal(9,"Greens"))
        barcol <- cut(ipipmean, breaks=seq(minv,maxv,maxv/18))
        mycol <- barbrew[barcol]
        
        
      } else if(input$edist=='SS') {
        mydata<- sort(sapply(ipip35[radiop12(),], function(x) mean(abs(x-50)^2)))
        ylabt <- 'Average (Distances from 50)^2'
        maxv <- max(mydata)
        minv <- 0
        barcol <- cut(mydata, breaks=seq(0,maxv,maxv/10))
        barbrew <- rev(brewer.pal(10, "Spectral"))
        mycol <- barbrew[barcol]
        myylim <- NULL
      }
      
      barplot(mydata, las=2, col=mycol, main='Average Distances from a Score of 50', ylab=ylabt, ylim=myylim)
      if(input$edist=='abs'){
        legend(x=c(1,15), y=c(40,50), legend=round(seq(0,100, 100/17)), fill=barbrew, cex=1.2, border=T,
               horiz=F, yjust=0, x.intersp=.1, y.intersp=.65, box.col=F, title='Average score across traits', title.adj=0, text.font=1)
      }
      
    }, height=600, width=600)
    
    
    ## Average Plots
    output$avgPlot <- renderPlot({
      
      par(mar=c(10,4.1,4.1,2.1))
      mydata <- data.frame(ipip35[radiop7(),input$columns7])
      
      barplot(apply(mydata, 1, mean), ylab="percentile", main="Averages for Sample", col=radiopcol7(), names.arg=rownames(ipip35)[radiop7()],
              ylim=c(0,100), las=2)
      abline(h=50, col='black')
      
    }, height=600, width=600)
    
    
    ## stars## main=paste('Parallel Coordinates: ', input$cat6, sep='')
    output$starPlot <- renderPlot({
      stars(t(ipip35[radiop14(),]), flip.labels=F, draw.segments=input$draw, key.loc=c(par()$usr[2]-2, par()$usr[1]+1.2))
    }, height=1000, width=800)
    
    
  })
  
}