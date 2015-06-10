#' @title Shiny app to visualize dataframe as simple interactive datatable
#' @description Launches a basic Shiny App that renders the given dataframe into an interactive datatable using \code{renderDataTable}
#' @param df dataframe to be visualized
#' @return Shiny App
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#' shinyTable(mtcars) 
#' }


shinyTable <- function(df) {
  shinyApp(
    shinyUI(fluidPage(
      h2(paste0(deparse(substitute(df)))),
      sidebarLayout(sidebarPanel(
        textInput('filter', 'Filter rows using R logical operators'),
        uiOutput('querystatus'),
        br(),
        br(),
        checkboxGroupInput('show_vars', 'Columns to show', names(df), selected=names(df))
      ),
      mainPanel(
        dataTableOutput('mytable')
        )
      )
    )),
    server = function(input, output) {
      output$mytable = renderDataTable({
        df1 <- df[,input$show_vars, drop=F]
        df2 <- try(eval(parse(text=paste0('subset(df1,', input$filter, ')'))))
        if(class(df2)!='try-error') {dfp <- df2
                                     output$querystatus <- renderText({'query successfull :)'})
                                     } else {dfp <- df1
                                             output$querystatus <- renderText({'query unsuccessful :( <br/> (showing full dataset)'})
                                     }
        dfp},
        options=list(lengthMenu=list(c(25,50,100,500,1000,5000,-1), c('25', '50', '100', '500', '1000', '5000', 'All')),
                     pageLength=500)
      )
    }
  )
}
