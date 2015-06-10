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
    shinyUI(basicPage(
      h2(paste0(deparse(substitute(df)))),
      sidebarPanel(
        checkboxGroupInput('show_vars', 'Columns to show', names(df), selected=names(df))
        ),
      mainPanel(
        dataTableOutput('mytable')
        )
    )),
    server = function(input, output) {
      output$mytable = renderDataTable({
        df
      })
    }
  )
}


shinyTable(mtcars)
