#' @title Leaflet interactive map widget
#' @description Launches a Shiny App an interactive Leaflet map which can be used to visualize coordinate data.  Intended
#' for quick exploratory geographic analyses 
#' 
#' @param data data.frame or data.table of data to visualize on map.  Included must be a column for longitude and latitude coordinates.
#' Four additional columns are allowed for filtering and coloring the data.  Note, the filters are currently only categorical, not currently 
#' supporting numeric ranges.  Recommend only passing columns needed for filtering.  Default is to pick first 4
#' @param vars named character vector.  The names of the vector should always be 'lon' and 'lat'.  The corresponding values
#' of this vector are the column names of the columns in \code{data} with the lat and lon data.
#' @seealso \code{leaflet}, \code{shiny}
#' @return Shiny App
#' @import shiny leaflet data.table RColorBrewer htmlwidgets
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' n <- 5000
#' df <- data.frame(latitude=runif(n, 35, 40),
#'                  longitude=runif(n, -100, -85),
#'                  animals=sample(c('dogs', 'cats', 'turtles'), n, replace=T)
#'                  )
#' df$westness <- cut(df$longitude, breaks=seq(min(df$longitude), max(df$longitude), length.out=10))
#' df$northness <- cut(df$latitude, breaks=seq(min(df$latitude), max(df$latitude), length.out=10))
#' leafletMapDF(df)
#' }

leafletMapDF <- function (data, vars=c('lon'='longitude', 'lat'='latitude')) {
  
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  set.seed(110); n=length(col_vector); col_vector <- sample(col_vector, n)
  uivars <- setdiff(names(data), c(vars['lon'], vars['lat']))
  
  if(length(uivars)>4) warning('currently supports only 4 variables for filtering. picking first 4 columns in data')
  
  shinyApp(
    ui = shinyUI(fluidPage(
      leafletOutput('mymap'),
      p(),
      h3('Filter data'),
      if(length(uivars)>=1) selectInput('ui1', label=sprintf('Filter by %s:', uivars[1]), choices=as.character(unique(data[[uivars[1]]])), multiple=T),
      if(length(uivars)>=2) selectInput('ui2', label=sprintf('Filter by %s:', uivars[2]), choices=as.character(unique(data[[uivars[2]]])), multiple=T),
      if(length(uivars)>=3) selectInput('ui3', label=sprintf('Filter by %s:', uivars[3]), choices=as.character(unique(data[[uivars[3]]])), multiple=T),
      if(length(uivars)>=4) selectInput('ui4', label=sprintf('Filter by %s:', uivars[4]), choices=as.character(unique(data[[uivars[4]]])), multiple=T),
      h3('Map parameters'),
      selectInput('colby', 'Color by', uivars),
      numericInput('radiusid', 'Circle radius (meters)', min=1, max=100000, value=10),
      downloadButton('downloadid', 'Save as HTML')
    )),
    
    server = function(input, output) {

      datar <- reactive({
        # filtering data based on ui.  showing rows that meet ALL (not ANY) of the filters.  blanks/NULLS are assumed to be all points.
        if(is.null(input[['ui1']])) cond1 <- rep(T, nrow(data)) else cond1 <- data[[uivars[1]]] %in% input[['ui1']]
        if(is.null(input[['ui2']])) cond2 <- rep(T, nrow(data)) else cond2 <- data[[uivars[2]]] %in% input[['ui2']]
        if(is.null(input[['ui3']])) cond3 <- rep(T, nrow(data)) else cond3 <- data[[uivars[3]]] %in% input[['ui3']]
        if(is.null(input[['ui4']])) cond4 <- rep(T, nrow(data)) else cond4 <- data[[uivars[4]]] %in% input[['ui4']]
        cond <- data.table(cond1, cond2, cond3, cond4)[,which(apply(.SD, 1, all))]
      
        data[cond,]
      })
      
      # Map plot function ############################
      makemap <- function() {
        tmp <- datar()
        xvec <- factor(tmp[[input$colby]])
        legcols <- col_vector[1:length(levels(xvec))]
        leglabs <- levels(xvec)
        
        m <- leaflet() %>%
          addProviderTiles("Stamen.TonerLite", options=providerTileOptions(noWrap = TRUE)
          ) %>%
          addCircles(data=cbind(tmp[[vars['lon']]], tmp[[vars['lat']]]), 
                     color=col_vector[as.numeric(factor(tmp[[input$colby]]))],
                     radius=input[['radiusid']]) %>%
          addLegend("topright", colors=legcols, labels=leglabs, opacity=2, title=input$colby)
        return(m)
      }
      
      ## Render map ###################################
      output$mymap <- renderLeaflet({
        mm <- makemap()
        #saveWidget(mm, 'leafletmap.html') # this will save on each render without browser directory choice
        mm
      })
      
      ## Download map as HTML  ########################
      output$downloadid <- downloadHandler(
        filename = function() 'leafletdownload.html',
        content = function(con) saveWidget(makemap(), con)
      )
    
    }
    
  )}



