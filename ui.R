library(leaflet)
library(shinydashboard)
#options( warn = -1 )

dashboardPage(

 
  dashboardHeader(
    title = "Lake Michigan - 2010"
  ),   
  
  dashboardSidebar(disable=TRUE),
  
  dashboardBody(
 
       fluidRow(
       column(width=8,
        box(width=NULL, solidHeader=TRUE,status="primary",
            leafletOutput("map")
          )),
        
        # Shiny versions prior to 0.11 should use class = "modal" instead.
        column(width=4,

          box(width=NULL, solidHeader=TRUE,status="primary",
                  h3("Model Data"),
                  h4("Current model gridpoint is at:"),
                  htmlOutput("plotwin")),
          box(width=NULL, solidHeader=TRUE,status="primary",
            h3("Station Data"),
            h4("Selected Station:"),
            textOutput("whichstation"),
            actionLink("load", "Load Station Data"),
            h4("Loaded Station:"),
            textOutput("loadedstation"),
            htmlOutput("htloaded"),
            h4("Plot"),
            checkboxInput("checkbox", "Check to overlay station data.", value = FALSE)
            
        )),
       ),

    fluidRow(
      column(width=4,
             box(width=NULL, solidHeader=TRUE, status="primary",
             radioButtons("radio", h3("Plot Limits"),
                          choices = list("Model min-max" = 1, "Fixed" = 2),
                          selected = 1)),
             box(width=NULL, solidHeader=TRUE, status="primary",
             sliderInput("timeRange", label = "Time range", width = 300,
                         timeFormat="%F", 
                         min = as.POSIXct("2010-01-02 00:00:00",tz = 'GMT'),
                         max = as.POSIXct("2011-01-01 00:00:00",tz = 'GMT'),
                         value = c(as.POSIXct("2010-01-02 00:00:00",tz = 'GMT'),
                                   as.POSIXct("2011-01-01 00:00:00",tz = 'GMT')))
             )
             ),
      column(width=8,
        box(width=NULL, solidHeader=TRUE, status="primary", 
          plotOutput("timeplot")
        )     
      )
    
  )
))

