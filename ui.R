library(leaflet)
library(shinydashboard)
options( warn = -1 )

dashboardPage(

  dashboardHeader(
    title = "Lake Michigan - 2015"
  ),   

  
  #----Sidebar  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "nav"),
      menuItem("Table", tabName = "table")
      #          badgeLabel = "new", badgeColor = "green")
    )
  ),
  #----end Sidebar

  #--Dashboard  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(80vh) !important;}"),
    #--Plural TabItems
    tabItems(
      #--One TabItem
      tabItem(tabName="nav",
        #Row
        fluidRow(
          column(width=6,
                 box(width=NULL, solidHeader=TRUE,status="primary",
                     leafletOutput("map")
                 )),
          column(width=6,
                 fluidRow(
                   column(width=12,
                     box(width=NULL,solidHeader=TRUE,status="primary",
                     plotOutput("timeplot")))),
                 fluidRow(
                    column(width=5,
                     strong("Model Output"),
                     htmlOutput("plotwin"),
                     br(),
                        selectInput("dropstation", "Station", "Alpha"),
                       checkboxInput("checkbox", "Check to overlay station data.", value = FALSE)),
                    column(width=3,
                       radioButtons("radio", "Plot Limits",
                                          choices = list("Model min-max" = 1, "Fixed" = 2),
                                         selected = 1)),
                    column(width=4,
                       sliderInput("timeRange", label = "Time range", 
                          timeFormat="%F", 
                                     min = as.POSIXct("2015-01-02 00:00:00",tz = 'GMT'),
                                     max = as.POSIXct("2016-01-01 00:00:00",tz = 'GMT'),
                                     value = c(as.POSIXct("2015-01-02 00:00:00",tz = 'GMT'),
                                               as.POSIXct("2016-01-01 00:00:00",tz = 'GMT'))))
                 )
                 )
                )#--End Row
      ),
      #--End first tabItem

      #--Plural TabItems
      tabItem(tabName="table",
        h2("Data Table"),
        fluidRow(
             column(3,
               selectInput("insources", "Source", multiple = TRUE, c("All sources"="", structure(allsources)))
             ),
             column(3,
                selectInput("instations", "Stations", multiple = TRUE, c("All stations"="", structure(allstations)))
             ),
            column(3,
              numericInput("minTP", "Min TP", min=0, max=47.4, value=0)
            ),
            column(3,
              numericInput("maxTP", "Max TP", min=0, max=47.4, value=47.4)
            )
        ),
        #--End Row
        hr(),
        DT::dataTableOutput("stationstable")
    )
    
  ) #End tab items   

  )# End dashboard Body
)

