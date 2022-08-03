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
    #--Plural TabItems
    tabItems(
      #--One TabItem
      tabItem(tabName="nav",
        #Row
        fluidRow(
          column(width=8,
          box(width=NULL, solidHeader=TRUE,status="primary",
            leafletOutput("map")
            )
          ), 
          #--
          column(width=4,
            box(width=NULL, solidHeader=TRUE,status="primary",
              h3("Model Data"),
              h4("Current model gridpoint is at:"),
              htmlOutput("plotwin")
              ),
            box(width=NULL, solidHeader=TRUE,status="primary",
              h3("Station Data"),
              #selectizeInput("dropstation", label = NULL, choices=alls, multiple=TRUE),
              # selectizeInput(
              #   'dropstation', label = NULL, choices = alls,
              #   options = list(create = TRUE)
              # ),
              selectInput("dropstation", "Station", allstations),
              h4("Plot"),
              checkboxInput("checkbox", "Check to overlay station data.", value = FALSE)
              )
            ),
           ),   #End of first Fluid Row

        #Row
        fluidRow(
          #--
          column(width=4,
              box(width=NULL, solidHeader=TRUE, status="primary",
                radioButtons("radio", h3("Plot Limits"),
                  choices = list("Model min-max" = 1, "Fixed" = 2),
                  selected = 1)
                ),
              box(width=NULL, solidHeader=TRUE, status="primary",
                sliderInput("timeRange", label = "Time range", width = 300,
                  timeFormat="%F", 
                  min = as.POSIXct("2015-01-02 00:00:00",tz = 'GMT'),
                  max = as.POSIXct("2016-01-01 00:00:00",tz = 'GMT'),
                  value = c(as.POSIXct("2015-01-02 00:00:00",tz = 'GMT'),
                  as.POSIXct("2016-01-01 00:00:00",tz = 'GMT')))
                )
              ),
          #--
          #--
          column(width=8,
              box(width=NULL, solidHeader=TRUE, status="primary",
                plotOutput("timeplot")
              )
            )
        #--End Row
      )),
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
            column(2,
              numericInput("minTP", "Min TP", min=0, max=47.4, value=0)
            ),
            column(2,
              numericInput("maxTP", "Max TP", min=0, max=47.4, value=47.4)
            )
        ),
        #--End Row
        hr(),
        DT::dataTableOutput("stationstable")
    )
    
  ) #End tab items   

  )# End dashboard Body
) # End dashboard Page

