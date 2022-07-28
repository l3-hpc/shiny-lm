library(leaflet)
options( warn = -1 )


navbarPage("Lake Michigan", id="nav",
           
    tabPanel("Interactive map",
        div(class="outer",
            
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
        ),
        
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width="100%", height="100%"),
        
        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                      draggable = TRUE, top = 15, left = "auto", right = 80, bottom = "auto",
                      width = 550, height = "auto",
                      h3("Click on a model point for a plot"),
                      htmlOutput("plotwin"),
                      textOutput("whichstation"),
                      
                      fluidRow(
                        
                        column(3,
                               h3("Plot Station?"),
                               checkboxInput("checkbox", "Check for Yes", value = FALSE)),
 
                   column(3,
                        radioButtons("radio", h3("Plot Limits"),
                                     choices = list("Model min-max" = 1, "Fixed" = 2),
                                                    selected = 1)),
                     column(6, sliderInput("timeRange", label = "Time range", width = 300,
                                           timeFormat="%F", 
                                        min = as.POSIXct("2010-01-02 00:00:00",tz = 'GMT'),
                                        max = as.POSIXct("2011-01-01 00:00:00",tz = 'GMT'),
                                        value = c(as.POSIXct("2010-01-01 00:00:00",tz = 'GMT'),
                                                  as.POSIXct("2010-12-31 00:00:00",tz = 'GMT'))))),
                      
                     ),
        
        absolutePanel(id = "controls2", class = "panel panel-default", fixed = FALSE,
                      draggable = TRUE, left="auto", right = 80, top = 260, bottom = "auto",
                      width = 550, height="auto",
                      
                      plotOutput("timeplot",width=550) )     
                      
        ,
        tags$div(id="cite",
                 'Thank You SuperZip for your example! https://shiny.rstudio.com/gallery/superzip-example.html'
        )
        
        
        
    )
    )
)
            
