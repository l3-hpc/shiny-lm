library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
require(ncdf4)
library(shinydashboard)

#Pothoven station data
load("Pothoven2010DF.Rdata")
load("PHS2010.Rdata")
#Mark's Rdata with grid data, loads 'coordsn'
load("LM_grid.Rdata")
npoints <- length(LM_grid$node)
#netCDF model output and Time
load("TP_2Layers_2010.Rdata")
#DolanLoadLocations
load("DolanLoadLocations.Rdata")
stationLocations <- PHS2010
stations <- stationLocations$Station
stationData <- Pothoven2010DF
whichstation <- "none"
loadedstation <-""

awesome <- makeAwesomeIcon(
  icon = "anchor",
  iconColor = "green",
  markerColor = "white",
  library = "fa"
)

function(input, output, session) {

  whichstation <- "None selected"
  loadedstation <- "None selected"
  output$whichstation <- renderText({whichstation})
  output$loadedstation <- renderText({loadedstation})

  ## Interactive Map ###########################################

  observe({
    event <- input$map_marker_click
    if (is.null(event))
      return()
    if (!any(stations==event$id))
      return()
     whichstation <<- event$id
     output$whichstation <- renderText({whichstation})
  })

  observeEvent(input$load, {
    loadedstation <<- whichstation
    output$loadedstation <- renderText({loadedstation})
  })  
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -86.2, lat = 43.2, zoom = 11)
  })
  
  
  

  observe({

      leafletProxy("map",data=stationLocations) %>%
#      clearShapes() %>%
      addAwesomeMarkers(~Lon, ~Lat, icon=awesome, label = ~Station, popup=~Station,layerId=~Station,
                 group="Stations") 
    
    leafletProxy("map",data=locations) %>%
      addMarkers(~lon, ~lat, popup=~trib,  label=~trib,layerId=~trib, 
               group="Tributaries")
 
      leafletProxy("map", data = LM_grid) %>%
          addCircles(~Lon, ~Lat, radius=700, layerId=LM_grid$Surface,
                    stroke=FALSE, fillOpacity=.8, fillColor="blue",group="Model") %>%

      addLayersControl(
        overlayGroups = c("Model","Tributaries","Stations"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })


  
  # This observer is for when a new data file is to be loaded.
  observe({
    
    event <- input$map_shape_click
    content <- "TP"

    if (is.null(event))
      return()
    
    isolate({

      #whichlayer <- (event$id %/% npoints) + 1
      whichindex <- event$id %% npoints
      GetPlot(whichindex) 
     
  })
  })
    
 
  GetIndicies <- function(id) { 
    whichlayer <- (id %/% npoints) + 1
    whichindex <- (id %% npoints)
    whichlon <- LM_grid$Lon[whichindex]
    whichlat <- LM_grid$Lat[whichindex]
    which <- list("index"=whichindex, "layer"=whichlayer,"lon"=whichlon,"lat"=whichlat)
    return(which)
  }
  
  # When map is clicked, show the station name in the viz tab
  observe({
    event <- input$map_shape_click
    isolate({
    gid <- GetIndicies(event$id)
    #content <- as.character(HTML(sprintf("Lat = %01.2f Lon = %01.2f Layer = %d",gid$lat,gid$lon,gid$layer)))
    content <- as.character(HTML(sprintf("Lat = %01.2f Lon = %01.2f",gid$lat,gid$lon)))
      })
    
      #renderUI({HTML(content)})
    if (is.null(event))
      content <- "None Selected"
      output$plotwin <- renderUI({HTML(content)})
    #renderUI({HTML(content)})
      return()
    
    isolate({
      output$plotwin <- renderUI({
        HTML(content)
      })
    })
  })
  

  GetPlot <- function(whichindex) { 
 
  #    Multiply to get ug/L
  TP_Surf <- BigTP_Layer1[whichindex,]*1000.
  TP_Bot <- BigTP_Layer20[whichindex,]*1000.
  
  output$timeplot <- renderPlot({
    
    is_min <- format(min(TP_Surf),digits=3)
    is_max <- format(max(TP_Surf),digits=3)
    is_mean <- format(mean(TP_Surf),digits=3)
    
    ib_min <- format(min(TP_Bot),digits=3)
    ib_max <- format(max(TP_Bot),digits=3)
    ib_mean <- format(mean(TP_Bot),digits=3)
    
    #xtitle=paste("Time: ",as.Date(input$timeRange[1])," until ",as.Date(input$timeRange[2]))
    #xlabel=paste(y_label," - ")
    #xs_label <- paste0("Surface, TPmin=",is_min,"    ",
    #                "TPmean=",is_mean,"    ",
    #                "TPmax=",is_max)
    #xb_label <- paste0("Bottom, TPmin=",ib_min,"    ",
    #                  "TPmean=",ib_mean,"    ",
    #                  "TPmax=",ib_max)
    
    if(input$radio == "1"){       
      ymi <- min(min(TP_Surf),min(TP_Bot))
      yma <- max(max(TP_Surf),max(TP_Bot))
    }else {
      ymi <- 2
      yma <- 12
    }
    
    ind_1 <- which(Time == input$timeRange[1])
    ind_2 <- which(Time == input$timeRange[2])
    Time <- Time[ind_1:ind_2]
    TP_Surf <- TP_Surf[ind_1:ind_2]
    TP_Bot<- TP_Bot[ind_1:ind_2]
    
    xlimits=c(input$timeRange[1],input$timeRange[2])      
    plot(Time,TP_Surf,main="Total Phosphorus - Modeled",ylab="TP um/L",cex=0.3,type="l",
         col="#006CD1",bg="#006CD1",ylim = c(ymi,yma),xlab="",xlim=xlimits)
    lines(Time,TP_Bot,pch=20,cex=0.3,type="l",col="#994F00",bg="#994F00")
    axis.Date(1, Time,format="%b %d")
    mtext("TP Bottom Layer", side=3, line=1, col="#994F00", cex=1, adj=1)
    mtext("TP Surface Layer", side=3, line=1, col="#006CD1", cex=1, adj=0)
    
    if(input$checkbox){
      if(loadedstation != "None selected"){ 
      plotstation <- stationData[stationData$Station == loadedstation,]
      points(plotstation$Time,plotstation$TP,pch=23,col="black",bg="#D9CA4B",xlab=plotstation$Station)
      mtext(plotstation$Station,side=1,cex=2,line=3)
      mtext(whichstation,side=4)
     }}
  })

  }

 
 
}
