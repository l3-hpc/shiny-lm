library(RColorBrewer)
library(scales)
library(lattice)
library(shinydashboard)

Year <- 2015

#Year dependent but same filenames
path <- file.path("data",Year,"TP_2Layers.Rdata")
#netCDF model output and Time
load(path)

#Not dependent on year:
#Original file Mark's Rdata LM_grid
#I saved just node, lat, lon
#This is for the whole FVCOM grid
path <- file.path("data","LM_gridpoints.Rdata")
load(path)
#Number of gridpoints in FVCOM grid
npoints <- length(LM_gridpoints$node)

#Location of Tributaries
#From Mark's original file, and I kept Trib, Lat/Lon, and USGS.Station
path <- file.path("data","DolanLoadLocations.Rdata")
load(path)

#StationData is from a csv from Wilson
#AllStations are the station/lat/lon for map markers
path <- file.path("data","AllStations.Rdata")
load(path)
#StationData is the actual Time/TP data, also has Depth
path <- file.path("data",Year,"StationData.Rdata")
load(path)


awesome <- makeAwesomeIcon(
  icon = "anchor",
  iconColor = "green",
  markerColor = "white",
  library = "fa"
)

function(input, output, session) {


  ## Interactive Map ###########################################
  output$timeplot <- renderPlot({
    plot(1,type="n",xlab="",ylab="",xaxt="n",yaxt="n")
    mtext("Click a gridpoint to start a plot.",side=3,line=-4,cex=1.5,col="#006CD1")
  })
  
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -86.2, lat = 43.2, zoom = 11)
  })
  
  
  
  observe({

      leafletProxy("map",data=AllStations) %>%
#      clearShapes() %>%
      addAwesomeMarkers(~Lon, ~Lat, icon=awesome, label = ~Station, popup=~Station,layerId=~Station,
                 group="Stations") 
    
    leafletProxy("map",data=DolanLoadLocations) %>%
      addMarkers(~Lon, ~Lat, popup=~Trib,  label=~Trib,layerId=~Trib, 
               group="Tributaries")
 
      leafletProxy("map", data = LM_gridpoints) %>%
          addCircles(~Lon, ~Lat, radius=800, layerId=LM_gridpoints$node,
                    stroke=FALSE, fillOpacity=.8, fillColor="blue",group="Model") %>%

      addLayersControl(
        overlayGroups = c("Model","Tributaries","Stations"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
  })

  
  # Start a plot when a gridpoint is clicked
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      #The event id is equal to the node
      GetPlot(event$id) 
    })
  })

  GetIndicies <- function(id) { 
    whichlon <- LM_gridpoints$Lon[id]
    whichlat <- LM_gridpoints$Lat[id]
    which <- list("lon"=whichlon,"lat"=whichlat)
    return(which)
  }    
 
  #This observes when the circle markers (model points) are clicked  
  observe({
    event <- input$map_shape_click
    
    isolate({
      #map returns layerid, which is the node.  GetIndicies returns lat/lon
      gid <- GetIndicies(event$id)
      # When map is clicked, show the Lat/Lon coordinates in the Model Data panel
      content <- as.character(HTML(sprintf("Lat = %01.2f Lon = %01.2f",gid$lat,gid$lon)))
    })
    
    if (is.null(event))
      content <- "None Selected"
      output$plotwin <- renderUI({HTML(content)})
      return()
      
    isolate({
      #This makes and empty plot with text, so the app doesn't start with
      #  a totally blank plot screen
      output$plotwin <- renderUI({
        HTML(content)
      })
    })
  })
  
  
  GetPlot <- function(inode) { 
 
    #    Multiply to get ug/L
    TP_Surf <- BigTP_Layer1[inode,]*1000.
    TP_Bot <- BigTP_Layer20[inode,]*1000.
  
    output$timeplot <- renderPlot({
    
      is_min <- format(min(TP_Surf),digits=3)
      is_max <- format(max(TP_Surf),digits=3)
      is_mean <- format(mean(TP_Surf),digits=3)
    
      ib_min <- format(min(TP_Bot),digits=3)
      ib_max <- format(max(TP_Bot),digits=3)
      ib_mean <- format(mean(TP_Bot),digits=3)
    
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
        if(input$dropstation != "None selected"){ 
        plotstation <- StationData[StationData$Station == input$dropstation,]
        points(plotstation$Date,plotstation$TP,pch=23,col="black",bg="#D9CA4B",cex=1.2,xlab=plotstation$Station)
        mtext(plotstation$Station,side=1,cex=2,line=3)
        mtext(input$dropstation,side=4)
      }}
  })

  }

 
 
}
