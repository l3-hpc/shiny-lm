#Some stations are at the same location.  Concatenate these names.
library(dplyr)
library(stringr)  
library(stringi)
library(lubridate)

#For 2015 stations
df_year = "2015"

#---
# Read and Sort Data for entire dataset (df)
#---
#Read data
path <- "/Users/lllowe/JamesPaper/"
csvfile <- file.path(path,"TP_Data_Nodes_XY.csv")
df <- read.csv(csvfile)

#Sort out the year
df <- df[df$Year==df_year,]

#Remove values where TP is not defined (not a number)
df <- df[!is.na(as.numeric(as.character(df$Tp_ug_L))),]

#---
# Find stations at the same location
#---
#Start with another dataframe, df2
df2 <- df
#Keep only unique station names
df2 <- distinct(df2,Station,.keep_all=TRUE)
#Keep only repeated nodes (Nodes that occur more than 1)
df2 <- subset(df2, Node %in% names(which(table(Node) > 1)))
#Sort by node, so repeated values are next to each other
df2 <- df2[order(df2$Node),]
#Length of dataframe
isn <- length(df2$Node)

#---
# Make new names
#---
#Keep previous node value to avoid adding the same names after renaming the first index
previousnode <- ""
#Loop over the 'repeated node' dataframe, df2
for (i in 1:isn){
  #After finding the first name, rename others and skip the ith iteration
  if(df2$Node[i] == previousnode) {
    df2$Station[i] <- df2$Station[i-1]
  }else{
    #If not, get the station names of all the repeated indicies 
    bsn <- df2[df2$Node == df2$Node[i],]
    #Find how many names
    ibn <- length(bsn$Node)
    #Start with empty string
    chn <- ""
    for (j in 1:ibn){
      #Make a new name by adding the names
      #If it is not the last name, separate with semicolon
      if(j != ibn){
       chn <- paste0(chn,bsn$Station[j],sep="; ")
      }else{   #For the last one, no semicolon
        chn <- paste0(chn,bsn$Station[j])
      }
    }
    #Rename the station with the calculted string
    df2$Station[i] <- chn
    #Avoid an infinite adding of previous strings
    previousnode <- df2$Node[i] 
  }
}

#---
# Modify the original data frame
#---
#Remove the repeated indicies for a list of new names for duplicate stations
df2 <- distinct(df2,Node,.keep_all=TRUE)

#Original data frame is df.  Make a copy and rename the nodes.
lastdf <- df
#Number of measurements
df2loop <- length(lastdf$Year)
#Number of repeated indicies
myloop <- length(unique(df2$Node))

#For every line in the original data frame
for(j in 1:df2loop){
  #Look for the repeated nodes
  for(i in 1:myloop){
     if(lastdf$Node[j] == df2$Node[i]){
       #If you find one, rename it
       lastdf$Station[j] = df2$Station[i]
     }
  }
}
#Save it just in case
save(lastdf,file="~/JamesPaper/SaveRenamed_lastdf.Rdata")

#---
# Save the data in R Shiny format
#---

#AllStations has just the node/lat/lon/Station info for the markers
AllStations <- lastdf[,c('Station','Node','Latitude','Longitude','Source','Zone')]
AllStations <- distinct(AllStations,Station,.keep_all=TRUE)
#Add a distinct layer id
rstring <- stri_rand_strings(131,20)
AllStations$layerId <- rstring
#Names used are Station, Lat, Lon, Node
colnames(AllStations) <- c('Station','Node','Lat','Lon','Source','Zone','layerId')
save(AllStations,file="~/R_apps/shiny-lm/data/AllStations.Rdata")

#Data for the measurements/plots
#Station, Time (POSIXct), TP, Depth, Source, Node, Lat, Lon
StationData <- lastdf[,c('Station','Latitude','Longitude','Date','Tp_ug_L','Depth','Source','Node')]
StationData$Date <- as.POSIXct(StationData$Date, format="%m/%d/%Y",tz="GMT")

StationData$layerId <- StationData$Node

for(i in 1:length(StationData$Node)){
    the_stations <- AllStations[AllStations$Station == StationData$Station[i],]
    StationData$layerId[i] <- the_stations$layerId
  }
  


colnames(StationData) <- c('Station','Lat','Lon','Date','TP','Depth','Source','Node','layerId')
filename <- "~/R_apps/shiny-lm/data/2015/StationData.Rdata"
save(StationData,file=filename)





