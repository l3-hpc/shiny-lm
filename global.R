library(dplyr)

Year <- 2015

#StationData is from a csv from Wilson
#AllStations are the station/lat/lon for map markers
path <- file.path("data","AllStations.Rdata")
load(path)

#StationData is the actual Time/TP data, also has Depth
path <- file.path("data",Year,"StationData.Rdata")
load(path)

allstations <- AllStations$Station
allsources <- unique(AllStations$Source)

muskstations <- AllStations$Station[AllStations$Zone == "Muskegon_Grand_Zone"]
