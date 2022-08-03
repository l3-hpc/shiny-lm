library(dplyr)

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
path <- file.path("data/2015/StationData.Rdata")
load(path)

allstations <- AllStations$Station
allsources <- unique(AllStations$Source)


