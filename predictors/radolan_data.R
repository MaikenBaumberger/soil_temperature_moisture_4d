####
#This script downloads the recent RADOLAN RW data from
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/recent/
# Historical data can be downloaded from https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/asc/
# In this case change url below


library(terra)
##################
# PARAMETERS (NEED USER ADJUSTMENT)
##################

### date to download
date <- "20230101"
# destination of downloaded files
testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan"

##################
# Download and extract
##################

url <- paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/recent/asc/RW-",date,".tar.gz")
url
testfile <- paste0(testfolder,"RW-",date,".tar.gz")
testfile
testfolder
download.file(url, testfile)
untar(testfile,exdir=testfolder)

##################
# Select all rasters from the selected date
##################
files <- list.files(testfolder,pattern=".asc$", full.names=TRUE)
files <- files[grepl(date,files)]

##################
# Read raster and project
##################



file <- terra::rast(files)
crs(file) <- "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0 +a=6370040 +b=6370040 +units=m"

##################
# Visualize
##################

plot(file[[4]])

file

### optional:
library(mapview)
library(raster)
file_rast = raster(file[[1]])
mapview(file_rast)

file_rast

###################
