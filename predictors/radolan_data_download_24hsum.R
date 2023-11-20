####
#This script downloads the recent RADOLAN RW data from
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/recent/
# Historical data can be downloaded from https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/asc/
# In this case change url below


library(terra)
library(raster)
##################
# PARAMETERS (NEED USER ADJUSTMENT)
##################



# destination of downloaded files
testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_24hsum_2/"

##################
# Download and extract
##################

### date to download
date <- 202201
end <- 202212
while (date <= end){
  
  url <- paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/2022/SF",date,".tar.gz")
  testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_24hsum_2/"
  testfile <- paste0(testfolder,"SF",date,".tar.gz")
  download.file(url, testfile, mode="wb")
  untar(testfile,exdir=testfolder)
  date <- date + 1
}

#untar the hourly data from daily data

start <- 20211201
end1 <- 20221231
#testfolder1 <- "E:/Thesis Research/radolan/Hourly radolan data"
while (start <= end1){
  testfile1 <- paste0(testfolder,"SF",start,".tar.gz")
  untar(testfile1,exdir=testfolder)
  start <- start + 1
}
