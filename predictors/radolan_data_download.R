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
testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data/"

##################
# Download and extract
##################

### date to download
date <- 202112
end <- 202112
while (date <= end){
  
  url <- paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/asc/2021/RW-",date,".tar")
  testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data/"
  testfile <- paste0(testfolder,"RW-",date,".tar")
  download.file(url, testfile, mode="wb")
  untar(testfile,exdir=testfolder)
  date <- date + 1
}

#untar the hourly data from daily data

start <- 20211201
end1 <- 20211231
#testfolder1 <- "E:/Thesis Research/radolan/Hourly radolan data"
while (start <= end1){
  testfile1 <- paste0(testfolder,"RW-",start,".tar.gz")
  untar(testfile1,exdir=testfolder)
  start <- start + 1
}



# 
# library(terra)
# library(raster)
# ##################
# # PARAMETERS (NEED USER ADJUSTMENT)
# ##################
# 
# 
# 
# # destination of downloaded files
# testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data/"
# 
# ##################
# # Download and extract
# ##################
# 
# ### date to download
# date <- 202201
# end <- 202212
# while (date <= end){
#   
#   url <- paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/asc/2022/RW-",date,".tar")
#   testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data/"
#   testfile <- paste0(testfolder,"RW-",date,".tar")
#   download.file(url, testfile, mode="wb")
#   untar(testfile,exdir=testfolder)
#   date <- date + 1
# }
# 
# #untar the hourly data from daily data
# 
# start <- 20220101
# end1 <- 20221231
# #testfolder1 <- "E:/Thesis Research/radolan/Hourly radolan data"
# while (start <= end1){
#   testfile1 <- paste0(testfolder,"RW-",start,".tar.gz")
#   untar(testfile1,exdir=testfolder)
#   start <- start + 1
# }