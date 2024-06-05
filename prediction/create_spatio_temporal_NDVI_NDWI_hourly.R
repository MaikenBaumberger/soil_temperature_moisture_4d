

####################################################

#https://rdrr.io/cran/terra/man/approximate.html
library(terra)


ndvi <- rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2_NDWI_4326.tif")
#ndvi <- rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2NDVI.tif")

b1 = ndvi[[1:10]]

#plot(b1)

#getting datetime from sentinel2 image name
bandnames_ndvi = names(ndvi)
for (i in bandnames_ndvi) {
  date_ndvi = substr(bandnames_ndvi, 17,24)
  time_ndvi = substr(bandnames_ndvi, 26,31)
}

datetime_ndvi= paste0(date_ndvi,time_ndvi)
datetime_ndvi_posix = c()
for (i in 1:length(datetime_ndvi)) {
  datetime_ndvi_posix <- append(datetime_ndvi_posix, as.POSIXct(datetime_ndvi[i], format = "%Y%m%d%H%M%S",tz="Etc/GMT-1"))
}
datetime_ndvi_posix

datehour_ndvi_posix = as.POSIXct(format(round(datetime_ndvi_posix, units="hours"), format="%Y-%m-%d %H:%M",tz="Etc/GMT-1"))

datehour_ndvi_posix

#date_ndvi_posix <- as.Date(datetime_ndvi_posix)
#date_ndvi_posix

date_ndvi_posix_non_duplicate = unique(datehour_ndvi_posix)

#calculate daily mean if two rasters are available
#daymean = terra::tapp(b1, date_ndvi[1:10], mean, na.rm=T)
daymean = terra::tapp(ndvi, date_ndvi, mean, na.rm=T)

#plot(daymean)



#create emty rasters for all days with no data
time(daymean) = date_ndvi_posix_non_duplicate#[1:7]
names(daymean) = date_ndvi_posix_non_duplicate#[1:7]
# ss <- fillTime(daymean)
# 
# plot(ss)
# 
# 
# a <- approximate(ss, rule=2)
# 
# plot(a)

50000/2^20

memory.limit()
mem_info(empty_raster)

empty_raster = ndvi[[1]]*NA
names(empty_raster) = "empty"
#empty_raster_full = empty_raster


# 
# for (i in 1:9) {
#   print(i)
#   empty_raster_full = c(empty_raster_full,empty_raster_full)
# }


empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)

#empty_raster_full = c(empty_raster_full,empty_raster_full)

empty_raster_full <- empty_raster[[1:365]]

#plot(empty_raster_full)

date_seq = seq(as.POSIXct("2022-01-01 00:00:00", tz="Etc/GMT-1"), as.POSIXct("2022-12-31 23:00:00" , tz="Etc/GMT-1"), by="hour") 
  #seq(as.Date("2022-01-01"),by="hour",length.out=365)


names(empty_raster_full) = date_seq#[1:21]
time(empty_raster_full) <- date_seq#[1:21]


ndvi_empty = c(empty_raster_full,daymean)

names(ndvi_empty)

time_ndvi_empty = time(ndvi_empty)

ndvi_daily_empty = terra::tapp(ndvi_empty, names(ndvi_empty), mean, na.rm=T)

#plot(ndvi_daily_empty)





ndvi_daily_filled <- approximate(ndvi_daily_empty, rule=2)
 
#plot(ndvi_daily_filled)


