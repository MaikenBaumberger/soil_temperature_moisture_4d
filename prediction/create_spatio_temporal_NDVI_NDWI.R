

####################################################

#https://rdrr.io/cran/terra/man/approximate.html
library(terra)


#ndvi <- rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2_NDWI_4326.tif")
ndvi <- rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2NDVI.tif")

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
date_ndvi_posix <- as.Date(datetime_ndvi_posix)
date_ndvi_posix

date_ndvi_posix_non_duplicate = unique(date_ndvi_posix)

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


empty_raster = ndvi[[1]]*NA
names(empty_raster) = "empty"
#empty_raster_full = empty_raster

# for (i in 1:20) {
#   empty_raster_full = c(empty_raster_full,empty_raster)
# }

empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)
empty_raster = c(empty_raster,empty_raster)

#plot(empty_raster_full)

date_seq = seq(as.Date("2022-01-01"),by="day",length.out=365)

#date_seq = date_seq[seq(1, 365, by = 2)]

empty_raster_full <- empty_raster[[1:200]]

names(empty_raster_full) = date_seq[1:200]
time(empty_raster_full) = date_seq[1:200]


ndvi_empty = c(empty_raster_full,daymean[[1:38]])
names(ndvi_empty)
time_ndvi_empty = time(ndvi_empty)
ndvi_daily_empty = terra::tapp(ndvi_empty, names(ndvi_empty), mean, na.rm=T)
names(ndvi_daily_empty)
#ndvi_daily_empty = ndvi_daily_empty[[1:200]]

#ndvi_daily_filled <- approximate(ndvi_daily_empty, rule=2)
 
#plot(ndvi_daily_empty[[1:16]])


names(daymean)

empty_raster_full_2 <- empty_raster[[1:200]]

names(empty_raster_full_2) = date_seq[166:365]
time(empty_raster_full_2) = date_seq[166:365]

ndvi_empty_2 = c(empty_raster_full_2,daymean[[39:59]])
names(ndvi_empty_2)
time_ndvi_empty_2 = time(ndvi_empty_2)
ndvi_daily_empty_2 = terra::tapp(ndvi_empty_2, names(ndvi_empty_2), mean, na.rm=T)
names(ndvi_daily_empty_2)
#ndvi_daily_empty = ndvi_daily_empty[[1:200]]


ndvi_daily_empty_combined = c(ndvi_daily_empty,ndvi_daily_empty_2[[36:200]])
names(ndvi_daily_empty_combined)

ndvi_daily_filled <- approximate(ndvi_daily_empty_combined, rule=2)

plot(ndvi_daily_filled[[120]])

#ndwi_daily_filled = ndvi_daily_filled

#plot(ndwi_daily_filled[[50]])

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily")
writeRaster(ndvi_daily_filled,"ndvi_daily_filled.tif",overwrite=TRUE)#,datatype='INT2S'




#ndwi_load = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndwi_daily_filled.tif")

#plot(ndwi_load[[90]])


ndwi_load = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndwi_daily_filled.tif")

ndvi_load = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndvi_daily_filled.tif")

par(mfrow =c(2,1))


plot(ndvi_load[[200]])
plot(ndwi_load[[200]])
