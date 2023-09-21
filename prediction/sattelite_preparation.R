
ndwi <- brick("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2_NDWI_4326.tif")
ndvi <- brick("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2NDVI.tif")
radar <-  brick("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel_1_2022_jan_dec.tif")
#radar$S1A_IW_GRDH_1SDV_20220104T170803_20220104T170828_041314_04E951_59F8_VV

##################################################

#getting datetime from sentinel2 image name
bandnames_ndvi = names(ndvi)
for (i in bandnames_ndvi) {
  date_ndvi = substr(bandnames_ndvi, 18,25)
  time_ndvi = substr(bandnames_ndvi, 27,32)
}

datetime_ndvi= paste0(date_ndvi,time_ndvi)
datetime_ndvi_posix = c()
for (i in 1:length(datetime_ndvi)) {
  datetime_ndvi_posix <- append(datetime_ndvi_posix, as.POSIXct(datetime_ndvi[i], format = "%Y%m%d%H%M%S"))
}
datetime_ndvi_posix

#######

bandnames_ndwi = names(ndwi)
for (i in bandnames_ndwi) {
  date_ndwi = substr(bandnames_ndwi, 18,25)
  time_ndwi = substr(bandnames_ndwi, 27,32)
}

datetime_ndwi= paste0(date_ndwi,time_ndwi)
datetime_ndwi_posix = c()
for (i in 1:length(datetime_ndwi)) {
  datetime_ndwi_posix <- append(datetime_ndwi_posix, as.POSIXct(datetime_ndwi[i], format = "%Y%m%d%H%M%S"))
}
datetime_ndwi_posix


#######

bandnames_radar = names(radar)
for (i in bandnames_ndwi) {
  date_radar = substr(bandnames_radar, 18,25)
  time_radar = substr(bandnames_radar, 27,32)
}

datetime_radar= paste0(date_radar,time_radar)
datetime_radar_posix = c()
for (i in 1:length(datetime_radar)) {
  datetime_radar_posix <- append(datetime_radar_posix, as.POSIXct(datetime_radar[i], format = "%Y%m%d%H%M%S"))
}
datetime_radar_posix





test=mean(ndwi[[97]],ndwi[[98]],ndwi[[99]],ndwi[[100]],ndwi[[101]],na.rm = TRUE)
plot(test)

test=mean(ndwi[[1]],ndwi[[2]],ndwi[[3]],ndwi[[4]],ndwi[[5]],ndwi[[6]],na.rm = TRUE)
plot(test)

nums <- seq(1, 1, length = 101)

plot(datetime_ndvi_posix,nums)



####################################################

ext_ndvi = extract(ndvi,probes$coordinates,df=T)
probes_ndvi = cbind(probes[1],ext_ndvi[2:102])
probes_ndvi_t = setNames(data.frame(t(probes_ndvi[,-1])), probes_ndvi[,1])
probes_ndvi_t= cbind(datetime_ndvi_posix,probes_ndvi_t)
#plot(probes_ndvi_t$datetime_ndvi_posix, probes_ndvi_t$S02_002)

#calculate mean of douple time stamps
probes_ndvi_t_2 = data.frame(do.call(rbind,lapply(lapply(split(probes_ndvi_t,probes_ndvi_t$datetime_ndvi_posix),`[`,2:265),colMeans,na.rm=T)))

