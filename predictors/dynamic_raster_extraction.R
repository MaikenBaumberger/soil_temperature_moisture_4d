
library(raster)
library(zoo)


ndwi <- brick("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2_NDWI_4326.tif")
ndvi <- brick("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dynamic_raster_variables/Sentinel2NDVI.tif")


#getting datetime from sentinel2 image name
bandnames = names(ndvi)
bandnames
for (i in bandnames) {
  date_u = substr(bandnames, 18,25)
  time_u = substr(bandnames, 27,32)
}
date_u
time_u
datetime= paste0(date_u,time_u)
datetime
datetime_u1 = c()
for (i in 1:length(datetime)) {
  datetime_u1 <- append(datetime_u1, as.POSIXct(datetime[i], format = "%Y%m%d%H%M%S"))
}
datetime_u1

#ndvi2 = ndvi[[2:3]]
#plot(ndvi2)

#ndvi_2= projectRaster(ndvi, crs = 4326)
#Warning messages:
#1: In projectRaster(ndvi, crs = 4326) : input and ouput crs are the same

#######################################
#predictor stack projection



#ext_ndvi = extract(ndvi,predictor_set$coordinates,df=T)

#id_datetime= c("id",datetime)
#data.frame(names(ext_ndvi),id_datetime)

#######################################

probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

probes = data.frame(probe_meta_data$probe_id,probe_meta_data$lat,probe_meta_data$lon)

names(probes) = c("probe_id","lat","lon")

probes$coordinates <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)

##########

ext_ndvi = extract(ndvi,probes$coordinates,df=T)

probes_ndvi = cbind(probes[1],ext_ndvi[2:102])

probes_ndvi_t = setNames(data.frame(t(probes_ndvi[,-1])), probes_ndvi[,1])

probes_ndvi_t= cbind(datetime_u1,probes_ndvi_t)


plot(probes_ndvi_t$datetime_u1, probes_ndvi_t$S02_002)

#calculate mean of douple time stamps
probes_ndvi_t_2 = data.frame(do.call(rbind,lapply(lapply(split(probes_ndvi_t,probes_ndvi_t$datetime_u1),`[`,2:265),colMeans,na.rm=T)))


#######################################

time= as.POSIXct(rownames(probes_ndvi_t_2))
probes_ndvi_t_2 <- cbind(time,probes_ndvi_t_2)

probes_ndvi_t_2$time = round(probes_ndvi_t_2$time,"hours")

ts_seq = data.frame(seq.POSIXt(probes_ndvi_t_2$time[1], 
                               probes_ndvi_t_2$time[length(probes_ndvi_t_2$time)], by="hour"))
names(ts_seq)="datetime"



ts_seq$datetime = as.POSIXct(strptime(ts_seq$datetime,"%Y-%m-%d %H:%M:%S"))
probes_ndvi_t_2$time = as.POSIXct(strptime(probes_ndvi_t_2$time,"%Y-%m-%d %H:%M:%S"))


probes_ndvi_hourly = merge(ts_seq,probes_ndvi_t_2,by.x="datetime",by.y="time",all.x=T)


########################################

#linear interpolation

for (i in c(2:length(probes_ndvi_hourly))){
  probes_ndvi_hourly[i] <- as.vector(na.approx(as.zoo(probes_ndvi_hourly[i]),maxgap=1000, na.rm=F))
}

plot(probes_ndvi_hourly$datetime,probes_ndvi_hourly$S04_009)

probes_ndvi_hourly <- cbind(probes_ndvi_hourly[1],round(probes_ndvi_hourly[2:length(probes_ndvi_hourly)], digits = 2))


#########################################


#melt(probes_ndvi_hourly, na.rm = FALSE, value.name = “datetime”, id = 'columns')
probes_ndvi_reshape = reshape2::melt(probes_ndvi_hourly, id = "datetime") 

names(probes_ndvi_reshape) = c("date_hour","probe_name","ndvi")


#########################################
#merge NDVI and predictor stack

load(file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")

#predictor_set$coordinates <- sf::st_as_sf(predictor_set,coords = c("lon","lat"), crs = 4326)

head(predictor_set)

predictor_set_2 = merge(predictor_set,probes_ndvi_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)

head(predictor_set_2)


plot(predictor_set_2$date_hour,predictor_set_2$ndvi)


###########################################
