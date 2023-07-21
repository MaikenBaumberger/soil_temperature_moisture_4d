
library(raster)
library(zoo)

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

###################################################

#soil probe meta data

probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

probes = data.frame(probe_meta_data$probe_id,probe_meta_data$lat,probe_meta_data$lon)
names(probes) = c("probe_id","lat","lon")
probes$coordinates <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)


####################################################

ext_ndvi = extract(ndvi,probes$coordinates,df=T)
probes_ndvi = cbind(probes[1],ext_ndvi[2:102])
probes_ndvi_t = setNames(data.frame(t(probes_ndvi[,-1])), probes_ndvi[,1])
probes_ndvi_t= cbind(datetime_ndvi_posix,probes_ndvi_t)
#plot(probes_ndvi_t$datetime_ndvi_posix, probes_ndvi_t$S02_002)

#calculate mean of douple time stamps
probes_ndvi_t_2 = data.frame(do.call(rbind,lapply(lapply(split(probes_ndvi_t,probes_ndvi_t$datetime_ndvi_posix),`[`,2:265),colMeans,na.rm=T)))

##########


ext_ndwi = extract(ndwi,probes$coordinates,df=T)
probes_ndwi = cbind(probes[1],ext_ndwi[2:102])
probes_ndwi_t = setNames(data.frame(t(probes_ndwi[,-1])), probes_ndwi[,1])
probes_ndwi_t= cbind(datetime_ndwi_posix,probes_ndwi_t)
#plot(probes_ndvi_t$datetime_ndvi_posix, probes_ndvi_t$S02_002)

#calculate mean of douple time stamps
probes_ndwi_t_2 = data.frame(do.call(rbind,lapply(lapply(split(probes_ndwi_t,probes_ndwi_t$datetime_ndwi_posix),`[`,2:265),colMeans,na.rm=T)))

##########


ext_radar = extract(radar,probes$coordinates,df=T)
probes_radar = cbind(probes[1],ext_radar[2:60])
probes_radar_t = setNames(data.frame(t(probes_radar[,-1])), probes_radar[,1])
probes_radar_t= cbind(datetime_radar_posix,probes_radar_t)
#plot(probes_radar_t$datetime_radar_posix, probes_radar_t$S07_032)

#calculate mean of douple time stamps
probes_radar_t_2 = data.frame(do.call(rbind,lapply(lapply(split(probes_radar_t,probes_radar_t$datetime_radar_posix),`[`,2:265),colMeans,na.rm=T)))


####################################################

time_ndvi= as.POSIXct(rownames(probes_ndvi_t_2))
probes_ndvi_t_2 <- cbind(time_ndvi,probes_ndvi_t_2)
probes_ndvi_t_2$time_ndvi = round(probes_ndvi_t_2$time_ndvi,"hours")

ts_seq = data.frame(seq.POSIXt(probes_ndvi_t_2$time_ndvi[1],probes_ndvi_t_2$time_ndvi[length(probes_ndvi_t_2$time_ndvi)], by="hour"))
names(ts_seq)="datetime"
ts_seq$datetime = as.POSIXct(strptime(ts_seq$datetime,"%Y-%m-%d %H:%M:%S"))

probes_ndvi_t_2$time_ndvi = as.POSIXct(strptime(probes_ndvi_t_2$time_ndvi,"%Y-%m-%d %H:%M:%S"))
probes_ndvi_hourly = merge(ts_seq,probes_ndvi_t_2,by.x="datetime",by.y="time_ndvi",all.x=T)

#########

time_ndwi= as.POSIXct(rownames(probes_ndwi_t_2))
probes_ndwi_t_2 <- cbind(time_ndwi,probes_ndwi_t_2)
probes_ndwi_t_2$time_ndwi = round(probes_ndwi_t_2$time_ndwi,"hours")

probes_ndwi_t_2$time_ndwi = as.POSIXct(strptime(probes_ndwi_t_2$time_ndwi,"%Y-%m-%d %H:%M:%S"))
probes_ndwi_hourly = merge(ts_seq,probes_ndwi_t_2,by.x="datetime",by.y="time_ndwi",all.x=T)


##########

time_radar= as.POSIXct(rownames(probes_radar_t_2))
probes_radar_t_2 <- cbind(time_radar,probes_radar_t_2)
probes_radar_t_2$time_radar = round(probes_radar_t_2$time_radar,"hours")

probes_radar_t_2$time_radar = as.POSIXct(strptime(probes_radar_t_2$time_radar,"%Y-%m-%d %H:%M:%S"))
probes_radar_hourly = merge(ts_seq,probes_radar_t_2,by.x="datetime",by.y="time_radar",all.x=T)


########################################

#linear interpolation from weekly resolution to hourly resolution

for (i in c(2:length(probes_ndvi_hourly))){
  probes_ndvi_hourly[i] <- as.vector(na.approx(as.zoo(probes_ndvi_hourly[i]),maxgap=1000, na.rm=F))
}

probes_ndvi_hourly <- cbind(probes_ndvi_hourly[1],round(probes_ndvi_hourly[2:length(probes_ndvi_hourly)], digits = 2))

#########

for (i in c(2:length(probes_ndwi_hourly))){
  probes_ndwi_hourly[i] <- as.vector(na.approx(as.zoo(probes_ndwi_hourly[i]),maxgap=1000, na.rm=F))
}

probes_ndwi_hourly <- cbind(probes_ndwi_hourly[1],round(probes_ndwi_hourly[2:length(probes_ndwi_hourly)], digits = 2))


############################################################

#function for nearest neighbour gap filling

probes_radar_hourly_1 = probes_radar_hourly

#https://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value


f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}



for(i in 2:265){
  print(colnames(probes_radar_hourly[i]))
  probes_radar_hourly[i]=f1(probes_radar_hourly[,i])
}


plot(probes_radar_hourly$datetime,probes_radar_hourly$S11_003)
points(probes_radar_hourly_1$datetime,probes_radar_hourly_1$S11_003,col="red",pch=20)


probes_radar_hourly <- cbind(probes_radar_hourly[1],round(probes_radar_hourly[2:length(probes_radar_hourly)], digits = 2))


###########################################################

#transform table combining date, probe name and ndvi/ndwi in one row

probes_ndvi_reshape = reshape2::melt(probes_ndvi_hourly, id = "datetime") 
names(probes_ndvi_reshape) = c("date_hour","probe_name","ndvi")

probes_ndwi_reshape = reshape2::melt(probes_ndwi_hourly, id = "datetime") 
names(probes_ndwi_reshape) = c("date_hour","probe_name","ndwi")

probes_radar_reshape = reshape2::melt(probes_radar_hourly, id = "datetime") 
names(probes_radar_reshape) = c("date_hour","probe_name","radar")


#########################################
#merge NDVI and predictor stack

load(file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")

#predictor_set$coordinates <- sf::st_as_sf(predictor_set,coords = c("lon","lat"), crs = 4326)

head(predictor_set)

predictor_set_2 = merge(predictor_set,probes_ndvi_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)
predictor_set_2 = merge(predictor_set_2,probes_ndwi_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)
predictor_set_2 = merge(predictor_set_2,probes_radar_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)

head(predictor_set_2)

plot(predictor_set_2$date_hour,predictor_set_2$ndvi)
plot(predictor_set_2$date_hour,predictor_set_2$ndwi)
plot(predictor_set_2$date_hour,predictor_set_2$radar)

###########################################


plot(predictor_set_2$T_05,predictor_set_2$ndvi)




