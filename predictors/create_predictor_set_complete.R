
library(raster)
library(zoo)

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_probes_data/list_probe_data.Rdata")

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

##################################

#create table from list
#write probe_name as a column

for (n in names(list_probes)){
  list_probes[[n]]['probe_name'] = n
}

table_probes = do.call("rbind",list_probes)

rownames(table_probes) <- NULL

table_probes[table_probes < -100] <- NA

##################################

#shifted air temperature

shift <- function(hours){
  a <- rep(NA, hours)
  seq = c(a, at_seq)
  seq = head(seq,-hours)
}

at_seq = meteo_data$air_temperature_mountain

meteo_data$air_temperature_mountain_1 = shift(1)
meteo_data$air_temperature_mountain_2 = shift(2)
meteo_data$air_temperature_mountain_3 = shift(3)
meteo_data$air_temperature_mountain_6 = shift(6)
meteo_data$air_temperature_mountain_12 = shift(12)
meteo_data$air_temperature_mountain_24 = shift(24)
meteo_data$air_temperature_mountain_48 = shift(48)
meteo_data$air_temperature_mountain_72 = shift(72)
meteo_data$air_temperature_mountain_96 = shift(96)
meteo_data$air_temperature_mountain_120 = shift(120)

at_seq = meteo_data$air_temperature_valley

meteo_data$air_temperature_valley_1 = shift(1)
meteo_data$air_temperature_valley_2 = shift(2)
meteo_data$air_temperature_valley_3 = shift(3)
meteo_data$air_temperature_valley_6 = shift(6)
meteo_data$air_temperature_valley_12 = shift(12)
meteo_data$air_temperature_valley_24 = shift(24)
meteo_data$air_temperature_valley_48 = shift(48)
meteo_data$air_temperature_valley_72 = shift(72)
meteo_data$air_temperature_valley_96 = shift(96)
meteo_data$air_temperature_valley_120 = shift(120)



####################################
#merge meteo data and soil data

table_probes$id  <- 1:nrow(table_probes)

predictor_set = merge(table_probes,meteo_data, by.x = "date_hour",by.y = "datetime", all.x = T)

predictor_set = predictor_set[order(predictor_set$id), ]

head(predictor_set)

##################################

#static feature

#data

soil_texture <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Texture_Fichtel_Mountains.grd")
soil_type <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Type_Fichtel_Mountains.grd")
elevation <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Elevation_Fichtel_Mountains.grd")
land_use <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Land_Use_Fichtel_Mountains.grd")
inclination <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Inclination_Fichtel_Mountains.tif")
exposition <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Exposition_Fichtel_Mountains.tif")
topo_wetness <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Topographic_Wettness_Fichtel_Mountains.tif")#calculated from dem using qgis

probes = data.frame(probe_meta_data$probe_id,probe_meta_data$plot_id,probe_meta_data$lat,probe_meta_data$lon)

names(probes) = c("probe_id","plot_id","lat","lon")


###########################
#projection

probes$coordinates <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)

soil_texture_2 <- projectRaster(soil_texture,crs = 4326,method = "ngb")
soil_type_2 <- projectRaster(soil_type,crs = 4326,method = "ngb")
elevation_2 <- projectRaster(elevation,crs = 4326,method = "ngb")
land_use_2 <- projectRaster(land_use,crs = 4326,method = "ngb")
inclination_2 <- projectRaster(inclination,crs = 4326,method = "ngb")
exposition_2 <- projectRaster(exposition,crs = 4326,method = "ngb")
topo_wetness_2 <- projectRaster(topo_wetness,crs = 4326,method = "ngb")

#mapview::mapview(land_use_2)+mapview::mapview(probes$coordinates)
#plot(topo_wetness_2)
#plot(probes$coordinates,add = TRUE)

##########################
#exposition to northness and eastness

northness <- cos(exposition_2 * pi / 180)
eastness <- sin(exposition_2 * pi / 180)

###########################
#extract raster values 


ext_soil_texture = extract(soil_texture_2,probes$coordinates,df=T)
probes$soil_texture = ext_soil_texture$soil_texture

ext_soil_type = extract(soil_type_2,probes$coordinates,df=T)
probes$soil_type = ext_soil_type$soil_type

ext_elevation = extract(elevation_2,probes$coordinates,df=T)
probes$elevation = ext_elevation$X20220324_Gelaendehoehe

ext_land_use = extract(land_use_2,probes$coordinates,df=T)
probes$land_use = ext_land_use$landuse

ext_inclination = extract(inclination_2,probes$coordinates,df=T)
probes$inclination = ext_inclination$Inclination_Fichtel_Mountains

ext_northness = extract(northness,probes$coordinates,df=T)
probes$northness = ext_northness$layer

ext_eastness = extract(eastness,probes$coordinates,df=T)
probes$eastness = ext_eastness$layer

ext_topo_wetness = extract(topo_wetness_2,probes$coordinates,df=T)
probes$topo_wetness = ext_topo_wetness$Topographic_Wettness_Fichtel_Mountains


probes_reduced = cbind(probes[1],probes[5:13])

predictor_set = merge(predictor_set, probes_reduced, by.x = "probe_name",by.y = "probe_id", all.x = T)

predictor_set = predictor_set[order(predictor_set$id), ]

head(predictor_set)



head(predictor_set)
################################

#################################



#elevation correction of temperature
#temp_mountain - ((temp_mountain-temp_valley)/(height_mountain-height_valley))*(height_mountain-height_location)


#plot(predictor_set$date_hour,predictor_set$air_temperature_mountain)


temperature_correction <- function(temp_mountain, temp_valley, elevation_location){
  elevation_mountain = 765 #Pflanzgarten 765 Weidebrunnen 775
  elevation_valley = 620  #Voitsumra
  temp_location = round(temp_mountain-((temp_mountain-temp_valley)/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation_location),2)
}

predictor_set$air_temperature_mountain = temperature_correction(predictor_set$air_temperature_mountain,
                                                                predictor_set$air_temperature_valley,
                                                                predictor_set$elevation) 

predictor_set$air_temperature_mountain_1 = temperature_correction(predictor_set$air_temperature_mountain_1,
                                                                  predictor_set$air_temperature_valley_1,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_2 = temperature_correction(predictor_set$air_temperature_mountain_2,
                                                                  predictor_set$air_temperature_valley_2,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_3 = temperature_correction(predictor_set$air_temperature_mountain_3,
                                                                  predictor_set$air_temperature_valley_3,
                                                                  predictor_set$elevation)

predictor_set$air_temperature_mountain_6 = temperature_correction(predictor_set$air_temperature_mountain_6,
                                                                  predictor_set$air_temperature_valley_6,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_12 = temperature_correction(predictor_set$air_temperature_mountain_12,
                                                                   predictor_set$air_temperature_valley_12,
                                                                   predictor_set$elevation) 

predictor_set$air_temperature_mountain_24 = temperature_correction(predictor_set$air_temperature_mountain_24,
                                                                   predictor_set$air_temperature_valley_24,
                                                                   predictor_set$elevation) 

predictor_set$air_temperature_mountain_48 = temperature_correction(predictor_set$air_temperature_mountain_48,
                                                                   predictor_set$air_temperature_valley_48,
                                                                   predictor_set$elevation) 

predictor_set$air_temperature_mountain_96 = temperature_correction(predictor_set$air_temperature_mountain_96,
                                                                   predictor_set$air_temperature_valley_96,
                                                                   predictor_set$elevation) 

predictor_set$air_temperature_mountain_120 = temperature_correction(predictor_set$air_temperature_mountain_120,
                                                                    predictor_set$air_temperature_valley_120,
                                                                    predictor_set$elevation) 


plot(predictor_set$date_hour,predictor_set$air_temperature_mountain)
plot(predictor_set$date_hour,predictor_set$air_temperature_valley)

hist(predictor_set$elevation,50)

plot(meteo_data$datetime,meteo_data$air_temperature_valley,type="l",col="blue")
lines(meteo_data$datetime,meteo_data$air_temperature_mountain,col="red")


#temperatures voitsumra very low 

predictor_set = predictor_set[ , -which(names(predictor_set) %in% c("air_temperature_valley_1","air_temperature_valley_2","air_temperature_valley_3",
                                                                    "air_temperature_valley_6", "air_temperature_valley_12","air_temperature_valley_24",
                                                                    "air_temperature_valley_48","air_temperature_valley_72","air_temperature_valley_96",
                                                                    "air_temperature_valley_120"))]

###########################
#dynamic raster

#data

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

#load(file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")

#predictor_set$coordinates <- sf::st_as_sf(predictor_set,coords = c("lon","lat"), crs = 4326)

head(predictor_set)

predictor_set_2 = merge(predictor_set,probes_ndvi_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)
predictor_set_2 = merge(predictor_set_2,probes_ndwi_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)
predictor_set_2 = merge(predictor_set_2,probes_radar_reshape, by.x = c("probe_name","date_hour"), by.y =  c("probe_name","date_hour"),all.x=T)

head(predictor_set_2)

predictor_set_2$plot_id = predictor_set_2$coordinates$plot_id

predictor_set_2 = predictor_set_2[ , -which(names(predictor_set_2) %in% c("coordinates"))]

predictor_set_2$date = format(as.POSIXct(predictor_set_2$date_hour,
                                          format = '%Y-%m-%d %H:%M:%S'),
                               format = '%Y-%m-%d')

head(predictor_set_2)

data.table::uniqueN(predictor_set_2$probe_name)
data.table::uniqueN(predictor_set_2$date)
data.table::uniqueN(predictor_set_2$plot_id)

#############################

#save(predictor_set_2,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_plot_id.Rdata")






