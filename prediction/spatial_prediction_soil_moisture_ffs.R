library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)


#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d/soil_temperature_moisture_4d/model_results/compare_train_ffs")
setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_moisture_model_v1")

#load("test_set.Rdata")
#load("train_set.Rdata")
#load("cv_spacetimefolds.Rdata")
load("ffs_model.Rdata")

ffsmodel
ffsmodel$selectedvars


##################################

#DI = trainDI(model=ffsmodel)

#save(DI,file = "DI.Rdata")

####################################



##prediction preparation

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

static_raster= stack("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

elevation_mountain = 765 #Pflanzgarten 765 Weidebrunnen 775
elevation_valley = 620  #Voitsumra

elevation = static_raster$elevation

raster_base = elevation*0

# meteo_data$air_temperature_day = zoo::rollmean(meteo_data$air_temperature_mountain, k = 24, fill = NA)#,align ="right"
# meteo_data$air_temperature_week = zoo::rollmean(meteo_data$air_temperature_mountain, k = 168, fill = NA)
# meteo_data$air_temperature_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 720, fill = NA)


load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/trends.RData")
head(trends)

#precipitaion

#precipitaion

meteo_data$prec_sum_3_month = zoo::rollsum(meteo_data$precipitation, 168*12, align = "right", fill=NA)

meteo_data$prec_sum_4_month = zoo::rollsum(meteo_data$precipitation, 168*16, align = "right", fill=NA)

meteo_data$prec_sum_5_month = zoo::rollsum(meteo_data$precipitation, 168*20, align = "right", fill=NA)


#############################
#############################

#prediction


meteo_data$datetime[5485]




#position = 5485 #"2022-01-15 12:00:00 CET"
#position = 6229 #"2022-02-15 12:00:00 CET"
#position = 6901 #"2022-03-15 12:00:00 CET"
#position = 7645 #"2022-04-15 12:00:00 CEST"
#position = 8365 #"2022-05-15 12:00:00 CEST"
#position = 9109 #"2022-06-15 12:00:00 CEST"
#position = 9829 #"2022-07-15 12:00:00 CEST"
#position = 10573 #"2022-08-15 12:00:00 CEST"
#position = 11317 #"2022-09-15 12:00:00 CEST"
#position = 12037 #"2022-10-15 12:00:00 CEST"
#position = 12781 #"2022-11-15 12:00:00 CET"
position = 13501 #"2022-12-15 12:00:00 CET"

date = strftime(meteo_data$datetime[9000], "%Y%m%d")

#time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")

for (i in c(5485,6229)){
  
  print(i)
  position = i
  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  date = strftime(meteo_data$datetime[i], "%Y%m%d")
  print(time)
  
  
  
  #meteo_data$datetime[position]
  
  #depths = 5
  
  prec_sum_3_month_spatially = raster_base + meteo_data$prec_sum_3_month[position]
  
  prec_sum_5_month_spatially = raster_base + meteo_data$prec_sum_5_month[position]
  
  # trend_3_month_spatially = raster_base + trends$trend_3month[position]
  
  # temp_spatially = round(meteo_data$air_temperature_mountain[position]-((meteo_data$air_temperature_mountain[position]-meteo_data$air_temperature_valley[position])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # radiation_spatially = raster_base+meteo_data$global_radiation[position]
  
  # humidity_spatially = raster_base+meteo_data$relative_humidity[position]
   
  # pressure_spatially = raster_base+meteo_data$air_pressure[position]
  
  # wind_spatially = raster_base+meteo_data$wind_speed[position]
  
  # precipitation_spatially = raster_base+meteo_data$precipitation[position]
  
  # temp_spatially_3 = round(meteo_data$air_temperature_mountain[position-3]-((meteo_data$air_temperature_mountain[position-3]-meteo_data$air_temperature_valley[position-3])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_6 = round(meteo_data$air_temperature_mountain[position-6]-((meteo_data$air_temperature_mountain[position-6]-meteo_data$air_temperature_valley[position-6])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_12 = round(meteo_data$air_temperature_mountain[position-12]-((meteo_data$air_temperature_mountain[position-12]-meteo_data$air_temperature_valley[position-12])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_24 = round(meteo_data$air_temperature_mountain[position-24]-((meteo_data$air_temperature_mountain[position-24]-meteo_data$air_temperature_valley[position-24])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_48 = round(meteo_data$air_temperature_mountain[position-48]-((meteo_data$air_temperature_mountain[position-48]-meteo_data$air_temperature_valley[position-48])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_96 = round(meteo_data$air_temperature_mountain[position-96]-((meteo_data$air_temperature_mountain[position-96]-meteo_data$air_temperature_valley[position-96])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  temp_spatially_120 = round(meteo_data$air_temperature_mountain[position-120]-((meteo_data$air_temperature_mountain[position-120]-meteo_data$air_temperature_valley[position-120])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_day = raster_base+meteo_data$air_temperature_day[position]
  # 
  # temp_spatially_week = raster_base+meteo_data$air_temperature_week[position]
  # 
  # temp_spatially_month = raster_base+meteo_data$air_temperature_month[position]
  
  
  texture_spatially = static_raster$soil_texture
  # type_spatially = static_raster$soil_type
  elevation_spatially = static_raster$elevation
  landuse_spatially = static_raster$land_use
  # inclination_spatially = static_raster$inclination
  # northness_spatially = static_raster$northness
  eastness_spatially = static_raster$eastness
  wettness_spatially = static_raster$topo_wetness
  
  
  
  #landuse_spatially[is.na(landuse_spatially[])] <- 0
  #texture_spatially[is.na(texture_spatially[])] <- 0
  #type_spatially[is.na(type_spatially[])] <- 0
  
  
  #"soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness"
  
  #texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially
  
  
  ##################################
  
  
  radolan_sum_week = stack(paste0("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area_168h_sum/radolan_168h_sum_",time,".tif"))
  

  ##################################
  
  
  radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
  radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)
  
  #date_day_before = strftime(meteo_data$datetime[i-24], "%Y%m%d")
  date_day_before = strftime(meteo_data$datetime[i-24], "%Y%m%d")
  date_day_before
  
  radolan_files_selected =  radolan_files[grepl(date,radolan_files)]
  radolan_raster = stack(radolan_files_selected)
  
  radolan_files_selected_day_before =  radolan_files[grepl(date_day_before,radolan_files)]
  radolan_raster_day_before = stack(radolan_files_selected_day_before)
  
  #plot(radolan_raster)

  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  time
  time_day_before = strftime(meteo_data$datetime[i-24], "%Y%m%d%H%M")
  time_day_before
  
  hour = strftime(meteo_data$datetime[i], "%H")
  hour
  
  if(hour < 18){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):(as.numeric(hour)+6)]])
  }
  
  if(hour == 19){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],
                            radolan_raster[[1:1]])
  }
  
  if(hour == 20){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],
                            radolan_raster[[1:2]])
  }
  
  if(hour == 21){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],
                            radolan_raster[[1:3]])
  }
  
  if(hour == 22){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],
                            radolan_raster[[1:4]])
  }
  
  if(hour == 23){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],
                            radolan_raster[[1:5]])
  }
  
  
  ##################################
  
  
  
  
  
  
  
  depths_spatially = raster_base + depths
  
  depths_05 = raster_base + 5
  depths_15 = raster_base + 15
  depths_25 = raster_base + 25
  depths_35 = raster_base + 35
  depths_45 = raster_base + 45
  depths_55 = raster_base + 55
  depths_65 = raster_base + 65
  depths_75 = raster_base + 75
  
  ##############################
  
  
  
  
  
  
  predictors_05 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_05, temp_spatially_3,
                        temp_spatially_12,temp_spatially,humidity_spatially)
  
  names(predictors_05) <- ffsmodel$selectedvars
  
  predictors_05 <- as(predictors_05,"SpatRaster")
  
  levels(predictors_05$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  # levels(predictors_05$soil_type) <- data.frame(ID=c(1,2,3,4), 
  #                                               soil_type=c("1","2","3","4"))
  # levels(predictors_05$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
  #                                                  soil_texture=c("10","30","40","50","60"))
  
  prediction_05 <- predict(predictors_05,ffsmodel,na.rm=TRUE)
  
  plot(prediction_05)
  
  #mapview::mapview(raster(prediction_05))
  
  
}


AOA <- aoa(predictors_05, ffsmodel, trainDI = DI)




predictors_15 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_15, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_15) <- ffsmodel$selectedvars

predictors_15 <- as(predictors_15,"SpatRaster")

levels(predictors_15$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_15 <- predict(predictors_15,ffsmodel,na.rm=TRUE)



predictors_25 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_25, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_25) <- ffsmodel$selectedvars

predictors_25 <- as(predictors_25,"SpatRaster")

levels(predictors_25$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_25 <- predict(predictors_25,ffsmodel,na.rm=TRUE)



predictors_35 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_35, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_35) <- ffsmodel$selectedvars

predictors_35 <- as(predictors_35,"SpatRaster")

levels(predictors_35$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_35 <- predict(predictors_35,ffsmodel,na.rm=TRUE)



predictors_45 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_45, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_45) <- ffsmodel$selectedvars

predictors_45 <- as(predictors_45,"SpatRaster")

levels(predictors_45$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_45 <- predict(predictors_45,ffsmodel,na.rm=TRUE)



predictors_55 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_55, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_55) <- ffsmodel$selectedvars

predictors_55 <- as(predictors_55,"SpatRaster")

levels(predictors_55$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_55 <- predict(predictors_55,ffsmodel,na.rm=TRUE)



predictors_65 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_65, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_65) <- ffsmodel$selectedvars

predictors_65 <- as(predictors_65,"SpatRaster")

levels(predictors_65$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_65 <- predict(predictors_65,ffsmodel,na.rm=TRUE)



predictors_75 = stack(trend_3_month_spatially,prec_sum_4_month_spatially,landuse_spatially, depths_75, temp_spatially_3,
                      temp_spatially_12,temp_spatially,humidity_spatially)

names(predictors_75) <- ffsmodel$selectedvars

predictors_75 <- as(predictors_75,"SpatRaster")

levels(predictors_75$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))

prediction_75 <- predict(predictors_75,ffsmodel,na.rm=TRUE)




#pred_st = stack(prediction_05,prediction_15,prediction_25,prediction_35,prediction_45,prediction_55,
#    prediction_65,prediction_75,AOA)

# names(pred_st) = c(paste0("05_",time),
#                    paste0("15_",time),
#                    paste0("25_",time),
#                    paste0("35_",time),
#                    paste0("45_",time),
#                    paste0("55_",time),
#                    paste0("65_",time),
#                    paste0("75_",time),
#                    "AOA")
# 
#writeRaster(pred_st,paste0("pred_st_",time,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')





##################################################
##################################################


cols=viridisLite::viridis(40)

b <- seq(from=0,to=20,by=0.5)

par(mfrow = c(8, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
plot(prediction_05, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_15, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_25, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_35, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_45, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_55, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_65, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_75, col = cols, breaks = b, axes=F,legend=F)
#col = topo.colors(100)

#spplot(prediction,at = seq(10,20,0.2))
#plot(prediction)
#mapview(prediction)

##############################

#plot(static_raster$inclination, col = cols, breaks = b, axes=F,legend=T)





















#######################################################################################################
#######################################################################################################


library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)


#############################
#############################
#############################

#test model



library("CAST")
library("caret")

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons_radolan_4.Rdata")
#predictor_set_complete_seasons_radolan.Rdata
#predictor_set_complete_seasons.Rdata

data.table::uniqueN(predictor_set_2$date)

predictor_set_2$day <- lubridate::day(predictor_set_2$date_hour)
predictor_set_2$week <- lubridate::week(predictor_set_2$date_hour)
predictor_set_2$month <- lubridate::month(predictor_set_2$date_hour)
head(predictor_set_2)

predictor_set_temperature = predictor_set_2[ , -which(names(predictor_set_2) %in% c("T_org","T_05","T_15","T_25","T_35","T_45","T_55",
                                                                                    "T_65","T_75","T_85","T_95","T_105","T_115",
                                                                                    "measurement","M_95","M_105","M_115","day","week","month"))]

names(predictor_set_temperature)

#predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[43:44],predictor_set_temperature[13:42],predictor_set_temperature[4:12])
predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[61:62],predictor_set_temperature[13:60],predictor_set_temperature[4:12])


names(predictor_set_temperature_2)


########################

dt <- subset(predictor_set_temperature_2,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))


head(dt)

dt$soil_texture = as.factor(dt$soil_texture)
dt$soil_type = as.factor(dt$soil_type)
dt$land_use = as.factor(dt$land_use)

data.table::uniqueN(dt$plot_id)


spacetimefolds <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date", k=4)

#plot(spacetimefolds$index[[1]][1:10000],spacetimefolds$index[[1]][1:10000])
#points(spacetimefolds$indexOut[[1]],spacetimefolds$indexOut[[1]],col="red")

#plot(spacetimefolds$index[[2]][1:10000],spacetimefolds$index[[2]][1:10000])
#points(spacetimefolds$indexOut[[2]],spacetimefolds$indexOut[[2]],col="red")

#create space time folds

train = dt[spacetimefolds$index[[1]],]
test = dt[spacetimefolds$indexOut[[1]],]

###############################################

#random sample

train_sub =  train[sample(1:nrow(train), 50000), ]  #50000

##############################################

#train set

head(train_sub)

train_melt <- reshape2::melt(train_sub, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                               "relative_humidity","air_pressure","wind_speed",
                                               "trend_week","trend_month", "trend_3month",
                                               "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                               "radolan_sum_48_72","radolan_sum_week",
                                               "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                               "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                               "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                               "air_temperature_day","air_temperature_week","air_temperature_month",
                                               "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                               "date", "plot_id"
)) 

train_melt$depths = substr(train_melt$variable, 3,5)

train_melt$soil_moisture=train_melt$value

train_set = train_melt[ , -which(names(train_melt) %in% c("value","variable"))]


train_set=train_set[complete.cases(train_set), ]

train_set$depths = as.numeric(train_set$depths)

#unique(train_set$date)
#unique(train_set$probe_name)
################################################
#train_set <- subset(train_set,depths == 55)



#################################################
head(test)

test_melt <- reshape2::melt(test, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                         "relative_humidity","air_pressure","wind_speed",
                                         "trend_week","trend_month", "trend_3month",
                                         "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                                         "radolan_sum_48_72","radolan_sum_week",
                                         "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                                         "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                         "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                         "air_temperature_day","air_temperature_week","air_temperature_month",
                                         "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                         "date", "plot_id"
)) 

test_melt$depths = substr(test_melt$variable, 3,5)

test_melt$soil_moisture=test_melt$value

test_set = test_melt[ , -which(names(test_melt) %in% c("value","variable"))]


test_set=test_set[complete.cases(test_set), ]

test_set$depths = as.numeric(test_set$depths)


#################################################

#cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id",timevar ="date", k=5)#
cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id", k=5)#



hyperparameter = expand.grid(mtry = 2,
                             min.node.size = 5,
                             splitrule = "variance")

#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results/model_with_day_week_month")

#save(train_set,file = "train_set.Rdata")
#save(test_set,file = "test_set.Rdata")
#save(cv_spacetimefolds,file = "cv_spacetimefolds.Rdata")


predictors <- c("air_temperature_mountain","precipitation","global_radiation","relative_humidity","air_pressure","wind_speed",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "air_temperature_day","air_temperature_week","air_temperature_month",
                "trend_week","trend_month", "trend_3month",
                "radolan", "radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","radolan_sum_24_48",
                "radolan_sum_48_72","radolan_sum_week",
                "prec_sum_1_month","prec_sum_2_month","prec_sum_3_month", "prec_sum_4_month","prec_sum_5_month", "prec_sum_6_month",
                "depths",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness",
                "ndvi", "ndwi", "radar")

#                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
#                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
#                "air_temperature_day","air_temperature_week","air_temperature_month",
#                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")



# predictors <- c("air_temperature_month","radolan_sum_week","prec_sum_3_month",
#                 "depths","elevation","land_use","inclination","northness","eastness","topo_wetness")
#  



response <- "soil_moisture" 

Sys.time()
rfmodel = caret::train(x =  train_set[,predictors],
                       y =   train_set[,response],
                       method = "ranger",
                       tuneGrid = hyperparameter,
                       num.trees = 100,
                       trControl = trainControl(method = "cv", number = 5,
                                                index = cv_spacetimefolds$index, 
                                                indexOut = cv_spacetimefolds$indexOut,
                                                savePredictions = "final"),
                       importance = "permutation")
Sys.time()

rfmodel
varImp(rfmodel,conditional=TRUE)
plot(varImp(rfmodel))


#############################
#############################
#############################


#prediction_test = predict(test_set,rfmodel)


#rf
plot(rfmodel$pred$obs,rfmodel$pred$pred,ylim=c(0,40),xlim=c(0,40),pch=20,col= rgb(red = 0, green = 0, blue = 1, alpha = 0.01))
abline(coef = c(0,1))

# hex <- hexbin::hexbin(rfmodel$pred$obs,rfmodel$pred$pred,xbins =50)
# plot(hex, legend = FALSE)
# 
# hexbin::hexbinplot(rfmodel$pred$obs~rfmodel$pred$pred, xlim=c(0, 40),ylim=c(0, 40)) ## with limits

###########################################
#test set

test_set[,"prediction"]=round(predict.train(object=rfmodel, newdata = test_set,na.action = na.omit),
                              digits = 2)
head(test_set)

test_set$difference = abs(test_set$soil_moisture-test_set$prediction)

mean(test_set$difference)

plot(test_set$soil_moisture,test_set$prediction,ylim=c(0,40),xlim=c(0,40),pch=20,col= rgb(red = 0, green = 0, blue = 1, alpha = 0.02))
abline(coef = c(0,1))


##################################################



head(test_set)

test_daily = data.frame(test_set$date,test_set$probe_name,test_set$soil_moisture)

names(test_daily) = c("date","probe_name", "soil_moisture")
# 
# 
# aggregate(test_daily$soil_moisture, by=list(Category=x$Category), FUN=sum)
# 
# 
# 
# library(dplyr)
# daily_sum = test_daily %>%
#   group_by(date, probe_name) %>% 
#   summarise_each(funs(mean))
# 
# 
# plot(daily_sum$soil_moisture,daily_sum$prediction,ylim=c(0,40),xlim=c(0,40))
# abline(coef = c(0,1))
# 

##################################################
###################################################

























##prediction preparation

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

static_raster= stack("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

elevation_mountain = 765 #Pflanzgarten 765 Weidebrunnen 775
elevation_valley = 620  #Voitsumra

elevation = static_raster$elevation

raster_base = elevation*0

meteo_data$air_temperature_day = zoo::rollmean(meteo_data$air_temperature_mountain, k = 24, fill = NA)#,align ="right"
meteo_data$air_temperature_week = zoo::rollmean(meteo_data$air_temperature_mountain, k = 168, fill = NA)
meteo_data$air_temperature_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 720, fill = NA)

############################

#trends

Dataexample = data.frame(c(1:18240))
names(Dataexample)= "X"

Dataexample$Y <- as.numeric(meteo_data$air_temperature_mountain)

#https://stackoverflow.com/questions/71312435/finding-the-slope-of-a-linear-trend-line-in-a-moving-window-in-r

trend_week = rollapply(Dataexample, 168, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)
trend_month = rollapply(Dataexample, 720, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)
trend_3month = rollapply(Dataexample, 2160, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)

trend_week = data.frame(trend_week)
trend_month = data.frame(trend_month)
trend_3month = data.frame(trend_3month)

trends = cbind(meteo_data[1],trend_week$X,trend_month$X,trend_3month$X)

names(trends) = c("datetime","trend_week","trend_month","trend_3month")

#trends <- subset(trends,
#             datetime >= as.POSIXct('2022-01-01 00:00') &
#               datetime <= as.POSIXct('2022-12-31 23:59'))


#!!!! METEO MUSS NOCH AUF SUBSET ZUGESCHNITTEN WERDEN 

############################
#precipitation


#prediction

#position = 5485 #"2022-01-15 12:00:00 CET"
#i=20220115
#position = 6229 #"2022-02-15 12:00:00 CET"
#i=20220215
#position = 6901 #"2022-03-15 12:00:00 CET"
#i=20220315
#position = 7645 #"2022-04-15 12:00:00 CEST"
#i=20220415
position = 8365 #"2022-05-15 12:00:00 CEST"
i=20220515
#position = 9109 #"2022-06-15 12:00:00 CEST"
#i=20220615
#position = 9829 #"2022-07-15 12:00:00 CEST"
#i=20220715
#position = 10573 #"2022-08-15 12:00:00 CEST"
#i=20220815
#position = 11317 #"2022-09-15 12:00:00 CEST"
#i=20220915
#position = 12037 #"2022-10-15 12:00:00 CEST"
#i=20221015
#position = 12781 #"2022-11-15 12:00:00 CET"
#i=20221115
#position = 13501 #"2022-12-15 12:00:00 CET"
#i=20221215

# ############################
# #precipitation
# 
# 
# radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
# radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)
# 
# 
# #radolan_date_time = substr(radolan_files, 116,126) 
# 
# radolan_date = as.numeric(substr(radolan_files, 117,124)) 
# 
# radolan_date_posix = as.POSIXct(strptime(radolan_date,"%Y%m%d"))
# 
# 
# #nur für 12 uhr
# 
# #i=20220215
# 
# daybefore1 = i-1
# daybefore2 = i-2
# daybefore3 = i-3
# daybefore4 = i-4
# daybefore5 = i-5
# 
# files_day<- radolan_files[grepl(i,radolan_files)]
# files_daybefore1<- radolan_files[grepl(daybefore1,radolan_files)]
# files_daybefore2<- radolan_files[grepl(daybefore2,radolan_files)]
# files_daybefore3<- radolan_files[grepl(daybefore3,radolan_files)]
# files_daybefore4<- radolan_files[grepl(daybefore4,radolan_files)]
# files_daybefore5<- radolan_files[grepl(daybefore5,radolan_files)]
# 
# radolan_day = stack(files_day)
# radolan_daybefore1 = stack(files_daybefore1)
# radolan_daybefore2 = stack(files_daybefore2)
# radolan_daybefore3 = stack(files_daybefore3)
# radolan_daybefore4 = stack(files_daybefore4)
# radolan_daybefore5 = stack(files_daybefore5)
# 
# 
# #daily_sum = sum(radolan_day/100)
# #plot(radolan_day/100)
# #plot(daily_sum)
# 
# radolan_now_spatially = radolan_day[[12]]/100
# radolan_3h_spatially = sum(radolan_day[[10:12]]/100)
# radolan_6h_spatially = sum(radolan_day[[7:12]]/100)
# radolan_12h_spatially = sum(radolan_day[[1:12]]/100)
# radolan_24h_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[12:24]]/100)
# radolan_2d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[12:24]]/100)
# radolan_3d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,radolan_daybefore3[[12:24]]/100)
# radolan_4d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
#                            radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[12:24]]/100)
# radolan_5d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
#                            radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[1:24]]/100,radolan_daybefore5[[12:24]]/100)
# 
# 
# 
# #############################
# #############################



############################
#precipitation


radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)


#radolan_date_time = substr(radolan_files, 116,126) 

radolan_date = as.numeric(substr(radolan_files, 117,124)) 

radolan_date_posix = as.POSIXct(strptime(radolan_date,"%Y%m%d"))


#nur für 12 uhr

#i=20220215

daybefore1 = i-1
daybefore2 = i-2
daybefore3 = i-3
daybefore4 = i-4
daybefore5 = i-5

files_day<- radolan_files[grepl(i,radolan_files)]
files_daybefore1<- radolan_files[grepl(daybefore1,radolan_files)]
files_daybefore2<- radolan_files[grepl(daybefore2,radolan_files)]
files_daybefore3<- radolan_files[grepl(daybefore3,radolan_files)]
files_daybefore4<- radolan_files[grepl(daybefore4,radolan_files)]
files_daybefore5<- radolan_files[grepl(daybefore5,radolan_files)]

radolan_day = stack(files_day)
radolan_daybefore1 = stack(files_daybefore1)
radolan_daybefore2 = stack(files_daybefore2)
radolan_daybefore3 = stack(files_daybefore3)
radolan_daybefore4 = stack(files_daybefore4)
radolan_daybefore5 = stack(files_daybefore5)


#daily_sum = sum(radolan_day/100)
#plot(radolan_day/100)
#plot(daily_sum)

radolan_now_spatially = radolan_day[[12]]/100
radolan_3h_spatially = sum(radolan_day[[10:12]]/100)
radolan_6h_spatially = sum(radolan_day[[7:12]]/100)
radolan_12h_spatially = sum(radolan_day[[1:12]]/100)
radolan_24h_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[12:24]]/100)
radolan_2d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[12:24]]/100)
radolan_3d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,radolan_daybefore3[[12:24]]/100)
radolan_4d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
                           radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[12:24]]/100)
radolan_5d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
                           radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[1:24]]/100,radolan_daybefore5[[12:24]]/100)



#############################
#############################


meteo_data$datetime[position]

depths = 5



temp_spatially = round(meteo_data$air_temperature_mountain[position]-((meteo_data$air_temperature_mountain[position]-meteo_data$air_temperature_valley[position])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

radiation_spatially = raster_base+meteo_data$global_radiation[position]

humidity_spatially = raster_base+meteo_data$relative_humidity[position]

pressure_spatially = raster_base+meteo_data$air_pressure[position]

wind_spatially = raster_base+meteo_data$wind_speed[position]

precipitation_spatially = raster_base+meteo_data$precipitation[position]

temp_spatially_3 = round(meteo_data$air_temperature_mountain[position-3]-((meteo_data$air_temperature_mountain[position-3]-meteo_data$air_temperature_valley[position-3])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_6 = round(meteo_data$air_temperature_mountain[position-6]-((meteo_data$air_temperature_mountain[position-6]-meteo_data$air_temperature_valley[position-6])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_12 = round(meteo_data$air_temperature_mountain[position-12]-((meteo_data$air_temperature_mountain[position-12]-meteo_data$air_temperature_valley[position-12])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_24 = round(meteo_data$air_temperature_mountain[position-24]-((meteo_data$air_temperature_mountain[position-24]-meteo_data$air_temperature_valley[position-24])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_48 = round(meteo_data$air_temperature_mountain[position-48]-((meteo_data$air_temperature_mountain[position-48]-meteo_data$air_temperature_valley[position-48])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_96 = round(meteo_data$air_temperature_mountain[position-96]-((meteo_data$air_temperature_mountain[position-96]-meteo_data$air_temperature_valley[position-96])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_120 = round(meteo_data$air_temperature_mountain[position-120]-((meteo_data$air_temperature_mountain[position-120]-meteo_data$air_temperature_valley[position-120])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)

temp_spatially_day = raster_base+meteo_data$air_temperature_day[position]

temp_spatially_week = raster_base+meteo_data$air_temperature_week[position]

temp_spatially_month = raster_base+meteo_data$air_temperature_month[position]

trend_week_spatially = raster_base+trends$trend_week[position]

trend_months_spatially = raster_base+trends$trend_month[position]

trend_3months_spatially = raster_base+trends$trend_3month[position]

plot(trend_week_spatially)



texture_spatially = static_raster$soil_texture
type_spatially = static_raster$soil_type
elevation_spatially = static_raster$elevation
landuse_spatially = static_raster$land_use
inclination_spatially = static_raster$inclination
northness_spatially = static_raster$northness
eastness_spatially = static_raster$eastness
wettness_spatially = static_raster$topo_wetness



#landuse_spatially[is.na(landuse_spatially[])] <- 0
#texture_spatially[is.na(texture_spatially[])] <- 0
#type_spatially[is.na(type_spatially[])] <- 0


#"soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness"

#texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially



depths_spatially = raster_base + depths

depths_05 = raster_base + 5
depths_15 = raster_base + 15
depths_25 = raster_base + 25
depths_35 = raster_base + 35
depths_45 = raster_base + 45
depths_55 = raster_base + 55
depths_65 = raster_base + 65
depths_75 = raster_base + 75

##############################



predictors_spatially = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                             temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                             temp_spatially_day, temp_spatially_week, temp_spatially_month,
                             trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                             radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                             radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                             depths_spatially,
                             texture_spatially, type_spatially, elevation_spatially, landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_spatially) <- predictors


predictors_spatially <- as(predictors_spatially,"SpatRaster")

levels(predictors_spatially$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                                    land_use=c("1000","1500","1600"))
levels(predictors_spatially$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                     soil_type=c("1","2","3","4"))
levels(predictors_spatially$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                        soil_texture=c("10","30","40","50","60"))

prediction <- predict(predictors_spatially,rfmodel,na.rm=TRUE)

plot(prediction)

################################













predictors_05 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_05,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_05) <- predictors

predictors_05 <- as(predictors_05,"SpatRaster")

levels(predictors_05$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_05$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_05$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_05 <- predict(predictors_05,rfmodel,na.rm=TRUE)





predictors_15 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_15,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_15) <- predictors

predictors_15 <- as(predictors_15,"SpatRaster")

levels(predictors_15$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_15$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_15$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_15 <- predict(predictors_15,rfmodel,na.rm=TRUE)




predictors_25 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_25,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_25) <- predictors

predictors_25 <- as(predictors_25,"SpatRaster")

levels(predictors_25$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_25$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_25$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_25 <- predict(predictors_25,rfmodel,na.rm=TRUE)




predictors_35 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_35,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_35) <- predictors

predictors_35 <- as(predictors_35,"SpatRaster")

levels(predictors_35$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_35$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_35$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_35 <- predict(predictors_35,rfmodel,na.rm=TRUE)




predictors_45 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_45,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_45) <- predictors

predictors_45 <- as(predictors_45,"SpatRaster")

levels(predictors_45$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_45$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_45$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_45 <- predict(predictors_45,rfmodel,na.rm=TRUE)






predictors_55 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_55,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_55) <- predictors

predictors_55 <- as(predictors_55,"SpatRaster")

levels(predictors_55$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_55$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_55$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_55 <- predict(predictors_55,rfmodel,na.rm=TRUE)




predictors_65 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_65,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_65) <- predictors

predictors_65 <- as(predictors_65,"SpatRaster")

levels(predictors_65$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_65$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_65$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_65 <- predict(predictors_65,rfmodel,na.rm=TRUE)





predictors_75 = stack(temp_spatially,precipitation_spatially,radiation_spatially,humidity_spatially,pressure_spatially,wind_spatially,
                      temp_spatially_3,temp_spatially_6,temp_spatially_12,temp_spatially_24,temp_spatially_48,temp_spatially_72,temp_spatially_96,temp_spatially_120,
                      temp_spatially_day, temp_spatially_week, temp_spatially_month,
                      trend_week_spatially,trend_months_spatially,trend_3months_spatially,
                      radolan_now_spatially,radolan_3h_spatially,radolan_6h_spatially,radolan_12h_spatially,radolan_24h_spatially,radolan_2d_spatially,
                      radolan_3d_spatially,radolan_4d_spatially,radolan_5d_spatially,
                      depths_75,
                      texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially)

names(predictors_75) <- predictors

predictors_75 <- as(predictors_75,"SpatRaster")

levels(predictors_75$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                             land_use=c("1000","1500","1600"))
levels(predictors_75$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                              soil_type=c("1","2","3","4"))
levels(predictors_75$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                 soil_texture=c("10","30","40","50","60"))

prediction_75 <- predict(predictors_75,rfmodel,na.rm=TRUE)



#cols=viridisLite::viridis(30,direction = -1)
#b <- seq(from=5,to=35,by=1)

cols=viridisLite::viridis(40,direction = -1)
b <- seq(from=0,to=40,by=1)

#c <- seq(from=10,to=30,by=0.5)
#legend
#plot(prediction_05, col = cols, breaks = c, axes=F,legend=T)




par(mfrow = c(8, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
plot(prediction_05, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_15, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_25, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_35, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_45, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_55, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_65, col = cols, breaks = b, axes=F,legend=F)
plot(prediction_75, col = cols, breaks = b, axes=F,legend=F)
#col = topo.colors(100)

#spplot(prediction,at = seq(10,20,0.2))
#plot(prediction)
#mapview(prediction)

##############################

#plot(static_raster$inclination, col = cols, breaks = b, axes=F,legend=T)

