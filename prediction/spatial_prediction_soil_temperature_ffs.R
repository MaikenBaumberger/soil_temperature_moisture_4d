library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)


#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d/soil_temperature_moisture_4d/model_results/compare_train_ffs")
setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v2")

#load("test_set.Rdata")
#load("train_set.Rdata")
#load("cv_spacetimefolds.Rdata")
load("ffs_model.Rdata")

ffsmodel
ffsmodel$selectedvars


##################################

#DI = trainDI(model=ffsmodel)

#AOA <- aoa(predictors_spatially, ffsmodel, trainDI = DI)

#save(DI,file = "DI.Rdata")

#save(AOA,file = "AOA_june.Rdata")

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

meteo_data$prec_sum_3_month = zoo::rollsum(meteo_data$precipitation, 168*12, align = "right", fill=NA)

meteo_data$prec_sum_4_month = zoo::rollsum(meteo_data$precipitation, 168*16, align = "right", fill=NA)

meteo_data$prec_sum_5_month = zoo::rollsum(meteo_data$precipitation, 168*20, align = "right", fill=NA)



#############################
#############################

#prediction


# meteo_data$datetime[5137]
# meteo_data$datetime[13873]


#plot(temp_spatially_12)


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
#position = 13501 #"2022-12-15 12:00:00 CET"



random_sample = sample(x = 5137:13873, size  = 200)



for (i in random_sample){

  print(i)
  position = i
  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  print(time)
  
  
  
  meteo_data$datetime[position]
  
  #depths = 5
  
  prec_sum_4_month_spatially = raster_base + meteo_data$prec_sum_4_month[position]
  
  trend_3_month_spatially = raster_base + trends$trend_3month[position]
  
  temp_spatially = round(meteo_data$air_temperature_mountain[position]-((meteo_data$air_temperature_mountain[position]-meteo_data$air_temperature_valley[position])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # radiation_spatially = raster_base+meteo_data$global_radiation[position]
  
  humidity_spatially = raster_base+meteo_data$relative_humidity[position]
  
  # pressure_spatially = raster_base+meteo_data$air_pressure[position]
  
  # wind_spatially = raster_base+meteo_data$wind_speed[position]
  
  # precipitation_spatially = raster_base+meteo_data$precipitation[position]
  
  temp_spatially_3 = round(meteo_data$air_temperature_mountain[position-3]-((meteo_data$air_temperature_mountain[position-3]-meteo_data$air_temperature_valley[position-3])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_6 = round(meteo_data$air_temperature_mountain[position-6]-((meteo_data$air_temperature_mountain[position-6]-meteo_data$air_temperature_valley[position-6])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  temp_spatially_12 = round(meteo_data$air_temperature_mountain[position-12]-((meteo_data$air_temperature_mountain[position-12]-meteo_data$air_temperature_valley[position-12])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_24 = round(meteo_data$air_temperature_mountain[position-24]-((meteo_data$air_temperature_mountain[position-24]-meteo_data$air_temperature_valley[position-24])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_48 = round(meteo_data$air_temperature_mountain[position-48]-((meteo_data$air_temperature_mountain[position-48]-meteo_data$air_temperature_valley[position-48])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_96 = round(meteo_data$air_temperature_mountain[position-96]-((meteo_data$air_temperature_mountain[position-96]-meteo_data$air_temperature_valley[position-96])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_120 = round(meteo_data$air_temperature_mountain[position-120]-((meteo_data$air_temperature_mountain[position-120]-meteo_data$air_temperature_valley[position-120])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_day = raster_base+meteo_data$air_temperature_day[position]
  # 
  # temp_spatially_week = raster_base+meteo_data$air_temperature_week[position]
  # 
  # temp_spatially_month = raster_base+meteo_data$air_temperature_month[position]
  
  
  # texture_spatially = static_raster$soil_texture
  # type_spatially = static_raster$soil_type
  # elevation_spatially = static_raster$elevation
  landuse_spatially = static_raster$land_use
  # inclination_spatially = static_raster$inclination
  # northness_spatially = static_raster$northness
  # eastness_spatially = static_raster$eastness
  # wettness_spatially = static_raster$topo_wetness
  
  
  
  #landuse_spatially[is.na(landuse_spatially[])] <- 0
  #texture_spatially[is.na(texture_spatially[])] <- 0
  #type_spatially[is.na(type_spatially[])] <- 0
  
  
  #"soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness"
  
  #texture_spatially, type_spatially, elevation_spatially,landuse_spatially, inclination_spatially, northness_spatially, eastness_spatially, wettness_spatially
  
  
  
  #depths_spatially = raster_base + depths
  
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
  


  #AOA <- aoa(predictors_05, ffsmodel, trainDI = DI)
  

  
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
  

  
  
  pred_st = c(prediction_05,prediction_15,prediction_25,prediction_35,prediction_45,prediction_55,
     prediction_65,prediction_75)

  names(pred_st) = c(paste0(time,"_05"),
                     paste0(time,"_15"),
                     paste0(time,"_25"),
                     paste0(time,"_35"),
                     paste0(time,"_45"),
                     paste0(time,"_55"),
                     paste0(time,"_65"),
                     paste0(time,"_75"))
  
  
  setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_temperature_prediction_v2")
  writeRaster(pred_st,paste0("pred_st_",time,".tif"),overwrite=TRUE,datatype='INT2U')
  

}

# plot(prediction_05)


##################################################
##################################################

# 
# cols=viridisLite::viridis(40)
# 
# b <- seq(from=0,to=20,by=0.5)
# 
# par(mfrow = c(8, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
# plot(prediction_05, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_15, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_25, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_35, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_45, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_55, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_65, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_75, col = cols, breaks = b, axes=F,legend=F)
# 
