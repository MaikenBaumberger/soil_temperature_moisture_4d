library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)
library(terra)


#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d/soil_temperature_moisture_4d/model_results/compare_train_ffs")
#setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v2")
setwd("C:/Users/maike/Desktop/Carbon4D/Palma/hyperparameter_tuning_2")

#load("test_set.Rdata")
#load("train_set.Rdata")
#load("cv_spacetimefolds.Rdata")
#load("ffs_model.Rdata")
load("rfmodel_sm.Rdata")

#ffsmodel
#ffsmodel$selectedvars


ffsmodel = rfmodel

plot(varImp(rfmodel))

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
meteo_data$air_temperature_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 720, fill = NA)
meteo_data$air_temperature_3_month = zoo::rollmean(meteo_data$air_temperature_mountain, k = 2160, fill = NA)

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/trends.RData")
head(trends)

#precipitaion

meteo_data$prec_sum_3_month = zoo::rollsum(meteo_data$precipitation, 168*12, align = "right", fill=NA)

meteo_data$prec_sum_4_month = zoo::rollsum(meteo_data$precipitation, 168*16, align = "right", fill=NA)

meteo_data$prec_sum_5_month = zoo::rollsum(meteo_data$precipitation, 168*20, align = "right", fill=NA)




ndvi = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndvi_daily_filled.tif")



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



random_sample = sample(x = 5137:13873, size  = 1)

mid_month = c(5485,6229,6901,7645,8365,9109,9829,10573,11317,12037,12781,13501)

for (i in mid_month){

  #i=9000
  print(i)
  position = i
  time = strftime(meteo_data$datetime[i], "%Y%m%d%H%M")
  print(time)
  doi = as.numeric(strftime(as.POSIXct(strftime(meteo_data$datetime[i], "%Y-%m-%d %H:%M:%S")), format = "%j"))
  print(doi)
  hour = as.numeric(strftime(as.POSIXct(strftime(meteo_data$datetime[i], "%Y-%m-%d %H:%M:%S")), format = "%H"))
  print(hour)
  
  ndvi_day_befor = ndvi[[doi-1]]
  ndvi_day = ndvi[[doi]]
  ndvi_day_after = ndvi[[doi+1]]
  
  empty_raster = ndvi[[1]]*NA
  gap <- rast(lapply(1:23, function(i) empty_raster))
  
  ndvi_gap = c(ndvi_day_befor,gap,ndvi_day,gap,ndvi_day_after)
  
  ndvi_49 = approximate(ndvi_gap, rule=2)
  
  ndvi_spatially = stack(ndvi_49[[13+hour]])
  
  ndvi_spatially = resample(ndvi_spatially,elevation)
  
  meteo_data$datetime[position]
  
  plot(ndvi_spatially)
  
  #depths = 5
  
  prec_sum_3_month_spatially = raster_base + meteo_data$prec_sum_3_month[position]
  
  #trend_3_month_spatially = raster_base + trends$trend_3month[position]
  
  #trend_month_spatially = raster_base + trends$trend_month[position]
  
  #temp_spatially = round(meteo_data$air_temperature_mountain[position]-((meteo_data$air_temperature_mountain[position]-meteo_data$air_temperature_valley[position])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  radiation_spatially = raster_base+meteo_data$global_radiation[position]
  
  #humidity_spatially = raster_base+meteo_data$relative_humidity[position]
  
  # pressure_spatially = raster_base+meteo_data$air_pressure[position]
  
  # wind_spatially = raster_base+meteo_data$wind_speed[position]
  
  precipitation_spatially = raster_base+meteo_data$precipitation[position]
  
  #temp_spatially_3 = round(meteo_data$air_temperature_mountain[position-3]-((meteo_data$air_temperature_mountain[position-3]-meteo_data$air_temperature_valley[position-3])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  #temp_spatially_6 = round(meteo_data$air_temperature_mountain[position-6]-((meteo_data$air_temperature_mountain[position-6]-meteo_data$air_temperature_valley[position-6])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  #temp_spatially_12 = round(meteo_data$air_temperature_mountain[position-12]-((meteo_data$air_temperature_mountain[position-12]-meteo_data$air_temperature_valley[position-12])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  
  # temp_spatially_24 = round(meteo_data$air_temperature_mountain[position-24]-((meteo_data$air_temperature_mountain[position-24]-meteo_data$air_temperature_valley[position-24])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_48 = round(meteo_data$air_temperature_mountain[position-48]-((meteo_data$air_temperature_mountain[position-48]-meteo_data$air_temperature_valley[position-48])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  temp_spatially_72 = round(meteo_data$air_temperature_mountain[position-72]-((meteo_data$air_temperature_mountain[position-72]-meteo_data$air_temperature_valley[position-72])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_96 = round(meteo_data$air_temperature_mountain[position-96]-((meteo_data$air_temperature_mountain[position-96]-meteo_data$air_temperature_valley[position-96])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_120 = round(meteo_data$air_temperature_mountain[position-120]-((meteo_data$air_temperature_mountain[position-120]-meteo_data$air_temperature_valley[position-120])/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation),2)
  # 
  # temp_spatially_day = raster_base+meteo_data$air_temperature_day[position]
  # 
  # temp_spatially_week = raster_base+meteo_data$air_temperature_week[position]
  # 
  #temp_spatially_month = raster_base+meteo_data$air_temperature_month[position]
  
  temp_spatially_3_month = raster_base+meteo_data$air_temperature_3_month[position]
  
  
  
  texture_spatially = static_raster$soil_texture
  type_spatially = static_raster$soil_type
  #elevation_spatially = static_raster$elevation
  landuse_spatially = static_raster$land_use
  inclination_spatially = static_raster$inclination
  # northness_spatially = static_raster$northness
  # eastness_spatially = static_raster$eastness
  wettness_spatially = static_raster$topo_wetness
  
  
  #landuse_spatially[is.na(landuse_spatially[])] <- 0
  #texture_spatially[is.na(texture_spatially[])] <- 0
  #type_spatially[is.na(type_spatially[])] <- 0
  

  
  radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
  radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)
  
  date = strftime(meteo_data$datetime[i], "%Y%m%d")
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
  
  hour = as.numeric(strftime(meteo_data$datetime[i], "%H"))
  hour
  

  radolan = radolan_raster[[as.numeric(hour)+1]]
  
  
  if(hour < 23){
  radolan_sum_0_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+2):24]],radolan_raster[[1:(as.numeric(hour)+1)]])
  }
  
  if(hour == 23){
    radolan_sum_0_24 = sum(radolan_raster[[1:24]])
  }
  

  
  if(hour >= 5){
    radolan_sum_0_6 = sum(radolan_raster[[(as.numeric(hour)-4):(as.numeric(hour)+1)]])
  }
  if(hour < 5){
    radolan_sum_0_6 = sum(radolan_raster_day_before[[(19+as.numeric(hour)):24]],radolan_raster[[1:(as.numeric(hour)+1)]])
  }
  
  
  
  
  if(hour < 18){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):(as.numeric(hour)+6)]])
  }
  
  if(hour >= 18 & hour < 23){
    radolan_sum_18_24 = sum(radolan_raster_day_before[[(as.numeric(hour)+1):24]],radolan_raster[[1:(as.numeric(hour)-17)]])
  }
  
  if(hour == 23){
    radolan_sum_18_24 = sum(radolan_raster[[1:6]])
  }
  
  
  
  if(hour < 12){
    radolan_sum_12_18 = sum(radolan_raster_day_before[[(as.numeric(hour)+6):(as.numeric(hour)+12)]])
  }
  
  if(hour >= 12 & hour < 18){
    radolan_sum_12_18 = sum(radolan_raster_day_before[[(as.numeric(hour)+6):24]],radolan_raster[[1:(as.numeric(hour)-11)]])
  }
  
  if(hour >= 18){
    radolan_sum_12_18 = sum(radolan_raster[[(as.numeric(hour)-17):(as.numeric(hour)-11)]])
  }
  
  
  
  


  
  
  #depths_spatially = raster_base + depths
  
  depths_05 = raster_base + 5
  depths_15 = raster_base + 15
  depths_25 = raster_base + 25
  depths_35 = raster_base + 35
  depths_45 = raster_base + 45
  depths_55 = raster_base + 55
  depths_65 = raster_base + 65
  depths_75 = raster_base + 75
  depths_85 = raster_base + 85
  
  ##############################
  
  
  predictors_05 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_05)
    
  names(predictors_05) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                           "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                           "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_05 <- as(predictors_05,"SpatRaster")
  
  levels(predictors_05$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_05$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_05$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                soil_texture=c("10","30","40","50","60"))
  
  prediction_05 <- predict(predictors_05,ffsmodel,na.rm=TRUE)
  
  plot(prediction_05)
  
  
 plot(varImp(ffsmodel)) 
  
####################################################################################################################
  #mapview::mapview(raster(prediction_05))
  


  #AOA <- aoa(predictors_05, ffsmodel, trainDI = DI)
  

  
  predictors_15 =stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                       radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                       texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_15)
  
  names(predictors_15) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_15 <- as(predictors_15,"SpatRaster")
  
  levels(predictors_15$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_15$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_15$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_15 <- predict(predictors_15,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_25 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_25)
  
  names(predictors_25) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_25 <- as(predictors_25,"SpatRaster")
  
  levels(predictors_25$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_25$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_25$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_25 <- predict(predictors_25,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_35 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_35)
  
  names(predictors_35) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_35 <- as(predictors_35,"SpatRaster")
  
  levels(predictors_35$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_35$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_35$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_35 <- predict(predictors_35,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_45 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_45)
  
  names(predictors_45) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_45 <- as(predictors_45,"SpatRaster")
  
  levels(predictors_45$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_45$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_45$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_45 <- predict(predictors_45,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_55 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_55)
  
  names(predictors_55) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_55 <- as(predictors_55,"SpatRaster")
  
  levels(predictors_55$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_55$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_55$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_55 <- predict(predictors_55,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_65 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_65)
  
  names(predictors_65) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_65 <- as(predictors_65,"SpatRaster")
  
  levels(predictors_65$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_65$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_65$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_65 <- predict(predictors_65,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_75 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_75)
  
  names(predictors_75) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_75 <- as(predictors_75,"SpatRaster")
  
  levels(predictors_75$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_75$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_75$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                   soil_texture=c("10","30","40","50","60"))
  
  prediction_75 <- predict(predictors_75,ffsmodel,na.rm=TRUE)
  
  
  
  predictors_85 = stack(temp_spatially_72,temp_spatially_3_month,precipitation_spatially,radiation_spatially,
                        radolan, radolan_sum_0_6, radolan_sum_12_18, radolan_sum_18_24 ,radolan_sum_0_24, prec_sum_3_month_spatially,
                        texture_spatially, type_spatially, landuse_spatially, inclination_spatially, wettness_spatially, ndvi_spatially,depths_85)
  
  names(predictors_85) <- c("air_temperature_mountain_72","air_temperature_3_month","precipitation","global_radiation",
                            "radolan", "radolan_sum_0_6","radolan_sum_12_18","radolan_sum_18_24","radolan_sum_0_24","prec_sum_3_month",
                            "soil_texture","soil_type","land_use","inclination","topo_wetness","ndvi","depths")
  
  predictors_85 <- as(predictors_85,"SpatRaster")
  
  levels(predictors_85$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  levels(predictors_85$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                                soil_type=c("1","2","3","4"))
  levels(predictors_85$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                    soil_texture=c("10","30","40","50","60"))
  
  prediction_85 <- predict(predictors_85,ffsmodel,na.rm=TRUE)
  
  
  
  
  pred_sm = c(prediction_05,prediction_15,prediction_25,prediction_35,prediction_45,prediction_55,
     prediction_65,prediction_75,prediction_85)

  names(pred_sm) = c(paste0(time,"_05"),
                     paste0(time,"_15"),
                     paste0(time,"_25"),
                     paste0(time,"_35"),
                     paste0(time,"_45"),
                     paste0(time,"_55"),
                     paste0(time,"_65"),
                     paste0(time,"_75"),
                     paste0(time,"_85"))
  
  
  setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/final_model_hyper/soil_moisture_12_ohne_AOA")
  writeRaster(pred_sm,paste0("pred_sm_",time,".tif"),overwrite=TRUE)
  



# plot(prediction_05)


##################################################
##################################################
# 
# 
# cols=viridisLite::viridis(40)
# 
# b <- seq(from=0,to=20,by=0.5)
# 
# par(mfrow = c(9, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
# plot(prediction_05, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_15, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_25, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_35, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_45, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_55, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_65, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_75, col = cols, breaks = b, axes=F,legend=F)
# plot(prediction_85, col = cols, breaks = b, axes=F,legend=F)
# 
# my_plot <- recordPlot()
# png(filename=paste0("pred_sm_",time,".png"), width = 1500, height = 9000)
# replayPlot(my_plot)
# dev.off()

}


