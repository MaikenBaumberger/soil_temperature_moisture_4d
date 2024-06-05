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

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons_radolan_4_Waldstein.Rdata")
#predictor_set_complete_seasons_radolan.Rdata
#predictor_set_complete_seasons.Rdata




categories <- unique(predictor_set_2$probe_name) 



data.table::uniqueN(predictor_set_2$date)

predictor_set_2$day <- lubridate::day(predictor_set_2$date_hour)
predictor_set_2$week <- lubridate::week(predictor_set_2$date_hour)
predictor_set_2$month <- lubridate::month(predictor_set_2$date_hour)
head(predictor_set_2)

predictor_set = predictor_set_2[ , -which(names(predictor_set_2) %in% c("M_org","T_org","M_95","M_105","M_115","T_95","T_105","T_115",
                                                                        "measurement","day","week","month"))]
names(predictor_set)

#predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[43:44],predictor_set_temperature[13:42],predictor_set_temperature[4:12])
#predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[61:62],predictor_set_temperature[13:60],predictor_set_temperature[4:12])
predictor_set = cbind(predictor_set[1:2],predictor_set[69:70],predictor_set[21:68],predictor_set[3:20])


names(predictor_set)


########################

dt <- subset(predictor_set,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))


head(dt)

dt$soil_texture = as.factor(dt$soil_texture)
dt$soil_type = as.factor(dt$soil_type)
dt$land_use = as.factor(dt$land_use)

data.table::uniqueN(dt$plot_id)


categories <- unique(dt$probe_name)





#spacetimefolds <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date", k=4)

#plot(spacetimefolds$index[[1]][1:10000],spacetimefolds$index[[1]][1:10000])
#points(spacetimefolds$indexOut[[1]],spacetimefolds$indexOut[[1]],col="red")

#plot(spacetimefolds$index[[2]][1:10000],spacetimefolds$index[[2]][1:10000])
#points(spacetimefolds$indexOut[[2]],spacetimefolds$indexOut[[2]],col="red")

#create space time folds

#train = dt[spacetimefolds$index[[1]],]
#test = dt[spacetimefolds$indexOut[[1]],]

###############################################

#random sample

#train_sub =  dt#train[sample(1:nrow(train), 50000), ]  #50000

##############################################

#train set

dt_st = dt[ , -which(names(dt) %in% c("M_05","M_15","M_25","M_35","M_45","M_55","M_65","M_75","M_85"))]

dt_sm = dt[ , -which(names(dt) %in% c("T_05","T_15","T_25","T_35","T_45","T_55","T_65","T_75","T_85"))]


head(dt_st)

###########################################



dt_st_melt <- reshape2::melt(dt_st, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
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

dt_st_melt$depths = substr(dt_st_melt$variable, 3,5)

dt_st_melt$soil_temperature=dt_st_melt$value

dt_st = dt_st_melt[ , -which(names(dt_st_melt) %in% c("value","variable"))]


dt_st=dt_st[complete.cases(dt_st), ]

dt_st$depths = as.numeric(dt_st$depths)


#########################################




dt_sm_melt <- reshape2::melt(dt_sm, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
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

dt_sm_melt$depths = substr(dt_sm_melt$variable, 3,5)

dt_sm_melt$soil_moisture=dt_sm_melt$value

dt_sm = dt_sm_melt[ , -which(names(dt_sm_melt) %in% c("value","variable"))]


dt_sm=dt_sm[complete.cases(dt_sm), ]

dt_sm$depths = as.numeric(dt_sm$depths)



#save(test_set,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/test_set_waldstein.Rdata")



#########################################

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

ffsmodel_st <- ffsmodel

setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_moisture_model_v1")

load("ffs_model.Rdata")

ffsmodel_sm <- ffsmodel

#############################################

#test set

dt_st[,"prediction"]=round(predict.train(object=ffsmodel_st, newdata = dt_st,na.action = na.omit),
                              digits = 2)
head(dt_st)

dt_st$difference = abs(dt_st$soil_temperature-dt_st$prediction)

mean(dt_st$difference)

# plot(dt_st$soil_temperature,dt_st$prediction,ylim=c(-1,34),xlim=c(-1,34),
#      pch=19,col=alpha("black",0.05),xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
# abline(coef = c(0,1))
# 

############################################


dt_sm[,"prediction"]=round(predict.train(object=ffsmodel_sm, newdata = dt_st,na.action = na.omit),
                           digits = 2)
head(dt_sm)

dt_sm$difference = abs(dt_sm$soil_moisture-dt_sm$prediction)

mean(dt_sm$difference)


#############################################


dt_st_walstein <- dt_st[dt_st$probe_name == "Waldstein_2"|dt_st$probe_name == "Waldstein_3",] 

dt_st_walstein_05 <- dt_st_walstein[dt_st_walstein$depths == 5,] 

dt_st_walstein_15 <- dt_st_walstein[dt_st_walstein$depths == 15,] 

dt_st_walstein_25 <- dt_st_walstein[dt_st_walstein$depths == 25,] 

dt_st_walstein_35 <- dt_st_walstein[dt_st_walstein$depths == 35,] 

library("viridis")
viridis(4)


plot(dt_st_walstein_05$date_hour,dt_st_walstein_05$soil_temperature,col = "#440154FF",pch=19,
     ylab="Soil temperature [°C]",xlab="")
points(dt_st_walstein_05$date_hour,dt_st_walstein_05$prediction,col="#35B779FF")
# points(test_set_walstein_15$date_hour,test_set_walstein_15$soil_temperature,col ="#31688EFF",pch=19)
# points(test_set_walstein_15$date_hour,test_set_walstein_15$prediction,col ="#31688EFF")
# points(test_set_walstein_25$date_hour,test_set_walstein_25$soil_temperature,col ="#35B779FF",pch=19)
# points(test_set_walstein_25$date_hour,test_set_walstein_25$prediction,col ="#35B779FF")
# points(test_set_walstein_35$date_hour,test_set_walstein_35$soil_temperature,col ="#FDE725FF",pch=19)
# points(test_set_walstein_35$date_hour,test_set_walstein_35$prediction,col ="#FDE725FF")


plot(dt_st_walstein_05$date_hour,dt_st_walstein_05$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(dt_st_walstein_05$date_hour,dt_st_walstein_05$prediction,col="#35B779FF",lwd=2)
# lines(test_set_walstein_15$date_hour,test_set_walstein_15$soil_temperature,col ="#31688EFF",lwd=3)
# lines(test_set_walstein_15$date_hour,test_set_walstein_15$prediction,col ="#31688EFF",lwd=3,lty=3)
# lines(test_set_walstein_25$date_hour,test_set_walstein_25$soil_temperature,col ="#35B779FF",lwd=3)
# lines(test_set_walstein_25$date_hour,test_set_walstein_25$prediction,col ="#35B779FF",lwd=3,lty=3)



plot(dt_st_walstein_35$date_hour,dt_st_walstein_35$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(dt_st_walstein_35$date_hour,dt_st_walstein_35$prediction,col="#35B779FF",lwd=2)



plot(dt_st_walstein_05$date_hour,dt_st_walstein_05$prediction,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(dt_st_walstein_35$date_hour,dt_st_walstein_35$prediction,col="#35B779FF",lwd=3)


plot(dt_st_walstein_05$date_hour[4000:4500],dt_st_walstein_05$soil_temperature[4000:4500],col = "#440154FF",type="l",lwd=3,ylim=c(5,20),
     ylab="Soil temperature [°C]",xlab="")
lines(dt_st_walstein_05$date_hour[4000:4500],dt_st_walstein_05$prediction[4000:4500],col="#35B779FF",lwd=3)





#########################################

dt_st_voitsumra <- dt_st[dt_st$probe_name == "Voitsumra",] 

dt_st_voitsumra_05 <- dt_st_voitsumra[dt_st_voitsumra$depths == 5,] 

dt_st_voitsumra_15 <- dt_st_voitsumra[dt_st_voitsumra$depths == 15,] 

dt_st_voitsumra_25 <- dt_st_voitsumra[dt_st_voitsumra$depths == 25,] 

dt_st_voitsumra_35 <- dt_st_voitsumra[dt_st_voitsumra$depths == 35,] 




plot(dt_st_voitsumra_05$date_hour[3000:3500],dt_st_voitsumra_05$soil_temperature[3000:3500],col = "#440154FF",type="l",lwd=3,ylim=c(5,20),
     ylab="Soil temperature [°C]",xlab="")
lines(dt_st_voitsumra_05$date_hour[3000:3500],dt_st_voitsumra_05$prediction[3000:3500],col="#35B779FF",lwd=3)



plot(test_set_voitsumra_05$date_hour,test_set_voitsumra_05$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,30),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_voitsumra_05$date_hour,test_set_voitsumra_05$prediction,col="#35B779FF",lwd=1)


plot(test_set_voitsumra_35$date_hour,test_set_voitsumra_35$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,30),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_voitsumra_35$date_hour,test_set_voitsumra_35$prediction,col="#35B779FF",lwd=1)



#############################################################################






dt_sm_walstein <- dt_sm[dt_sm$probe_name == "Waldstein_2"|dt_sm$probe_name == "Waldstein_3",] 

dt_sm_walstein_05 <- dt_sm_walstein[dt_sm_walstein$depths == 5,] 


plot(dt_sm_walstein_05$date_hour,dt_sm_walstein_05$soil_moisture,col = "#440154FF",pch=19,
     ylab="Soil moisture [%]",xlab="",ylim=c(0,30))
points(dt_sm_walstein_05$date_hour,dt_sm_walstein_05$prediction,col="#35B779FF")



###############################################


dt_sm_voitsumra <- dt_sm[dt_sm$probe_name == "Voitsumra",] 

dt_sm_voitsumra_05 <- dt_sm_voitsumra[dt_sm_voitsumra$depths == 05,] 

plot(dt_sm_voitsumra_05$date_hour,dt_sm_voitsumra_05$soil_moisture,col = "#440154FF",type="l",lwd=3,
     ylab="Soil temperature [°C]",xlab="")
lines(dt_sm_voitsumra_05$date_hour,dt_sm_voitsumra_05$prediction,col="#35B779FF",lwd=3)


mean(dt_sm_voitsumra_05$difference)


dt_sm_voitsumra <- dt_sm[dt_sm$probe_name == "S08_014",] 

dt_sm_voitsumra_05 <- dt_sm_voitsumra[dt_sm_voitsumra$depths == 05,] 

plot(dt_sm_voitsumra_05$date_hour,dt_sm_voitsumra_05$soil_moisture,col = "#440154FF",type="l",lwd=3,
     ylab="Soil temperature [°C]",xlab="",ylim=c(0,40))
lines(dt_sm_voitsumra_05$date_hour,dt_sm_voitsumra_05$prediction,col="#35B779FF",lwd=3)


mean(dt_sm_voitsumra_05$difference)

boxplot(dt_sm$difference~dt_sm$probe_name)

