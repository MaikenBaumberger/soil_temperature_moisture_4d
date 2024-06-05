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

predictor_set_temperature = predictor_set_2[ , -which(names(predictor_set_2) %in% c("M_org","M_05","M_15","M_25","M_35","M_45","M_55",
                                                                                    "M_65","M_75","M_85","M_95","M_105","M_115",
                                                                                    "measurement","T_95","T_105","T_115","day","week","month"))]

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

train_sub =  dt#train[sample(1:nrow(train), 50000), ]  #50000

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

train_melt$soil_temperature=train_melt$value

train_set = train_melt[ , -which(names(train_melt) %in% c("value","variable"))]


train_set=train_set[complete.cases(train_set), ]

train_set$depths = as.numeric(train_set$depths)


test_set = train_set

#save(test_set,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/test_set_waldstein.Rdata")


categories <- unique(test_set$probe_name)
categories

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

ffsmodel
ffsmodel$selectedvars

#############################################

#test set

test_set[,"prediction"]=round(predict.train(object=ffsmodel, newdata = test_set,na.action = na.omit),
                              digits = 2)
head(test_set)

test_set$difference = abs(test_set$soil_temperature-test_set$prediction)

mean(test_set$difference)

plot(test_set$soil_temperature,test_set$prediction,ylim=c(-1,34),xlim=c(-1,34),
     pch=19,col=alpha("black",0.05),xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
abline(coef = c(0,1))


#############################################

test_set_walstein <- test_set[test_set$probe_name == "S02_014",]

test_set_walstein <- test_set[test_set$probe_name == "Waldstein_2"|test_set$probe_name == "Waldstein_3",] 

test_set_walstein_05 <- test_set_walstein[test_set_walstein$depths == 5,] 

test_set_walstein_15 <- test_set_walstein[test_set_walstein$depths == 15,] 

test_set_walstein_25 <- test_set_walstein[test_set_walstein$depths == 25,] 

test_set_walstein_35 <- test_set_walstein[test_set_walstein$depths == 35,] 

library("viridis")
viridis(4)


plot(test_set_walstein_05$date_hour,test_set_walstein_05$soil_temperature,col = "#440154FF",pch=19,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
points(test_set_walstein_05$date_hour,test_set_walstein_05$prediction,col="#35B779FF")
# points(test_set_walstein_15$date_hour,test_set_walstein_15$soil_temperature,col ="#31688EFF",pch=19)
# points(test_set_walstein_15$date_hour,test_set_walstein_15$prediction,col ="#31688EFF")
# points(test_set_walstein_25$date_hour,test_set_walstein_25$soil_temperature,col ="#35B779FF",pch=19)
# points(test_set_walstein_25$date_hour,test_set_walstein_25$prediction,col ="#35B779FF")
# points(test_set_walstein_35$date_hour,test_set_walstein_35$soil_temperature,col ="#FDE725FF",pch=19)
# points(test_set_walstein_35$date_hour,test_set_walstein_35$prediction,col ="#FDE725FF")


plot(test_set_walstein_05$date_hour,test_set_walstein_05$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_walstein_05$date_hour,test_set_walstein_05$prediction,col="#35B779FF",lwd=2)
# lines(test_set_walstein_15$date_hour,test_set_walstein_15$soil_temperature,col ="#31688EFF",lwd=3)
# lines(test_set_walstein_15$date_hour,test_set_walstein_15$prediction,col ="#31688EFF",lwd=3,lty=3)
# lines(test_set_walstein_25$date_hour,test_set_walstein_25$soil_temperature,col ="#35B779FF",lwd=3)
# lines(test_set_walstein_25$date_hour,test_set_walstein_25$prediction,col ="#35B779FF",lwd=3,lty=3)



plot(test_set_walstein_35$date_hour,test_set_walstein_35$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_walstein_35$date_hour,test_set_walstein_35$prediction,col="#35B779FF",lwd=2)



plot(test_set_walstein_05$date_hour,test_set_walstein_05$prediction,col = "#440154FF",type="l",lwd=3,ylim=c(0,15),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_walstein_35$date_hour,test_set_walstein_35$prediction,col="#35B779FF",lwd=3)


plot(test_set_walstein_05$date_hour[4000:4500],test_set_walstein_05$soil_temperature[4000:4500],col = "#440154FF",type="l",lwd=3,ylim=c(5,20),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_walstein_05$date_hour[4000:4500],test_set_walstein_05$prediction[4000:4500],col="#35B779FF",lwd=3)





#########################################

test_set_voitsumra <- test_set[test_set$probe_name == "Voitsumra",] 

test_set_voitsumra_05 <- test_set_voitsumra[test_set_voitsumra$depths == 5,] 

test_set_voitsumra_15 <- test_set_voitsumra[test_set_voitsumra$depths == 15,] 

test_set_voitsumra_25 <- test_set_voitsumra[test_set_voitsumra$depths == 25,] 

test_set_voitsumra_35 <- test_set_voitsumra[test_set_voitsumra$depths == 35,] 




plot(test_set_voitsumra_05$date_hour[3000:3500],test_set_voitsumra_05$soil_temperature[3000:3500],col = "#440154FF",type="l",lwd=3,ylim=c(5,20),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_voitsumra_05$date_hour[3000:3500],test_set_voitsumra_05$prediction[3000:3500],col="#35B779FF",lwd=3)



plot(test_set_voitsumra_05$date_hour,test_set_voitsumra_05$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,30),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_voitsumra_05$date_hour,test_set_voitsumra_05$prediction,col="#35B779FF",lwd=1)


plot(test_set_voitsumra_35$date_hour,test_set_voitsumra_35$soil_temperature,col = "#440154FF",type="l",lwd=3,ylim=c(0,30),
     ylab="Soil temperature [°C]",xlab="")
lines(test_set_voitsumra_35$date_hour,test_set_voitsumra_35$prediction,col="#35B779FF",lwd=1)





