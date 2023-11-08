load("C:/Users/maike/Desktop/Carbon4D/Palma/soil_moisture_model_v1/ffs_model.Rdata")


ffsmodel
varImp(ffsmodel,conditional=TRUE)
plot(varImp(ffsmodel))


plot(ffsmodel$pred$obs,ffsmodel$pred$pred,ylim=c(0,40),xlim=c(0,40),pch=20,col= rgb(red = 0, green = 0, blue = 1, alpha = 0.01))
abline(coef = c(0,1))



##########################################
library(raster)
library(zoo)
library(mapview)
library(sf)
library(caret)
library(CAST)
library(ggplot2)

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


head(dt)

plot(dt$date_hour,dt$M_05,col = factor(dt$land_use))

ggplot(dt, aes(x = date_hour, y = M_05, colour = elevation)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(dt, aes(x = date_hour, y = M_05, colour = prec_sum_3_month)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(dt, aes(x = date_hour, y = M_05, colour = topo_wetness)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))



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

train_melt$soil_moisture=train_melt$value

train_set = train_melt[ , -which(names(train_melt) %in% c("value","variable"))]


train_set=train_set[complete.cases(train_set), ]

train_set$depths = as.numeric(train_set$depths)

head(train_set)

ggplot(train_set, aes(x = date_hour, y = soil_moisture, colour = depths)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))


