

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


load("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model/ffs_model_spacetimefolds.Rdata")


ffsmodel
varImp(ffsmodel,conditional=TRUE)
plot(varImp(ffsmodel))


plot(ffsmodel$pred$obs,ffsmodel$pred$pred,ylim=c(0,25),xlim=c(0,25),pch=20,col= rgb(red = 0, green = 0, blue = 1, alpha = 0.01))
abline(coef = c(0,1))



load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons_radolan_4.Rdata")
#predictor_set_complete_seasons_radolan.Rdata
#predictor_set_complete_seasons.Rdata

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


head(dt)

plot(dt$date_hour,dt$T_05,col = factor(dt$land_use))

ggplot(dt, aes(x = date_hour, y = T_05, colour = air_temperature_mountain_72)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(dt, aes(x = date_hour, y = T_05, colour = land_use)) +
  geom_point()
  #scale_colour_gradientn(colours = terrain.colors(10))

ggplot(dt, aes(x = date_hour, y = T_05, colour = ndwi)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))


ggplot(dt, aes(x = date_hour, y = T_25, colour = probe_name)) +
  geom_point( show.legend = FALSE)

dt$probe_name
dt_sub <- subset(dt,probe_name == "S01_025"|
                    probe_name == "S01_017"|
                   probe_name == "S08_001"|
                   probe_name == "S05_002"|
                   probe_name == "S06_002"|
                   probe_name == "S09_003"|
                   probe_name == "S02_004"|
                   probe_name == "S01_005"|
                   probe_name == "S01_006"|
                   probe_name == "S12_007"|
                   probe_name == "S15_008"|
                   probe_name == "S03_009"|
                   probe_name == "S02_010"|
                   probe_name == "S09_014"|
                   probe_name == "S02_012"|
                   probe_name == "S01_011"|
                   probe_name == "S01_013"|
                   probe_name == "S03_006"|
                   probe_name == "S02_007"|
                   probe_name == "S09_008"|
                   probe_name == "S02_012"|
                   probe_name == "S01_011"|
                   probe_name == "S01_013"|
                   probe_name == "S12_012"|
                   probe_name == "S02_007"|
                   probe_name == "S09_008"|
                   probe_name == "S02_010"|
                   probe_name == "S12_009"|
                   probe_name == "S04_013"|
                   probe_name == "S12_012"|
                   probe_name == "S12_011"|
                   probe_name == "S04_011"|
                   probe_name == "S12_011"|
                   probe_name == "S12_008"|
                   probe_name == "S12_009"|
                   probe_name == "S15_011")

ggplot(dt_sub, aes(x = date_hour, y = T_25, colour = plot_id)) +
  geom_point( show.legend = FALSE)

ggplot(dt, aes(x = date_hour, y = T_25)) +
  geom_point(show.legend = FALSE)+
  geom_point(dt_sub, aes(x = date_hour, y = T_25, colour = plot_id),show.legend = FALSE)



p1 = ggplot(dt, aes(x = date_hour, y = T_25, colour = probe_name)) +
  geom_point( show.legend = FALSE)

p2 = p1 + geom_point(aes(x = dt_sub$date_hour, y = dt_sub$T_25, colour =dt_sub$plot_id),show.legend = FALSE)

p2


ggplot(dt, aes(x = date_hour, y = T_25)) +
  geom_point(show.legend = FALSE)+
  geom_point(data = dt_sub, mapping = aes(x = dt_sub$date_hour, y = dt_sub$T_25, colour =dt_sub$plot_id),show.legend = FALSE)



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

train_melt$soil_temperature=train_melt$value

train_set = train_melt[ , -which(names(train_melt) %in% c("value","variable"))]


train_set=train_set[complete.cases(train_set), ]

train_set$depths = as.numeric(train_set$depths)

head(train_set)

ggplot(train_set, aes(x = date_hour, y = soil_temperature, colour = depths)) +
  geom_point()+
  scale_colour_gradientn(colours = terrain.colors(10))


