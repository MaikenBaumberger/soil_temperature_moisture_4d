
library("CAST")
library("caret")

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_plot_id.Rdata")

data.table::uniqueN(predictor_set_2$date)

predictor_set_temperature = predictor_set_2[ , -which(names(predictor_set_2) %in% c("M_org","M_05","M_15","M_25","M_35","M_45","M_55",
                                                                                    "M_65","M_75","M_85","M_95","M_105","M_115",
                                                                                    "measurement","T_95","T_105","T_115"))]

names(predictor_set_temperature)

predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[42:43],predictor_set_temperature[13:41],predictor_set_temperature[4:12])

names(predictor_set_temperature_2)

########################

dt <- subset(predictor_set_temperature_2,
             date_hour >= as.POSIXct('2022-01-01 00:00') &
               date_hour <= as.POSIXct('2022-12-31 23:59'))


dt$soil_texture = as.factor(dt$soil_texture)
dt$soil_type = as.factor(dt$soil_type)
dt$land_use = as.factor(dt$land_use)

data.table::uniqueN(dt$plot_id)

spacetimefolds <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date", k=3)

plot(spacetimefolds$index[[1]][1:10000],spacetimefolds$index[[1]][1:10000])
points(spacetimefolds$indexOut[[1]],spacetimefolds$indexOut[[1]],col="red")

plot(spacetimefolds$index[[2]][1:10000],spacetimefolds$index[[2]][1:10000])
points(spacetimefolds$indexOut[[2]],spacetimefolds$indexOut[[2]],col="red")

#create space time folds

train = dt[spacetimefolds$index[[1]],]
test = dt[spacetimefolds$indexOut[[1]],]

plot(dt$date_hour,dt$T_25)
points(test$date_hour,test$T_25,col="red")

plot(train$date_hour,train$T_25)
points(test$date_hour,test$T_25,col="red")

#data.table::uniqueN(train$plot_id)
#data.table::uniqueN(test$plot_id)

#data.table::uniqueN(train$date)
#data.table::uniqueN(test$date)

###############################################

#unique(test$date)
#unique(train$date)

###############################################

#random sample

#train_sub =  train[sample(1:nrow(train), 10000), ]  

##############################################

#train set

train_melt <- reshape2::melt(train, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                               "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                                               "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                               "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                               "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                               "date", "plot_id"
)) 

train_melt$depths = substr(train_melt$variable, 3,5)

train_melt$soil_temperature=train_melt$value

train_set = train_melt[ , -which(names(train_melt) %in% c("value","variable"))]


train_set=train_set[complete.cases(train_set), ]

train_set$depths = as.numeric(train_set$depths)

#unique(train_set$date)
#unique(train_set$probe_name)

#################################################


test_melt <- reshape2::melt(test, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                         "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                                         "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                         "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                         "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar",
                                         "date", "plot_id"
)) 

test_melt$depths = substr(test_melt$variable, 3,5)

test_melt$soil_temperature=test_melt$value

test_set = test_melt[ , -which(names(test_melt) %in% c("value","variable"))]


test_set=test_set[complete.cases(test_set), ]

test_set$depths = as.numeric(test_set$depths)


#################################################

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id",timevar ="date", k=5)#



setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results/compare_train_ffs_all_data")

save(train_set,file = "train_set.Rdata")
save(test_set,file = "test_set.Rdata")
save(cv_spacetimefolds,file = "cv_spacetimefolds.Rdata")
