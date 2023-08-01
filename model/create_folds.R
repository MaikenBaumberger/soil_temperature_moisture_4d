
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


spacetimefolds <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date", k=4)

plot(spacetimefolds$index[[1]][1:10000],spacetimefolds$index[[1]][1:10000])
points(spacetimefolds$indexOut[[1]],spacetimefolds$indexOut[[1]],col="red")

plot(spacetimefolds$index[[2]][1:10000],spacetimefolds$index[[2]][1:10000])
points(spacetimefolds$indexOut[[2]],spacetimefolds$indexOut[[2]],col="red")

#create space time folds

train = dt[spacetimefolds$index[[1]],]
test = dt[spacetimefolds$indexOut[[1]],]

data.table::uniqueN(train$plot_id)
data.table::uniqueN(test$plot_id)

data.table::uniqueN(train$date)
data.table::uniqueN(test$date)

###############################################

unique(test$date)
unique(train$date)

###############################################

#random sample

train_sub =  train[sample(1:nrow(train), 10000), ]  

##############################################

train_melt <- reshape2::melt(train_sub, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
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

unique(train_set$date)
unique(train_set$probe_name)

##############

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id",timevar ="date", k=5)


hyperparameter = expand.grid(mtry = seq(2,10),
                       min.node.size = c(5,10,15),
                       splitrule = c("variance"))

predictors <- c("air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")

response <- "soil_temperature" 


rfmodel = caret::train(x =  train_set[,predictors],
                       y =   train_set[,response],
                       method = "ranger",
                       tuneGrid = hyperparameter,
                       num.trees = 100,
                       trControl = trainControl(method = "cv", number = 5,
                                                index = cv_spacetimefolds$index, indexOut = cv_spacetimefolds$indexOut,
                                                savePredictions = "final"),
                       importance = "permutation")

rfmodel

varImp(rfmodel,conditional=TRUE)
plot(varImp(rfmodel))

#############################################

model_prediction = data.frame(rfmodel$pred)


plot(rfmodel$pred$pred,rfmodel$pred$obs,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))
