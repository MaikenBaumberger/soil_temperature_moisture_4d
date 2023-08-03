
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


spacetimefolds <- CAST::CreateSpacetimeFolds(dt,spacevar = "plot_id",timevar ="date", k=4)

plot(spacetimefolds$index[[1]][1:10000],spacetimefolds$index[[1]][1:10000])
points(spacetimefolds$indexOut[[1]],spacetimefolds$indexOut[[1]],col="red")

plot(spacetimefolds$index[[2]][1:10000],spacetimefolds$index[[2]][1:10000])
points(spacetimefolds$indexOut[[2]],spacetimefolds$indexOut[[2]],col="red")

#create space time folds

train = dt[spacetimefolds$index[[1]],]
test = dt[spacetimefolds$indexOut[[1]],]

#data.table::uniqueN(train$plot_id)
#data.table::uniqueN(test$plot_id)

#data.table::uniqueN(train$date)
#data.table::uniqueN(test$date)

###############################################

#unique(test$date)
#unique(train$date)

###############################################

#random sample

train_sub =  train[sample(1:nrow(train), 10000), ]  

##############################################

#train set

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

#hyperparameter = expand.grid(mtry = seq(2,15),
#                       min.node.size = c(5,10,15),
#                       splitrule = c("variance"))

hyperparameter = expand.grid(mtry = 2,
                             min.node.size = 5,
                             splitrule = "variance")

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results/compare_train_ffs")

save(train_set,file = "train_set.Rdata")
save(test_set,file = "test_set.Rdata")
save(cv_spacetimefolds,file = "cv_spacetimefolds.Rdata")


predictors <- c("air_temperature_mountain","precipitation","global_radiation",
                "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")

response <- "soil_temperature" 

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


Sys.time()
ffsmodel = CAST::ffs(predictors =  train_set[,predictors],
                    response =   train_set[,response],
                    method = "ranger",
                    tuneGrid = hyperparameter,
                    num.trees = 100,
                    trControl = trainControl(method = "cv", number = 5,
                                             index = cv_spacetimefolds$index, 
                                             indexOut = cv_spacetimefolds$indexOut,
                                             savePredictions = "final"),
                    importance = "permutation")
Sys.time()

ffsmodel
varImp(ffsmodel,conditional=TRUE)
plot(varImp(ffsmodel))

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results/compare_train_ffs")

save(rfmodel,file = "rf_model_spacetimefolds.Rdata")
save(ffsmodel,file = "ffs_model_spacetimefolds.Rdata")


#############################################

model_prediction = data.frame(rfmodel$pred)


plot(rfmodel$pred$pred,rfmodel$pred$obs,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

plot(ffsmodel$pred$pred,ffsmodel$pred$obs,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))


###########################################
#test set

test_set[,"prediction"]=round(predict.train(object=ffsmodel, newdata = test_set,na.action = na.omit),
                            digits = 2)
head(test_set)

test_set$difference = abs(test_set$soil_temperature-test_set$prediction)

mean(test_set$difference)

plot(test_set$soil_temperature,test_set$prediction,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

#############################################

library(ggplot2)

ggplot(test_set, aes(x=land_use, y=difference, fill=land_use)) + 
  geom_boxplot(outlier.shape = NA)+
  ylim(0, 5)

ggplot(test_set, aes(x=depths, y=difference, group=depths, fill=depths)) + 
  geom_boxplot(outlier.shape = NA)+
  ylim(0, 5)

plot(test_set$date_hour,test_set$dif)

plot(test_set$date_hour,test_set$soil_temperature)
points(test_set$date_hour,test_set$prediction,col="red")



test_set$date <- format(strptime(test_set$date_hour,"%Y-%m-%d"),"%Y-%m-%d %H:%M:%S %Z")
test_set$date <- as.POSIXct(strptime(test_set$date,"%Y-%m-%d %H:%M:%S"))

temporal_error = aggregate(test_set$dif, list(test_set$date), mean)
names(temporal_error) = c("time","dif")


plot(temporal_error$time,temporal_error$dif,type="l")


###############################################
rsq <- function (x, y) cor(x, y) ^ 2

rsq(test_set$soil_temperature,test_set$prediction)

land_use_meadow <- test_set[test_set$land_use == 1600, ] 
land_use_arable <- test_set[test_set$land_use == 1500, ] 
land_use_forest <- test_set[test_set$land_use == 1000, ] 

rsq(land_use_meadow$soil_temperature,land_use_meadow$prediction)
rsq(land_use_arable$soil_temperature,land_use_arable$prediction)
rsq(land_use_forest$soil_temperature,land_use_forest$prediction)

plot(land_use_meadow$soil_temperature,land_use_meadow$prediction,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

plot(land_use_arable$soil_temperature,land_use_arable$prediction,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

plot(land_use_forest$soil_temperature,land_use_forest$prediction,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

################################################


depths_05 <- test_set[test_set$depths == 5, ] 
depths_15 <- test_set[test_set$depths == 15, ] 
depths_25 <- test_set[test_set$depths == 25, ] 
depths_35 <- test_set[test_set$depths == 35, ] 
depths_45 <- test_set[test_set$depths == 45, ] 
depths_55 <- test_set[test_set$depths == 55, ] 
depths_65 <- test_set[test_set$depths == 65, ] 
depths_75 <- test_set[test_set$depths == 75, ] 
depths_85 <- test_set[test_set$depths == 85, ] 

rsq(depths_05$soil_temperature,depths_05$prediction)
rsq(depths_15$soil_temperature,depths_15$prediction)
rsq(depths_25$soil_temperature,depths_25$prediction)
rsq(depths_35$soil_temperature,depths_35$prediction)
rsq(depths_45$soil_temperature,depths_45$prediction)
rsq(depths_55$soil_temperature,depths_55$prediction)
rsq(depths_65$soil_temperature,depths_65$prediction)
rsq(depths_75$soil_temperature,depths_75$prediction)
rsq(depths_85$soil_temperature,depths_85$prediction)


##################################################

test_set$month <- lubridate::month(test_set$date_hour)
head(test_set)

month_1 <- test_set[test_set$month == 1, ] 
month_2 <- test_set[test_set$month == 2, ] 
month_3 <- test_set[test_set$month == 3, ] 
month_4 <- test_set[test_set$month == 4, ] 
month_5 <- test_set[test_set$month == 5, ] 
month_6 <- test_set[test_set$month == 6, ] 
month_7 <- test_set[test_set$month == 7, ] 
month_8 <- test_set[test_set$month == 8, ] 
month_9 <- test_set[test_set$month == 9, ] 
month_10 <- test_set[test_set$month == 10, ] 
month_11 <- test_set[test_set$month == 11, ] 
month_12 <- test_set[test_set$month == 12, ] 

rsq(month_1$soil_temperature,month_1$prediction)
rsq(month_2$soil_temperature,month_2$prediction)
rsq(month_3$soil_temperature,month_3$prediction)
rsq(month_4$soil_temperature,month_4$prediction)
rsq(month_5$soil_temperature,month_5$prediction)
rsq(month_6$soil_temperature,month_6$prediction)
rsq(month_7$soil_temperature,month_7$prediction)
rsq(month_8$soil_temperature,month_8$prediction)
rsq(month_9$soil_temperature,month_9$prediction)
rsq(month_10$soil_temperature,month_10$prediction)
rsq(month_11$soil_temperature,month_11$prediction)
rsq(month_12$soil_temperature,month_12$prediction)

#################################################



