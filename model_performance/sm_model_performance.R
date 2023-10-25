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

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons_radolan.Rdata")
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
predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[55:56],predictor_set_temperature[13:54],predictor_set_temperature[4:12])



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
                                               "prec", "prec_sum_3", "prec_sum_6", "prec_sum_12", "prec_sum_24", "prec_sum_48", "prec_sum_72", "prec_sum_96", "prec_sum_120",
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

#################################################
head(test)

test_melt <- reshape2::melt(test, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                         "relative_humidity","air_pressure","wind_speed",
                                         "trend_week","trend_month", "trend_3month",
                                         "prec", "prec_sum_3", "prec_sum_6", "prec_sum_12", "prec_sum_24", "prec_sum_48", "prec_sum_72", "prec_sum_96", "prec_sum_120",
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

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id",timevar ="date", k=5)#

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
                "prec", "prec_sum_3", "prec_sum_6", "prec_sum_12", "prec_sum_24", "prec_sum_48", "prec_sum_72", "prec_sum_96", "prec_sum_120",
                "depths",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness")

#                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
#                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
#                "air_temperature_day","air_temperature_week","air_temperature_month",
#                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")

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



#test set

test_set[,"prediction"]=round(predict.train(object=rfmodel, newdata = test_set,na.action = na.omit),
                              digits = 2)
head(test_set)

test_set$difference = abs(test_set$soil_moisture-test_set$prediction)

mean(test_set$difference)

plot(test_set$soil_moisture,test_set$prediction,ylim=c(-5,40),xlim=c(-5,40))
abline(coef = c(0,1))


###############################


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


################################


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

rsq(month_1$soil_moisture,month_1$prediction)
rsq(month_2$soil_moisture,month_2$prediction)
rsq(month_3$soil_moisture,month_3$prediction)
rsq(month_4$soil_moisture,month_4$prediction)
rsq(month_5$soil_moisture,month_5$prediction)
rsq(month_6$soil_moisture,month_6$prediction)
rsq(month_7$soil_moisture,month_7$prediction)
rsq(month_8$soil_moisture,month_8$prediction)
rsq(month_9$soil_moisture,month_9$prediction)
rsq(month_10$soil_moisture,month_10$prediction)
rsq(month_11$soil_moisture,month_11$prediction)
rsq(month_12$soil_moisture,month_12$prediction)

#################################################

head(test_set)

names(test_set$probe_name)

S03_011 <- test_set[test_set$probe_name == "S04_006", ] 

plot(S03_011$date_hour,S03_011$soil_moisture)

S03_011_sub <- subset(S03_011,
                      date_hour >= as.POSIXct('2022-04-09 00:00') &
                        date_hour <= as.POSIXct('2022-04-12 23:59'))


plot(S03_011_sub$date_hour,S03_011_sub$soil_moisture,col=factor(S03_011$depths),pch=19)


ggplot() + 
  geom_line(S03_011_sub,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_moisture,group=depths, color=depths))+
  geom_line(S03_011_sub,size=1, mapping=aes(x=date_hour, y=prediction,group=depths, color=depths))+
  scale_colour_gradientn(colours=rainbow(4))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil moisture [%]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))

#####################


head(test_set)

names(test_set$probe_name)

S03_011 <- test_set[test_set$probe_name == "S02_006", ] 

plot(S03_011$date_hour,S03_011$soil_moisture)

S03_011_sub <- subset(S03_011,
                      date_hour >= as.POSIXct('2022-04-09 00:00') &
                        date_hour <= as.POSIXct('2022-04-12 23:59'))


plot(S03_011_sub$date_hour,S03_011_sub$soil_moisture,col=factor(S03_011$depths),pch=19)


ggplot() + 
  geom_line(S03_011_sub,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_moisture,group=depths, color=depths))+
  geom_line(S03_011_sub,size=1, mapping=aes(x=date_hour, y=prediction,group=depths, color=depths))+
  scale_colour_gradientn(colours=rainbow(4))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil moisture [%]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))

################################


plot(test_set$date_hour,test_set$soil_moisture,ylab="soil moisture [%]",xlab="")
points(test_set$date_hour,test_set$prediction,col="red")

