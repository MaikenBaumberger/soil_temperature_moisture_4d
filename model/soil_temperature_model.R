

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete.Rdata")


predictor_set_temperature = predictor_set_2[ , -which(names(predictor_set_2) %in% c("M_org","M_05","M_15","M_25","M_35","M_45","M_55",
                                                                                    "M_65","M_75","M_85","M_95","M_105","M_115",
                                                                                    "measurement","T_95","T_105","T_115"))]


names(predictor_set_temperature)

predictor_set_temperature_2 = cbind(predictor_set_temperature[1:2],predictor_set_temperature[13:41],predictor_set_temperature[4:12])

names(predictor_set_temperature_2)


melt_data <- reshape2::melt(predictor_set_temperature_2, id = c("probe_name","date_hour","id","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                                                                "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                                                                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                                                                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                                                                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar"    
)) 

melt_data$depths = substr(melt_data$variable, 3,5)

melt_data$soil_temperature=melt_data$value

predictor_set_temperature = melt_data[ , -which(names(melt_data) %in% c("value","variable"))]



dt <- subset(predictor_set_temperature,
                         date_hour >= as.POSIXct('2022-01-01 00:00') &
                           date_hour <= as.POSIXct('2022-12-31 23:59'))

dt=dt[complete.cases(dt), ]

dt$depths = as.numeric(dt$depths)

########################################

#probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
#probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
#probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

#probe_location = cbind(probe_meta_data[1], probe_meta_data[7:8])

#dt_lat_lon = merge(dt, probe_location, by.x = "probe_name",by.y = "probe_id", all.x = T)

#dt_lat_lon = dt_lat_lon[ , -which(names(dt_lat_lon) %in% c("id"))]        

#dt_lat_lon= cbind(dt_lat_lon[1:31],dt_lat_lon[33:34],dt_lat_lon[32])

#save(dt_lat_lon,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/dt_lat_lon.Rdata")



########################################

#kategorielle daten einzelne vorkommen löschen


###################################################

#static variables as factors

dt$soil_texture = as.factor(dt$soil_texture)
dt$soil_type = as.factor(dt$soil_type)
dt$land_use = as.factor(dt$land_use)




# Manuell predictors selektieren:
predictors <- c("air_temperature_mountain","air_temperature_valley","precipitation","global_radiation",
                "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")

response <- "soil_temperature" 

####################################################



### Trainingsdaten definieren: ####
## Leave-One-Out:
# 1/4 der Daten auslassen, um mit ihnen spaeter zu validieren:
IDs=unique(dt$probe_name)
Random_IDs=sample(IDs,size = length(IDs)*0.2)

traindat <- dt[dt$probe_name %in% Random_IDs, ]
vardat <- dt[!dt$probe_name %in% Random_IDs, ]


# Subset der Daten zum schnelleren Rechnen:
trainIDs <- caret::createDataPartition(traindat$probe_name,p=0.025,list = FALSE) # Alle train-Daten so ca 390.000
# Auf ca 2180 Trainingsdaten kommen (==Anzahl Trainingsdaten im TopSoil-Model)!
traindat_sub <- traindat[trainIDs,]

timefolds <- CAST::CreateSpacetimeFolds(traindat_sub,spacevar = "probe_name")

# Dasselbe fuer Var-Daten:
varIDs <- caret::createDataPartition(vardat$probe_name,p=0.1,list = FALSE) # Alle var-Daten so ca 120.000
# Auf ca ~12k vardaten kommen wie bei TopSoil
var_sub <- vardat[varIDs,]

###################################################

#### Forward feature selection: #### 
# (ca. 3,5 st Laufzeit bei 2185 Zeilen & 18 Preds und PC alleine gelassen) 
Sys.time()
#model_ffs_DS <- ffs(predictors =  traindat_sub[,predictors],
#                    response =   traindat_sub[,response],
#                    method="rf",#rf
#                    tuneLength=5,
#                    ntree = 75,
#                    mtry=2,
#                    trControl=trainControl(method="cv",index = timefolds$index,indexOut = timefolds$indexOut,
#                                           savePredictions = "final"))
Sys.time()


setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results")

save(traindat,traindat_sub,trainIDs, vardat,var_sub,varIDs, file = "Data_Model_DeepSoil_2.Rdata")
save(model_ffs_DS,var_sub,file = "Model_DeepSoil.Rdata_2")


###################################################


## Modell betrachten:
plot(model_ffs_DS)
plot_ffs(model_ffs_DS)

## Welche predictors ausgewaehlt?
model_ffs_DS$selectedvars

varImp(model_ffs_DS,conditional=TRUE)

plot(varImp(model_ffs_DS))


####################################################

vardat[,"Prediction"]=round(predict.train(object=model_ffs_DS, newdata = vardat,na.action = na.omit),
                             digits = 2)

head(vardat)

plot(vardat$soil_temperature,vardat$Prediction,ylim=c(0,40),xlim=c(0,40))

vardat$dif = abs(vardat$soil_temperature - vardat$Prediction)



library(ggplot2)

ggplot(vardat, aes(x=land_use, y=dif, fill=land_use)) + 
  geom_boxplot(outlier.shape = NA)+
  ylim(0, 7)

ggplot(vardat, aes(x=depths, y=dif, fill=depths)) + 
  geom_boxplot(outlier.shape = NA)+
  ylim(0, 7)

plot(vardat$date_hour,vardat$dif)

plot(vardat$date_hour,vardat$soil_temperature)
points(vardat$date_hour,vardat$Prediction,col="red")


plot(vardat$date_hour[100:200],vardat$soil_temperature[100:200],type="l",ylim=c(-5,10),ylab="soil temperature [°C]",xlab="time")
points(vardat$date_hour[100:200],vardat$Prediction[100:200],col="red",type="l")
points(vardat$date_hour[100:200],vardat$air_temperature_mountain[100:200],col="grey",type="l")

plot(vardat$date_hour[2100:2200],vardat$soil_temperature[2100:2200],type="l",ylab="soil temperature [°C]",xlab="time")
points(vardat$date_hour[2100:2200],vardat$Prediction[2100:2200],col="red",type="l")
points(vardat$date_hour[2100:2200],vardat$air_temperature_mountain[2100:2200],col="grey",type="l")

plot(vardat$date_hour[12100:12200],vardat$soil_temperature[12100:12200],ylim=c(0,25),type="l",ylab="soil temperature [°C]",xlab="time")
points(vardat$date_hour[12100:12200],vardat$Prediction[12100:12200],col="red",type="l")
points(vardat$date_hour[12100:12200],vardat$air_temperature_mountain[12100:12200],col="grey",type="l")

vardat[100:100,]


vardat[2100:2100,]


vardat2 = vardat

vardat2$date <- format(strptime(vardat2$date_hour,"%Y-%m-%d"),"%Y-%m-%d %H:%M:%S %Z")
vardat2$date <- as.POSIXct(strptime(vardat2$date,"%Y-%m-%d %H:%M:%S"))

temporal_error = aggregate(vardat2$dif, list(vardat2$date), mean)
names(temporal_error) = c("time","dif")


plot(temporal_error$time,temporal_error$dif,type="l")


vardat2 = vardat

vardat2$date <- format(strptime(vardat2$date_hour,"%Y-%m-%d"),"%Y-%m-%d %H:%M:%S %Z")
vardat2$date <- as.POSIXct(strptime(vardat2$date,"%Y-%m-%d %H:%M:%S"))

temporal_error = aggregate(vardat2$dif, list(vardat2$date), mean)
names(temporal_error) = c("time","dif")


plot(temporal_error$time,temporal_error$dif,type="l")

temporal_error$runmed = runmed(temporal_error$dif, 21, endrule = "median")

plot(temporal_error$time,temporal_error$runmed,type="l",lwd=1,ylab="MAE",xlab="time")


#######################################

#######################################

dt = dt[ , -which(names(dt) %in% c("air_temperature_valley","precipitation","global_radiation",
                                   "relative_humidity","air_pressure","wind_speed","soil_texture",
                                   "soil_type","elevation","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar"))]




predictors <- c("air_temperature_mountain","air_temperature_mountain_1","air_temperature_mountain_2",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "land_use","depths")


### Trainingsdaten definieren: ####
## Leave-One-Out:
# 1/4 der Daten auslassen, um mit ihnen spaeter zu validieren:
IDs=unique(dt$probe_name)
Random_IDs=sample(IDs,size = length(IDs)*0.2)

traindat <- dt[dt$probe_name %in% Random_IDs, ]
vardat <- dt[!dt$probe_name %in% Random_IDs, ]


# Subset der Daten zum schnelleren Rechnen:
trainIDs <- caret::createDataPartition(traindat$probe_name,p=0.025,list = FALSE) # Alle train-Daten so ca 390.000
# Auf ca 2180 Trainingsdaten kommen (==Anzahl Trainingsdaten im TopSoil-Model)!
traindat_sub <- traindat[trainIDs,]

timefolds <- CAST::CreateSpacetimeFolds(traindat_sub,spacevar = "probe_name")

# Dasselbe fuer Var-Daten:
varIDs <- caret::createDataPartition(vardat$probe_name,p=0.1,list = FALSE) # Alle var-Daten so ca 120.000
# Auf ca ~12k vardaten kommen wie bei TopSoil
var_sub <- vardat[varIDs,]


dt = dt[ , -which(names(dt) %in% c("probe_name","date_hour","id"))]

library("CAST")
library("caret")

model_ffs_ranger <- train(traindat_sub[,predictors],
                          traindat_sub[,response],
                          method="ranger",#rf
                          tuneLength=5,
                          metric="RMSE",
                          importance = "permutation",
                          trControl=trainControl(method="cv",index = timefolds$index,indexOut = timefolds$indexOut,
                                                savePredictions = "final"))

model_ffs_ranger

varImp(model_ffs_ranger,conditional=TRUE)
plot(varImp(model_ffs_ranger))


#IDs=unique(dt$soil_temperature)
#random_sample = sample(IDs,size = length(IDs)*8)
#local_obs <- dt[dt$depths %in% random_sample, ]

dt= dt[which(dt$depth == 5),]
          
          
local_obs = dt[sample(nrow(dt), 6), ]
local_obs = local_obs[ , -which(names(local_obs) %in% c("soil_temperature"))]

explainer_caret <- lime::lime(local_obs, model_ffs_ranger, n_bins = 5)
class(explainer_caret)
summary(explainer_caret)


explanation_caret <- lime::explain(
  x = local_obs, 
  explainer = explainer_caret, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 12, 
  feature_select = "highest_weights",
  labels = "Yes"
)

explanation_caret

lime::plot_features(explanation_caret)




#https://stackoverflow.com/questions/48334929/r-using-ranger-with-caret-tunegrid-argument

#tgrid <- expand.grid(
#  .mtry = 2:4,
#  .splitrule = "gini",
#  .min.node.size = c(10, 20)
#)

#model_caret <- train(soil_temperature  ~ ., data = traindat_sub,
#                     method = "ranger",
#                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
#                     tuneGrid = tgrid,
#                     num.trees = 100,
#                     importance = "permutation")

#model <- train(trainDat[,predictors],
#               trainDat[,response],
#               method="rf",
 #              tuneLength=5,
 #warning              trControl=trainControl(method="cv",savePredictions = "final"))
