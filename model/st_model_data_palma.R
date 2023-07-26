

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
trainIDs <- caret::createDataPartition(traindat$probe_name,p=0.001,list = FALSE) # Alle train-Daten so ca 390.000
# Auf ca 2180 Trainingsdaten kommen (==Anzahl Trainingsdaten im TopSoil-Model)!
traindat_sub <- traindat[trainIDs,]

timefolds <- CAST::CreateSpacetimeFolds(traindat_sub,spacevar = "probe_name")

# Dasselbe fuer Var-Daten:
varIDs <- caret::createDataPartition(vardat$probe_name,p=0.001,list = FALSE) # Alle var-Daten so ca 120.000
# Auf ca ~12k vardaten kommen wie bei TopSoil
var_sub <- vardat[varIDs,]

save(traindat_sub,file = "C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model/st_train_dat.Rdata")



