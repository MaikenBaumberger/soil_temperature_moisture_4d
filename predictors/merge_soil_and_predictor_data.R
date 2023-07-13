

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_probes_data/list_probe_data.Rdata")

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

##################################

#create table from list
#write probe_name as a column

for (n in names(list_probes)){
  list_probes[[n]]['probe_name'] = n
}

table_probes = do.call("rbind",list_probes)

rownames(table_probes) <- NULL

table_probes[table_probes < -100] <- NA

##################################

#shifted air temperature

shift <- function(hours){
  a <- rep(NA, hours)
  seq = c(a, at_seq)
  seq = head(seq,-hours)
}

at_seq = meteo_data$air_temperature_mountain

meteo_data$air_temperature_mountain_1 = shift(1)
meteo_data$air_temperature_mountain_2 = shift(2)
meteo_data$air_temperature_mountain_3 = shift(3)
meteo_data$air_temperature_mountain_6 = shift(6)
meteo_data$air_temperature_mountain_12 = shift(12)
meteo_data$air_temperature_mountain_24 = shift(24)
meteo_data$air_temperature_mountain_48 = shift(48)
meteo_data$air_temperature_mountain_72 = shift(72)
meteo_data$air_temperature_mountain_96 = shift(96)
meteo_data$air_temperature_mountain_120 = shift(120)

at_seq = meteo_data$air_temperature_valley

meteo_data$air_temperature_valley_1 = shift(1)
meteo_data$air_temperature_valley_2 = shift(2)
meteo_data$air_temperature_valley_3 = shift(3)
meteo_data$air_temperature_valley_6 = shift(6)
meteo_data$air_temperature_valley_12 = shift(12)
meteo_data$air_temperature_valley_24 = shift(24)
meteo_data$air_temperature_valley_48 = shift(48)
meteo_data$air_temperature_valley_72 = shift(72)
meteo_data$air_temperature_valley_96 = shift(96)
meteo_data$air_temperature_valley_120 = shift(120)



####################################
#merge meteo data and soil data

table_probes$id  <- 1:nrow(table_probes)

predictor_set = merge(table_probes,meteo_data, by.x = "date_hour",by.y = "datetime", all.x = T)

predictor_set = predictor_set[order(predictor_set$id), ]

head(predictor_set)

##################################

#static feature

probe_meta_data_reduced = probe_meta_data[ , -which(names(probe_meta_data) %in% c("start_time","end_time","location","height",
                                                                      "org_layer","excess_length","probe_length",
                                                                      "landuse","time_res","available","geometry"))]

colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "Hoehe_DGM"] ="elevation"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "Landn"] ="land_use"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "Expo"] ="exposition"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "Inkl"] ="inclination"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "BArt"] ="soil_texture"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "BTyp"] ="soil_type"
colnames(probe_meta_data_reduced)[colnames(probe_meta_data_reduced) == "ESC"] ="esc"


#exposition as category
#transforamtion of aspact values

probe_meta_data_reduced$northness <- cos(probe_meta_data_reduced$exposition * pi / 180)
probe_meta_data_reduced$eastness <- sin(probe_meta_data_reduced$exposition * pi / 180)

probe_meta_data_reduced$slope <- probe_meta_data_reduced$inclination


probe_meta_data_reduced = probe_meta_data_reduced[ , -which(names(probe_meta_data_reduced) %in% c("exposition","inclination"))]

predictor_set = merge(predictor_set, probe_meta_data_reduced, by.x = "probe_name",by.y = "probe_id", all.x = T)

predictor_set = predictor_set[order(predictor_set$id), ]

head(predictor_set)

#################################
                                   


#elevation correction of temperature
#temp_mountain - ((temp_mountain-temp_valley)/(height_mountain-height_valley))*(height_mountain-height_location)


plot(predictor_set$date_hour,predictor_set$air_temperature_mountain)


temperature_correction <- function(temp_mountain, temp_valley, elevation_location){
  elevation_mountain = 765 #Pflanzgarten 765 Weidebrunnen 775
  elevation_valley = 620  #Voitsumra
  temp_location = round(temp_mountain-((temp_mountain-temp_valley)/(elevation_mountain-elevation_valley))*(elevation_mountain-elevation_location),2)
}

predictor_set$air_temperature_mountain = temperature_correction(predictor_set$air_temperature_mountain,
                                                                predictor_set$air_temperature_valley,
                                                                predictor_set$elevation) 

predictor_set$air_temperature_mountain_1 = temperature_correction(predictor_set$air_temperature_mountain_1,
                                                                predictor_set$air_temperature_valley_1,
                                                                predictor_set$elevation) 

predictor_set$air_temperature_mountain_2 = temperature_correction(predictor_set$air_temperature_mountain_2,
                                                                  predictor_set$air_temperature_valley_2,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_3 = temperature_correction(predictor_set$air_temperature_mountain_3,
                                                                  predictor_set$air_temperature_valley_3,
                                                                  predictor_set$elevation)

predictor_set$air_temperature_mountain_6 = temperature_correction(predictor_set$air_temperature_mountain_6,
                                                                  predictor_set$air_temperature_valley_6,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_12 = temperature_correction(predictor_set$air_temperature_mountain_12,
                                                                  predictor_set$air_temperature_valley_12,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_24 = temperature_correction(predictor_set$air_temperature_mountain_24,
                                                                  predictor_set$air_temperature_valley_24,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_48 = temperature_correction(predictor_set$air_temperature_mountain_48,
                                                                  predictor_set$air_temperature_valley_48,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_96 = temperature_correction(predictor_set$air_temperature_mountain_96,
                                                                  predictor_set$air_temperature_valley_96,
                                                                  predictor_set$elevation) 

predictor_set$air_temperature_mountain_120 = temperature_correction(predictor_set$air_temperature_mountain_120,
                                                                  predictor_set$air_temperature_valley_120,
                                                                  predictor_set$elevation) 


plot(predictor_set$date_hour,predictor_set$air_temperature_mountain)
plot(predictor_set$date_hour,predictor_set$air_temperature_valley)

hist(predictor_set$elevation,50)

plot(meteo_data$datetime,meteo_data$air_temperature_valley,type="l",col="blue")
lines(meteo_data$datetime,meteo_data$air_temperature_mountain,col="red")


#temperatures voitsumra very low 

predictor_set = predictor_set[ , -which(names(predictor_set) %in% c("air_temperature_valley_1","air_temperature_valley_2","air_temperature_valley_3",
                                                                    "air_temperature_valley_6", "air_temperature_valley_12","air_temperature_valley_24",
                                                                    "air_temperature_valley_48","air_temperature_valley_72","air_temperature_valley_96",
                                                                    "air_temperature_valley_120"))]

###########################




save(predictor_set,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")






