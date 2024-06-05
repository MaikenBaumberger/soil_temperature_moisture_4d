# 
# library("CAST")
# library("caret")
# 
load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons.Rdata")
# 
# 
# #rbind train und test
# #predict
# #prediction pro tiefe eine spalte evt. merge mit datensatz
# #für test prediction daten verwenden
# 
# setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v2")
# load("test_set.Rdata")
# load("train_set.Rdata")
# load("ffs_model.Rdata")
# 
# predictor_set_2 <- subset(predictor_set_2,
#                           date_hour >= as.POSIXct('2022-01-01 00:00') &
#                             date_hour <= as.POSIXct('2022-12-31 23:59'))
# 
# 
# 
# predictor_set_2 = predictor_set_2[complete.cases(predictor_set_2$date_hour),]
# 
# summary(test_set)
# 
# #test_set = rbind(train_set,test_set)
# 
# test_set[,"prediction"]=round(predict.train(object=ffsmodel, newdata = test_set,na.action = na.omit),digits = 2)
# 
# head(test_set)
# head(predictor_set_2)
# 
# predictor_set_2$date_hour = as.POSIXct(strptime(predictor_set_2$date_hour,"%Y-%m-%d %H:%M:%S"))
# 
# prediction_by_depth = predictor_set_2#cbind(predictor_set_2[1:30],predictor_set_2[51:51])
# 
# summary(prediction_by_depth)
# 
# names(predictor_set_2)
# predictor_set_2[duplicated(predictor_set_2), ]
# predictor_set_2 = predictor_set_2[!duplicated(predictor_set_2), ]
# 
# 
# options(digits=3)
# # names(prediction_by_depth)
# # prediction_by_depth[3:28] = round(prediction_by_depth[3:28],2)
# # head(prediction_by_depth)
# # test_set$soil_moisture = round(test_set$soil_moisture,2)
# # test_set$prediction = round(test_set$prediction,2)
# 
# dt_05 <- subset(test_set,depths == 5)
# test_set_05 = data.frame(dt_05[3:3],dt_05[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_05, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_05'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_15 <- subset(test_set,depths == 15)
# test_set_15 = data.frame(dt_15[3:3],dt_15[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_15, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_15'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_25 <- subset(test_set,depths == 25)
# test_set_25 = data.frame(dt_25[3:3],dt_25[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_25, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_25'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_35 <- subset(test_set,depths == 35)
# test_set_35 = data.frame(dt_35[3:3],dt_35[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_35, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_35'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_45 <- subset(test_set,depths == 45)
# test_set_45 = data.frame(dt_45[3:3],dt_45[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_45, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_45'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_55 <- subset(test_set,depths == 55)
# test_set_55 = data.frame(dt_55[3:3],dt_55[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_55, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_55'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_65 <- subset(test_set,depths == 65)
# test_set_65 = data.frame(dt_65[3:3],dt_65[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_65, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_65'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_75 <- subset(test_set,depths == 75)
# test_set_75 = data.frame(dt_75[3:3],dt_75[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_75, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_75'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# dt_85 <- subset(test_set,depths == 85)
# test_set_85 = data.frame(dt_85[3:3],dt_85[55:55])
# prediction_by_depth = merge(prediction_by_depth, test_set_85, by= "id",all.x =TRUE)
# names(prediction_by_depth)[names(prediction_by_depth) == 'prediction'] <- 'prediction_85'
# prediction_by_depth = prediction_by_depth[!duplicated(prediction_by_depth), ]
# 
# 
# head(dt_85[55:55])
# 
# ################################
###################################



#load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons_radolan_4.Rdata")
#load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons.Rdata")

#predictor_set_2$max_temp = pmax(predictor_set_2[17:25])

#head(predictor_set_2)

#max(predictor_set_2[4:12])



###########################################
#version 1 complete original data

df = predictor_set_2
test = predictor_set_2[17:25]

###########################################
#version 2 prediction of test set and train set

# test = prediction_by_depth[62:70]
# names(test) = c("T_05", "T_15", "T_25", "T_35", "T_45", "T_55", "T_65", "T_75", "T_85")
# 
# names(prediction_by_depth)
# df = cbind(prediction_by_depth[1:3],prediction_by_depth[30:70])
# names(df)
# names(df)[names(df) == 'prediction_05'] <- 'T_05'
# names(df)[names(df) == 'prediction_15'] <- 'T_15'
# names(df)[names(df) == 'prediction_25'] <- 'T_25'
# names(df)[names(df) == 'prediction_35'] <- 'T_35'
# names(df)[names(df) == 'prediction_45'] <- 'T_45'
# names(df)[names(df) == 'prediction_55'] <- 'T_55'
# names(df)[names(df) == 'prediction_65'] <- 'T_65'
# names(df)[names(df) == 'prediction_75'] <- 'T_75'
# names(df)[names(df) == 'prediction_85'] <- 'T_85'

############################################
#version 3 original data of test and train set

# df = prediction_by_depth
# 
# df= df[complete.cases(df$prediction_35),]
# test = predictor_set_2[17:25]


############################################


###########################################
###########################################

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set_complete_seasons.Rdata")

predictor_set_2$max_temp = pmax(predictor_set_2[4:12])

head(predictor_set_2)

max(predictor_set_2[4:12])

test = predictor_set_2[4:12]

library(dplyr)

test = test%>% rowwise() %>% mutate(max = max(T_05, T_15, T_25, T_35, T_45, T_55, T_65, T_75, T_85))
test = test%>% rowwise() %>% mutate(min = min(T_05, T_15, T_25, T_35, T_45, T_55, T_65, T_75, T_85))
head(test)
test$range = test$max - test$min

boxplot(test$range)

##################################
#tagesgang

df = df#predictor_set_2

df_max = df %>%
  group_by(probe_name,date) %>%
  summarise(max_05 = max(T_05, na.rm=TRUE))

df_min = df %>%
  group_by(probe_name,date) %>%
  summarise(min_05 = min(T_05, na.rm=TRUE))

head(df_max)
head(df_min)

df_max$min_05 = df_min$min_05

df_max$range = df_max$max_05 - df_max$min_05


boxplot(df_max$range)

######################################
#location space


df2 = df#predictor_set_2

df2_max = (df2 %>%
  group_by(date_hour) %>%
  summarise(max_05 = max(T_05, na.rm=TRUE)))

df2_min = df2 %>%
  group_by(date_hour) %>%
  summarise(min_05 = min(T_05, na.rm=TRUE))

head(df2_max)
head(df2_min)

df2_max$min_05 = df2_min$min_05

df2_max$range = df2_max$max_05 - df2_max$min_05

boxplot(df2_max$range)

############


df2 = df#predictor_set_2

df2_max = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_05 = max(T_05, na.rm=TRUE)))

df2_max$max_15 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_15 = max(T_15, na.rm=TRUE)))[,2]

df2_max$max_25 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_25 = max(T_25, na.rm=TRUE)))[,2]

df2_max$max_35 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_35 = max(T_35, na.rm=TRUE)))[,2]

df2_max$max_45 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_45 = max(T_45, na.rm=TRUE)))[,2]

df2_max$max_55 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_55 = max(T_55, na.rm=TRUE)))[,2]

df2_max$max_65 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_65 = max(T_65, na.rm=TRUE)))[,2]

df2_max$max_75 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_75 = max(T_75, na.rm=TRUE)))[,2]

df2_max$max_85 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_85 = max(T_85, na.rm=TRUE)))[,2]

head(df2_max)

df2_min = df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_05 = min(T_05, na.rm=TRUE))

df2_min$min_15 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_15 = min(T_15, na.rm=TRUE)))[,2]

df2_min$min_25 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_25 = min(T_25, na.rm=TRUE)))[,2]

df2_min$min_35 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_35 = min(T_35, na.rm=TRUE)))[,2]

df2_min$min_45 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_45 = min(T_45, na.rm=TRUE)))[,2]

df2_min$min_55 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_55 = min(T_55, na.rm=TRUE)))[,2]

df2_min$min_65 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_65 = min(T_65, na.rm=TRUE)))[,2]

df2_min$min_75 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_75 = min(T_75, na.rm=TRUE)))[,2]

df2_min$min_85 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_85 = min(T_85, na.rm=TRUE)))[,2]





head(df2_max)
head(df2_min)

df2_range = df2_max[2:10] - df2_min[2:10]

head(df2_range)

range_location = c(df2_range[[1]],df2_range[[2]],df2_range[[3]],df2_range[[4]],df2_range[[5]],
                   df2_range[[6]],df2_range[[7]],df2_range[[8]],df2_range[[9]])



boxplot(range_location)


df2_max$min_05 = df2_min$min_05

df2_max$range = df2_max$max_05 - df2_max$min_05



#########################################
#jahresgang

df3 = df#predictor_set_2

df3_max = df3 %>%
                  group_by(land_use) %>%
                  summarise(max_05 = max(T_05, na.rm=TRUE))

df3_max$max_15 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_15 = max(T_15, na.rm=TRUE)))[,2]

df3_max$max_25 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_25 = max(T_25, na.rm=TRUE)))[,2]

df3_max$max_35 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_35 = max(T_35, na.rm=TRUE)))[,2]

df3_max$max_45 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_45 = max(T_45, na.rm=TRUE)))[,2]

df3_max$max_55 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_55 = max(T_55, na.rm=TRUE)))[,2]

df3_max$max_65 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_65 = max(T_65, na.rm=TRUE)))[,2]

df3_max$max_75 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_75 = max(T_75, na.rm=TRUE)))[,2]

df3_max$max_85 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_85 = max(T_85, na.rm=TRUE)))[,2]




head(df3_max)


df3_min = df3 %>%
            group_by(land_use) %>%
            summarise(min_05 = min(T_05, na.rm=TRUE))

df3_min$min_15 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_15 = min(T_15, na.rm=TRUE)))[,2]

df3_min$min_25 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_25 = min(T_25, na.rm=TRUE)))[,2]

df3_min$min_35 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_35 = min(T_35, na.rm=TRUE)))[,2]

df3_min$min_45 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_45 = min(T_45, na.rm=TRUE)))[,2]

df3_min$min_55 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_55 = min(T_55, na.rm=TRUE)))[,2]

df3_min$min_65 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_65 = min(T_65, na.rm=TRUE)))[,2]

df3_min$min_75 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_75 = min(T_75, na.rm=TRUE)))[,2]

df3_min$min_85 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_85 = min(T_85, na.rm=TRUE)))[,2]


head(df3_max)
head(df3_min)

df3_range = df3_max[2:10] - df3_min[2:10]

range_seasons = c(df3_range[[1]],df3_range[[2]],df3_range[[3]],df3_range[[4]],df3_range[[5]],
                  df3_range[[6]],df3_range[[7]],df3_range[[8]],df3_range[[9]])


boxplot(range_seasons)

range_location_st= range_location
range_seasons_st = range_seasons
range_depths_st = test$range


########################################

boxplot(range_location,range_seasons,test$range, names=c("space","time","depth"),ylim=c(0,40),ylab="Soil Temperature [°C]")


########################################
########################################


###########################################
#version 1 complete original data

df = predictor_set_2
test = predictor_set_2[17:25]

###########################################
#version 2 prediction of test set and train set

# test = prediction_by_depth[62:70]
# names(test) = c("M_05", "M_15", "M_25", "M_35", "M_45", "M_55", "M_65", "M_75", "M_85")
# 
# names(prediction_by_depth)
# df = cbind(prediction_by_depth[1:3],prediction_by_depth[30:70])
# names(df)
# names(df)[names(df) == 'prediction_05'] <- 'M_05'
# names(df)[names(df) == 'prediction_15'] <- 'M_15'
# names(df)[names(df) == 'prediction_25'] <- 'M_25'
# names(df)[names(df) == 'prediction_35'] <- 'M_35'
# names(df)[names(df) == 'prediction_45'] <- 'M_45'
# names(df)[names(df) == 'prediction_55'] <- 'M_55'
# names(df)[names(df) == 'prediction_65'] <- 'M_65'
# names(df)[names(df) == 'prediction_75'] <- 'M_75'
# names(df)[names(df) == 'prediction_85'] <- 'M_85'
# 
# ############################################
#version 3 original data of test and train set

#df = prediction_by_depth

#df= df[complete.cases(df$prediction_35),]
#test = predictor_set_2[17:25]


############################################


library(dplyr)

#test = test%>% rowwise() %>% mutate(max = max(T_05, T_15, T_25, T_35, T_45, T_55, T_65, T_75, T_85))
#test = test%>% rowwise() %>% mutate(min = min(T_05, T_15, T_25, T_35, T_45, T_55, T_65, T_75, T_85))

test = test%>% rowwise() %>% mutate(max = max(M_05, M_15, M_25, M_35, M_45, M_55, M_65, M_75, M_85))
test = test%>% rowwise() %>% mutate(min = min(M_05, M_15, M_25, M_35, M_45, M_55, M_65, M_75, M_85))


head(test)
test$range = test$max - test$min

boxplot(test$range)

##################################
#tagesgang

#df = predictor_set_2

df_max = df %>%
  group_by(probe_name,date) %>%
  summarise(max_05 = max(M_05, na.rm=TRUE))

df_min = df %>%
  group_by(probe_name,date) %>%
  summarise(min_05 = min(M_05, na.rm=TRUE))

head(df_max)
head(df_min)

df_max$min_05 = df_min$min_05

df_max$range = df_max$max_05 - df_max$min_05


boxplot(df_max$range)

######################################
#location space


df2 = df#predictor_set_2

df2_max = (df2 %>%
             group_by(date_hour) %>%
             summarise(max_05 = max(M_05, na.rm=TRUE)))

df2_min = df2 %>%
  group_by(date_hour) %>%
  summarise(min_05 = min(M_05, na.rm=TRUE))

head(df2_max)
head(df2_min)

df2_max$min_05 = df2_min$min_05

df2_max$range = df2_max$max_05 - df2_max$min_05

boxplot(df2_max$range)

############


df2 = df#predictor_set_2

df2_max = (df2 %>%
             group_by(date_hour) %>%
             summarise(max_05 = max(M_05, na.rm=TRUE)))

df2_max$max_15 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_15 = max(M_15, na.rm=TRUE)))[,2]

df2_max$max_25 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_25 = max(M_25, na.rm=TRUE)))[,2]

df2_max$max_35 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_35 = max(M_35, na.rm=TRUE)))[,2]

df2_max$max_45 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_45 = max(M_45, na.rm=TRUE)))[,2]

df2_max$max_55 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_55 = max(M_55, na.rm=TRUE)))[,2]

df2_max$max_65 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_65 = max(M_65, na.rm=TRUE)))[,2]

df2_max$max_75 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_75 = max(M_75, na.rm=TRUE)))[,2]

df2_max$max_85 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(max_85 = max(M_85, na.rm=TRUE)))[,2]

head(df2_max)

df2_min = df2 %>%
  group_by(date_hour) %>%
  summarise(min_05 = min(M_05, na.rm=TRUE))

df2_min$min_15 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_15 = min(M_15, na.rm=TRUE)))[,2]

df2_min$min_25 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_25 = min(M_25, na.rm=TRUE)))[,2]

df2_min$min_35 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_35 = min(M_35, na.rm=TRUE)))[,2]

df2_min$min_45 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_45 = min(M_45, na.rm=TRUE)))[,2]

df2_min$min_55 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_55 = min(M_55, na.rm=TRUE)))[,2]

df2_min$min_65 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_65 = min(M_65, na.rm=TRUE)))[,2]

df2_min$min_75 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_75 = min(M_75, na.rm=TRUE)))[,2]

df2_min$min_85 = (df2 %>%
                    group_by(date_hour) %>%
                    summarise(min_85 = min(M_85, na.rm=TRUE)))[,2]





head(df2_max)
head(df2_min)

df2_range = df2_max[2:10] - df2_min[2:10]

head(df2_range)

range_location = c(df2_range[[1]],df2_range[[2]],df2_range[[3]],df2_range[[4]],df2_range[[5]],
                   df2_range[[6]],df2_range[[7]],df2_range[[8]],df2_range[[9]])



boxplot(range_location)


df2_max$min_05 = df2_min$min_05

df2_max$range = df2_max$max_05 - df2_max$min_05



#########################################
#jahresgang

df3 = df#predictor_set_2

df3_max = df3 %>%
  group_by(land_use) %>%
  summarise(max_05 = max(M_05, na.rm=TRUE))

df3_max$max_15 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_15 = max(M_15, na.rm=TRUE)))[,2]

df3_max$max_25 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_25 = max(M_25, na.rm=TRUE)))[,2]

df3_max$max_35 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_35 = max(M_35, na.rm=TRUE)))[,2]

df3_max$max_45 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_45 = max(M_45, na.rm=TRUE)))[,2]

df3_max$max_55 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_55 = max(M_55, na.rm=TRUE)))[,2]

df3_max$max_65 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_65 = max(M_65, na.rm=TRUE)))[,2]

df3_max$max_75 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_75 = max(M_75, na.rm=TRUE)))[,2]

df3_max$max_85 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(max_85 = max(M_85, na.rm=TRUE)))[,2]




head(df3_max)


df3_min = df3 %>%
  group_by(land_use) %>%
  summarise(min_05 = min(M_05, na.rm=TRUE))

df3_min$min_15 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_15 = min(M_15, na.rm=TRUE)))[,2]

df3_min$min_25 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_25 = min(M_25, na.rm=TRUE)))[,2]

df3_min$min_35 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_35 = min(M_35, na.rm=TRUE)))[,2]

df3_min$min_45 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_45 = min(M_45, na.rm=TRUE)))[,2]

df3_min$min_55 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_55 = min(M_55, na.rm=TRUE)))[,2]

df3_min$min_65 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_65 = min(M_65, na.rm=TRUE)))[,2]

df3_min$min_75 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_75 = min(M_75, na.rm=TRUE)))[,2]

df3_min$min_85 = (df3 %>%
                    group_by(land_use) %>%
                    summarise(min_85 = min(M_85, na.rm=TRUE)))[,2]


head(df3_max)
head(df3_min)

df3_range = df3_max[2:10] - df3_min[2:10]

range_seasons = c(df3_range[[1]],df3_range[[2]],df3_range[[3]],df3_range[[4]],df3_range[[5]],
                  df3_range[[6]],df3_range[[7]],df3_range[[8]],df3_range[[9]])


boxplot(range_seasons)

range_location_sm= range_location
range_seasons_sm = range_seasons
range_depths_sm = test$range


########################################

boxplot(range_location,range_seasons,test$range, names=c("space","time","depth"),ylim=c(0,40),ylab="Soil Moisture [%]")


########################################

pdf("C:/Users/maike/Desktop/Carbon4D/Paper_Soil_Temperature_Moisture_4D/Grafiken/data_variability/data_variability_17.pdf",
    width= 4, 
    height= 3.5)

par(mfrow = c(1,2),mai = c(0.6, 0.6, 0.1, 0.1),mar=c(3,3,1.5,0.5))

boxplot(range_location_st,range_seasons_st,range_depths_st, names=c("space","time","depth"),ylim=c(0,35),ylab="Soil temperature variability [°C]",
        las=1,xaxt="n",ann=F)
axis(side = 1,cex=2,labels = FALSE,at = c(1,2,3))
labs=c("space","time","depths")
text(cex=1, x=c(1,2,3), y=-6, labs, xpd=TRUE, srt=45)
title(ylab = "soil temperature variability [°C]", cex.lab = 1,line = 2)
mtext(cex=1.2,"A",adj=0)

boxplot(range_location_sm,range_seasons_sm,range_depths_sm, names=c("space","time","depth"),ylim=c(0,39),ylab="Soil moisture variability [%]",
        las=1,xaxt="n",ann=F)
axis(side = 1,cex=2,labels = FALSE,at = c(1,2,3))
labs=c("space","time","depths")
text(cex=1, x=c(1,2,3), y=-7, labs, xpd=TRUE, srt=45)
title(ylab = "soil moisture variability [%]", cex.lab = 1,line = 2)
mtext(cex=1.2,"B",adj=0)

dev.off()


