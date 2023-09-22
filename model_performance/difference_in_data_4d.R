

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

df = predictor_set_2

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


df2 = predictor_set_2

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


df2 = predictor_set_2

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

df3 = predictor_set_2

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


########################################

boxplot(range_location,range_seasons,test$range, names=c("space","time","depth"),ylim=c(0,30),ylab="Soil Temperature [°C]")

########################################

