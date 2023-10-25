
setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/model_results/model_with_season")


load("test_set.Rdata")
load("train_set.Rdata")
load("cv_spacetimefolds.Rdata")
load("rf_model_spacetimefolds.Rdata")
load("ffs_model_spacetimefolds.Rdata")


############################################

plot(varImp(ffsmodel))

#rf
plot(rfmodel$pred$pred,rfmodel$pred$obs,ylim=c(-5,30),xlim=c(-5,30))
abline(coef = c(0,1))

#ffs
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

################################################


plot(test_set$date_hour,test_set$soil_temperature,ylab="soil temperature [°C]",xlab="")
points(test_set$date_hour,test_set$prediction,col="red")


#################################################

S03_011 <- test_set[test_set$probe_name == "S03_010", ] 

plot(S03_011$date_hour,S03_011$soil_temperature)

S03_011_sub <- subset(S03_011,
                      date_hour >= as.POSIXct('2022-08-27 00:00') &
                        date_hour <= as.POSIXct('2022-08-29 23:59'))


plot(S03_011_sub$date_hour,S03_011_sub$soil_temperature,col=factor(S03_011$depths),pch=19)


ggplot() + 
  geom_line(S03_011_sub,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_temperature,group=depths, color=depths))+
  geom_line(S03_011_sub,size=1, mapping=aes(x=date_hour, y=prediction,group=depths, color=depths))+
  scale_colour_gradientn(colours=rainbow(4))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil temperature [°C]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))

#######################################################






S03_005 <- test_set[test_set$probe_name == "S03_005", ] 

plot(S03_005$date_hour,S03_005$soil_temperature)

S03_005_sub <- subset(S03_005,
                      date_hour >= as.POSIXct('2022-03-17 00:00') &
                        date_hour <= as.POSIXct('2022-03-18 23:59'))

ggplot() + 
  #geom_point(S03_005_sub, mapping=aes(x=date_hour, y=soil_temperature, color=depths),shape=1)+
  geom_line(S03_005_sub,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_temperature,group=depths, color=depths))+
  #geom_point(S03_005_sub, mapping=aes(x=date_hour, y=prediction, color=depths),shape=19)+
  geom_line(S03_005_sub,size=1, mapping=aes(x=date_hour, y=prediction,group=depths, color=depths))+
  scale_colour_gradientn(colours=rainbow(4))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil temperature [°C]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))
###############################################




S06_008 <- test_set[test_set$probe_name == "S08_008", ] 

plot(S06_008$date_hour,S06_008$soil_temperature)

S06_008_sub <- subset(S06_008,
                      date_hour >= as.POSIXct('2022-06-29 00:00') &
                        date_hour <= as.POSIXct('2022-06-30 23:59'))

ggplot() + 
  #geom_point(S03_005_sub, mapping=aes(x=date_hour, y=soil_temperature, color=depths),shape=1)+
  geom_line(S06_008_sub,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_temperature,group=depths, color=depths))+
  #geom_point(S03_005_sub, mapping=aes(x=date_hour, y=prediction, color=depths),shape=19)+
  geom_line(S06_008_sub,size=1, mapping=aes(x=date_hour, y=prediction,group=depths, color=depths))+
  scale_colour_gradientn(colours=rainbow(4))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil temperature [°C]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))




################################################

time_subset <- subset(test_set,
                      date_hour >= as.POSIXct('2022-03-28 00:00') &
                        date_hour <= as.POSIXct('2022-03-30 23:59'))

time_subset <- subset(test_set,
                      date_hour >= as.POSIXct('2022-06-28 00:00') &
                        date_hour <= as.POSIXct('2022-06-30 23:59'))



plot(time_subset$date_hour,time_subset$soil_temperature)

time_depths_subset <- time_subset[time_subset$depths == 5, ] 

plot(time_depths_subset$date_hour,time_depths_subset$soil_temperature)

plot(time_depths_subset$date_hour,time_depths_subset$prediction)



ggplot() + 
  geom_line(time_depths_subset,linetype = "dotted",size=1, mapping=aes(x=date_hour, y=soil_temperature,group=land_use, color=land_use))+
  geom_line(time_depths_subset,size=1, mapping=aes(x=date_hour, y=prediction,group=land_use, color=land_use))+
  theme(panel.border = element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x=element_text(size=rel(1.2), angle=0,color="black"))+
  theme(axis.text.y=element_text(size=rel(1.2), angle=0,color="black"))+
  xlab("") + 
  ylab("Soil temperature [°C]")
#scale_fill_manual(name = "test", values = c("label" = "grey50"))
