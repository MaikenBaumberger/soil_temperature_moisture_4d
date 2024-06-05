
library(caret)
library(terra)
library(CAST)

#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d/soil_temperature_moisture_4d/model_results/compare_train_ffs")
#setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v2")
#setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v3")
setwd("C:/Users/maike/Desktop/Carbon4D/Palma/hyperparameter_tuning_2")

load("test_set_st.Rdata")
load("train_set_st.Rdata")
#load("cv_spacetimefolds.Rdata")
load("rfmodel_st.Rdata")
#load("ffs_model.Rdata")

rfmodel
ffsmodel <- rfmodel

test_set <- test_set_st

varImp(ffsmodel)
plot(varImp(ffsmodel))

plot(varImp(rfmodel))
############################################

#rf
#plot(rfmodel$pred$pred,rfmodel$pred$obs,ylim=c(-5,30),xlim=c(-5,30))
#abline(coef = c(0,1))

ffs
plot(ffsmodel$pred$pred,ffsmodel$pred$obs,ylim=c(0,36),xlim=c(0,36))
abline(coef = c(0,1))

#ffsmodel

###########################################
#test set

test_set[,"prediction"]=round(predict.train(object=ffsmodel, newdata = test_set,na.action = na.omit),
                              digits = 2)
#head(test_set)

test_set$difference = abs(test_set$soil_temperature-test_set$prediction)

mean(test_set$difference)

sqrt(mean((test_set$soil_temperature-test_set$prediction)^2))

rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_set$soil_temperature,test_set$prediction)


plot(test_set$soil_temperature,test_set$prediction,ylim=c(-1,34),xlim=c(-1,34),
     pch=19,col=alpha("black",0.05),xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
abline(coef = c(0,1))

# 
# pal = colorRampPalette(c("blue", "red"))
# test_set$order = findInterval(test_set$trend_3month, sort(test_set$trend_3month))
# plot(test_set$soil_temperature,test_set$prediction,ylim=c(-1,34),xlim=c(-1,34),
#      pch=19,col=alpha(pal(nrow(test_set))[test_set$order],0.03),
#      xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
# abline(coef = c(0,1))
# legend("topleft", col=pal(2), pch=19,title ="trend 3 month",
#        legend=c(round(range(test_set$trend_3month), 3)))
# 
# pal = colorRampPalette(c("blue", "red"))
# test_set$order = findInterval(test_set$prec_sum_4_month, sort(test_set$prec_sum_4_month))
# plot(test_set$soil_temperature,test_set$prediction,ylim=c(-1,34),xlim=c(-1,34),
#      pch=19,col=alpha(pal(nrow(test_set))[test_set$order],0.03),
#      xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
# abline(coef = c(0,1))
# legend("topleft", col=pal(2), pch=19,title ="precipitation sum 4 month",
#        legend=c(round(range(test_set$prec_sum_4_month), 3)))
# 
# 
# colors <- c("#66BD63", # Darker green
#             "#FDAE61", # Orange
#             "#D9EF8B") # Light green
#              
# plot(test_set$soil_temperature,test_set$prediction,ylim=c(-1,34),xlim=c(-1,34),
#      pch=19,col = alpha(colors[factor(test_set$land_use)],0.03),
#      xlab="soil temperature truth [%]",ylab="soil temperature prediction [%]")
# abline(coef = c(0,1))
# legend("topleft",
#        legend = c("Forest", "Arable land", "Meadow"),
#        pch = 19,
#        col = colors)

#############################################


boxplot(test_set$difference~test_set$land_use,names=c("forest", "arable land", "meadow"),
        ylab="difference (true - pred)",xlab="land use")

boxplot(test_set$difference~test_set$depth,ylab="difference (true - pred)",xlab="soil depths")

test_set$month = lubridate::month(as.POSIXlt(test_set$date_hour, format="%Y-%m-%d %H:%M:%S %Z"))
boxplot(test_set$difference~test_set$month,ylab="difference (true - pred)",xlab="month")

#boxplot(test_set$difference~test_set$probe_name,ylab="difference (true - pred)",xlab="location")






############################################

###############################################
rsq <- function (x, y) cor(x, y) ^ 2

rsq(test_set$soil_temperature,test_set$prediction)

land_use_meadow <- test_set[test_set$land_use == 1600, ] 
land_use_arable <- test_set[test_set$land_use == 1500, ] 
land_use_forest <- test_set[test_set$land_use == 1000, ] 

rsq_meadow = rsq(land_use_meadow$soil_temperature,land_use_meadow$prediction)
rsq_arable = rsq(land_use_arable$soil_temperature,land_use_arable$prediction)
rsq_forest = rsq(land_use_forest$soil_temperature,land_use_forest$prediction)


rsq_landuse = c(rsq_meadow,rsq_arable,rsq_forest)
num = c(1,2,3)

plot(num,rsq_landuse,pch=19,ylab="R²",xlab="land use",xaxt="n",xlim = c(0.5,3.5))
axis(side=1,at=c(1,2,3),labels=c("meadow", "arable land", "forest"))

plot(land_use_meadow$soil_temperature,land_use_meadow$prediction,ylim=c(0,40),xlim=c(0,40))
abline(coef = c(0,1))

plot(land_use_arable$soil_temperature,land_use_arable$prediction,ylim=c(0,40),xlim=c(0,40))
abline(coef = c(0,1))

plot(land_use_forest$soil_temperature,land_use_forest$prediction,ylim=c(0,40),xlim=c(0,40))
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

rsq_05 = rsq(depths_05$soil_temperature,depths_05$prediction)
rsq_15 = rsq(depths_15$soil_temperature,depths_15$prediction)
rsq_25 = rsq(depths_25$soil_temperature,depths_25$prediction)
rsq_35 = rsq(depths_35$soil_temperature,depths_35$prediction)
rsq_45 = rsq(depths_45$soil_temperature,depths_45$prediction)
rsq_55 = rsq(depths_55$soil_temperature,depths_55$prediction)
rsq_65 = rsq(depths_65$soil_temperature,depths_65$prediction)
rsq_75 = rsq(depths_75$soil_temperature,depths_75$prediction)
rsq_85 = rsq(depths_85$soil_temperature,depths_85$prediction)

rsq_depth = c(rsq_05,rsq_15,rsq_25,rsq_35,rsq_45,rsq_55,rsq_65,rsq_75,rsq_85)
depth = c(5,15,25,35,45,55,65,75,85)

plot(depth,rsq_depth,pch=19,ylab="R²",xlab="soil depth",xaxt="n")
axis(side=1,at=c(5,15,25,35,45,55,65,75,85))

plot(depths_25$soil_temperature,depths_25$prediction,ylim=c(0,40),xlim=c(0,40),pch=19,)
abline(coef = c(0,1))

plot(depths_85$soil_temperature,depths_85$prediction,ylim=c(0,40),xlim=c(0,40),pch=19,)
abline(coef = c(0,1))


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

rsq_1 = rsq(month_1$soil_temperature,month_1$prediction)
rsq_2 = rsq(month_2$soil_temperature,month_2$prediction)
rsq_3 = rsq(month_3$soil_temperature,month_3$prediction)
rsq_4 = rsq(month_4$soil_temperature,month_4$prediction)
rsq_5 = rsq(month_5$soil_temperature,month_5$prediction)
rsq_6 = rsq(month_6$soil_temperature,month_6$prediction)
rsq_7 = rsq(month_7$soil_temperature,month_7$prediction)
rsq_8 = rsq(month_8$soil_temperature,month_8$prediction)
rsq_9 = rsq(month_9$soil_temperature,month_9$prediction)
rsq_10 = rsq(month_10$soil_temperature,month_10$prediction)
rsq_11 = rsq(month_11$soil_temperature,month_11$prediction)
rsq_12 = rsq(month_12$soil_temperature,month_12$prediction)

rsq_month = c(rsq_1,rsq_2,rsq_3,rsq_4,rsq_5,rsq_6,rsq_7,rsq_8,rsq_9,rsq_10,rsq_11,rsq_12)
month = c(1,2,3,4,5,6,7,8,9,10,11,12)

plot(month,rsq_month,pch=19,ylab="R²",xaxt="n")
axis(side=1,at=c(1,2,3,4,5,6,7,8,9,10,11,12))

plot(month_2$soil_temperature,month_2$prediction,ylim=c(0,40),xlim=c(0,40))
abline(coef = c(0,1))


























#plot(test_set$air_temperature_month,test_set$difference)


###########################################

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

S03_011 <- test_set[test_set$probe_name == "S03_011", ] 

plot(S03_011$date_hour,S03_011$soil_temperature)

S03_011_sub <- subset(S03_011,
             date_hour >= as.POSIXct('2022-09-28 00:00') &
               date_hour <= as.POSIXct('2022-09-30 23:59'))


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
                      date_hour >= as.POSIXct('2022-03-28 00:00') &
                        date_hour <= as.POSIXct('2022-03-30 23:59'))

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




S06_008 <- test_set[test_set$probe_name == "S06_008", ] 

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
