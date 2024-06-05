
library(caret)
library(terra)
library(CAST)

#setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d/soil_temperature_moisture_4d/model_results/compare_train_ffs")
#setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v2")
#setwd("C:/Users/maike/Desktop/Carbon4D/Palma/soil_temperature_model_v3")
setwd("C:/Users/maike/Desktop/Carbon4D/Palma/hyperparameter_tuning_2")

load("test_set_st.Rdata")
load("train_set_st.Rdata")
load("test_set_sm.Rdata")
load("train_set_sm.Rdata")
#load("cv_spacetimefolds.Rdata")
load("rfmodel_st.Rdata")
#load("ffs_model.Rdata")

rfmodel
rf_model_st <- rfmodel

load("rfmodel_sm.Rdata")

rfmodel
rf_model_sm <- rfmodel


############################################

plot(rf_model_st$pred$pred,rf_model_st$pred$obs,ylim=c(0,36),xlim=c(0,36))
abline(coef = c(0,1))

plot(rf_model_sm$pred$pred,rf_model_sm$pred$obs,ylim=c(0,36),xlim=c(0,36))
abline(coef = c(0,1))

#ffsmodel

###########################################

par(mfrow = c(2, 4), mai = c(0.6, 0.6, 0.1, 0.1))


#test set

test_set_st[,"prediction"]=round(predict.train(object=rf_model_st, newdata = test_set_st,na.action = na.omit),
                              digits = 2)

test_set_st$difference = abs(test_set_st$soil_temperature-test_set_st$prediction)

mean(test_set_st$difference)

sqrt(mean((test_set_st$soil_temperature-test_set_st$prediction)^2))

rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_set_st$soil_temperature,test_set_st$prediction)


plot(test_set_st$soil_temperature,test_set_st$prediction,ylim=c(-1,34),xlim=c(-1,34),
     pch=19,col=alpha("black",0.05),xlab="soil temperature truth [°C]",ylab="soil temperature prediction [°C]",
     cex.lab=2, cex.axis=2,cex=2)
abline(coef = c(0,1))

# 



boxplot(test_set_st$difference~test_set_st$land_use,names=c("forest", "arable land", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2)

boxplot(test_set_st$difference~test_set_st$depth,ylab="difference (true - pred)",xlab="soil depths",cex.lab=2, cex.axis=2,cex=2)

test_set_st$month = lubridate::month(as.POSIXlt(test_set_st$date_hour, format="%Y-%m-%d %H:%M:%S %Z"))
boxplot(test_set_st$difference~test_set_st$month,ylab="difference (true - pred)",xlab="month",cex.lab=2, cex.axis=2,cex=2)

#################



test_set_sm[,"prediction"]=round(predict.train(object=rf_model_sm, newdata = test_set_sm,na.action = na.omit),
                                 digits = 2)

test_set_sm$difference = abs(test_set_sm$soil_moisture-test_set_sm$prediction)

mean(test_set_sm$difference)

sqrt(mean((test_set_sm$soil_moisture-test_set_sm$prediction)^2))

rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_set_sm$soil_moisture,test_set_sm$prediction)


plot(test_set_sm$soil_moisture,test_set_sm$prediction,ylim=c(-1,38),xlim=c(-1,38),
     pch=19,col=alpha("black",0.05),xlab="soil moisture truth [%]",ylab="soil moisture prediction [%]",
     cex.lab=2, cex.axis=2,cex=2)
abline(coef = c(0,1))

# 



boxplot(test_set_sm$difference~test_set_sm$land_use,names=c("forest", "arable land", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2)

boxplot(test_set_sm$difference~test_set_sm$depth,ylab="difference (true - pred)",xlab="soil depths",cex.lab=2, cex.axis=2,cex=2)

test_set_sm$month = lubridate::month(as.POSIXlt(test_set_sm$date_hour, format="%Y-%m-%d %H:%M:%S %Z"))
boxplot(test_set_sm$difference~test_set_sm$month,ylab="difference (true - pred)",xlab="month",cex.lab=2, cex.axis=2,cex=2)

