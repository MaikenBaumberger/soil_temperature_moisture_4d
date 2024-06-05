
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

###########################################



pdf("C:/Users/maike/Desktop/Carbon4D/Paper_Soil_Temperature_Moisture_4D/Grafiken/Prediction_Error/error_4d_26.pdf",
    width= 16, 
    height= 8)


par(mfrow = c(2, 4), mai = c(0.6, 0.6, 0.1, 0.1),mar=c(5,5,2.5,1.5))




#test set

test_set_st[,"prediction"]=round(predict.train(object=rf_model_st, newdata = test_set_st,na.action = na.omit),
                              digits = 2)

test_set_st$difference = abs(test_set_st$soil_temperature-test_set_st$prediction)

mean(test_set_st$difference)

sqrt(mean((test_set_st$soil_temperature-test_set_st$prediction)^2))

rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_set_st$soil_temperature,test_set_st$prediction)


st <- ggplot(test_set_st, aes(x=soil_temperature, y=prediction) ) +
  geom_hex(bins=40) +
  xlim(0, 22) +
  ylim(0, 22) +
  scale_fill_gradient(low = "white", high = "black") +
  geom_abline (slope=1, linetype = "solid", color="black",lwd=0.7)+
  xlab("soil temperature truth [°C]") + 
  ylab("soil temperature prediction [°C]")+
  ggtitle("A") +
  theme(text = element_text(size=16),
        plot.title = element_text(size=22,vjust = -0.9),
        axis.text.x = element_text(size=16,color="black",angle = 45,vjust = 1, hjust=1),
        axis.text.y = element_text(size=16,color="black",vjust = 0.5, hjust=1),
        axis.ticks.length=unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.89, 0.28),
        plot.margin = unit(c(-0.1,0.5,0,0.2), "cm"),
        legend.background=element_blank())


# plot(test_set_st$soil_temperature,test_set_st$prediction,ylim=c(-1,34),xlim=c(-1,34),
#      pch=19,col=alpha("black",0.05),xlab="soil temperature truth [°C]",ylab="soil temperature prediction [°C]",
#      cex.lab=2, cex.axis=2,cex=2,las=1,ann=F,xaxt="n",yaxt="n")
# axis(side = 1,cex=2,cex.axis=2,at=c(0,10,20,30))
# axis(side = 2,cex=2,cex.axis=2,at=c(0,10,20,30))
# abline(coef = c(0,1))
# title(xlab = "soil temperature truth [°C]", cex.lab = 2,line = 4)
# title(ylab = "soil temperature prediction [°C]", cex.lab = 2,line = 3.5)
# mtext(cex=2,"A",adj=0)
# # 



boxplot(test_set_st$difference~test_set_st$land_use,names=c("forest", "arable", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2,las=1,ann=F)
title(xlab = "land use", cex.lab = 2,line = 4)
title(ylab = "error soil temperature [°C]", cex.lab = 2,line = 3.5)
mtext(cex=2,"A",adj=0)

boxplot(test_set_st$difference~test_set_st$land_use,names=c("forest", "arable", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2,las=1,ann=F)
title(xlab = "land use", cex.lab = 2,line = 4)
title(ylab = "error soil temperature [°C]", cex.lab = 2,line = 3.5)
mtext(cex=2,"B",adj=0)


boxplot(test_set_st$difference~test_set_st$depth,ylab="difference (true - pred)",xlab="soil depths",cex.lab=2, 
        cex.axis=2,cex=2,las=1,xaxt = "n",ann=F)
axis(side = 1,cex=2,labels = FALSE,at=c(1,2,3,4,5,6,7,8,9))
labs_depths=c(5,15,25,35,45,55,65,75,85)
text(cex=2, x=c(1,2,3,4,5,6,7,8,9), y=-2.5, labs_depths, xpd=TRUE, srt=45)
title(xlab = "soil depths", cex.lab = 2,line = 4)
title(ylab = "error soil temperature [°C]", cex.lab = 2,line = 3.5)
mtext(cex=2,"C",adj=0)



test_set_st$month = lubridate::month(as.POSIXlt(test_set_st$date_hour, format="%Y-%m-%d %H:%M:%S %Z"))
boxplot(test_set_st$difference~test_set_st$month,ylab="difference (true - pred)",xlab="month",cex.lab=2, cex.axis=2,cex=2,las=1,
        xaxt = "n",ann=F)
labs=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
axis(side = 1,cex=2,labels = FALSE,at = c(1,2,3,4,5,6,7,8,9,10,11,12))
text(cex=2, x=c(1,2,3,4,5,6,7,8,9,10,11,12), y=-2.5, labs, xpd=TRUE, srt=45)
title(xlab = "months", cex.lab = 2,line = 4)
title(ylab = "error soil temperature [°C]", cex.lab = 2,line = 3.5)
mtext(cex=2,"D",adj=0)

#################



test_set_sm[,"prediction"]=round(predict.train(object=rf_model_sm, newdata = test_set_sm,na.action = na.omit),
                                 digits = 2)

test_set_sm$difference = abs(test_set_sm$soil_moisture-test_set_sm$prediction)

mean(test_set_sm$difference)

sqrt(mean((test_set_sm$soil_moisture-test_set_sm$prediction)^2))

rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_set_sm$soil_moisture,test_set_sm$prediction)


sm <- ggplot(test_set_sm, aes(x=soil_moisture, y=prediction) ) +
  geom_hex(bins=40) +
  xlim(0, 30) +
  ylim(0, 30) +
  scale_fill_gradient(low = "white", high = "black") +
  geom_abline (slope=1, linetype = "solid", color="black",lwd=0.7)+
  xlab("soil moisture truth [%]") + 
  ylab("soil moisture prediction [%]")+
  ggtitle("E") +
  theme(text = element_text(size=16),
        plot.title = element_text(size=22,vjust = -0.9),
        axis.text.x = element_text(size=16,color="black",angle = 45,vjust = 1, hjust=1),
        axis.text.y = element_text(size=16,color="black",vjust = 0.5, hjust=1),
        axis.ticks.length=unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.89, 0.28),
        plot.margin = unit(c(-0.1,0.5,0,0.2), "cm"),
        legend.background=element_blank())


#print(sm, vp=vp.bottomleft)

# plot(test_set_sm$soil_moisture,test_set_sm$prediction,ylim=c(-1,38),xlim=c(-1,38),
#      pch=19,col=alpha("black",0.05),xlab="soil moisture truth [%]",ylab="soil moisture prediction [%]",
#      cex.lab=2, cex.axis=2,cex=2,las=1,ann=F)
# abline(coef = c(0,1))
# title(xlab = "soil moisture truth [%]", cex.lab = 2,line = 4)
# title(ylab = "soil moisture prediction [%]", cex.lab = 2,line = 3.5)
# mtext(cex=2,"E",adj=0)

# 



boxplot(test_set_sm$difference~test_set_sm$land_use,names=c("forest", "arable", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2,las=1,ann=F)
title(xlab = "land use", cex.lab = 2,line = 4)
title(ylab = "error soil moisture [%]", cex.lab = 2,line = 3.5)
mtext(cex=2,"E",adj=0)

boxplot(test_set_sm$difference~test_set_sm$land_use,names=c("forest", "arable", "meadow"),
        ylab="difference (true - pred)",xlab="land use",cex.lab=2, cex.axis=2,cex=2,las=1,ann=F)
title(xlab = "land use", cex.lab = 2,line = 4)
title(ylab = "error soil moisture [%]", cex.lab = 2,line = 3.5)
mtext(cex=2,"F",adj=0)


boxplot(test_set_sm$difference~test_set_sm$depth,ylab="difference (true - pred)",xlab="soil depths",cex.lab=2, 
        cex.axis=2,cex=2, xaxt = "n",las=1,ann=F)
axis(side = 1,cex=1,labels = FALSE,at=c(1,2,3,4,5,6,7,8,9))
labs_depths=c(5,15,25,35,45,55,65,75,85)
text(cex=2, x=c(1,2,3,4,5,6,7,8,9), y=-3, labs_depths, xpd=TRUE, srt=45)
title(xlab = "soil depths", cex.lab = 2,line = 4)
title(ylab = "error soil moisture [%]", cex.lab = 2,line = 3.5)
mtext(cex=2,"G",adj=0)


test_set_sm$month = lubridate::month(as.POSIXlt(test_set_sm$date_hour, format="%Y-%m-%d %H:%M:%S %Z"))
boxplot(test_set_sm$difference~test_set_sm$month,ylab="difference (true - pred)",xlab="month",cex.lab=2, cex.axis=2,cex=2,
        xaxt = "n",las=1, ann=F)
labs=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
axis(side = 1,cex=2,labels = FALSE,at = c(1,2,3,4,5,6,7,8,9,10,11,12))
text(cex=2, x=c(1,2,3,4,5,6,7,8,9,10,11,12), y=-3.5, labs, xpd=TRUE, srt=45)
title(xlab = "months", cex.lab = 2,line = 4)
title(ylab = "error soil moisture [%]", cex.lab = 2,line = 3.5)
mtext(cex=2,"H",adj=0)




vp.topleft <- viewport(height=unit(.5, "npc"), width=unit(0.25, "npc"), 
                       just=c("right","bottom"), 
                       y=0.5, x=0.25)
vp.bottomleft <- viewport(height=unit(.5, "npc"), width=unit(0.25, "npc"), 
                          just=c("right","top"), 
                          y=0.5, x=0.25)


# plot the ggplot using the print command
print(st, vp=vp.topleft)

print(sm, vp=vp.bottomleft)


dev.off()

###############################
