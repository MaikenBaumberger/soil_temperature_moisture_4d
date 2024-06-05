
library(raster)
library(terra)

#st_pred_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_temperature_prediction_v2"
st_pred_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/ffs_selection/moisture_15_12"
st_pred_files <- list.files(st_pred_folder,pattern=".tif$", full.names=TRUE)

static_raster= terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

plot(static_raster$soil_type)

land_use <- static_raster$land_use

forest_mask <- land_use == 1000
meadow_mask <- land_use == 1600
arable_mask <- land_use == 1500

plot(forest_mask)
plot(meadow_mask)
plot(arable_mask)

#forest_mask_bin <- as.logical(forest_mask)


st_pred = terra::rast(st_pred_files)
names(st_pred)

st_pred_05 = st_pred[[c(seq(1,3544,8))]]
st_pred_15 = st_pred[[c(seq(2,3544,8))]]
st_pred_25 = st_pred[[c(seq(3,3544,8))]]
st_pred_35 = st_pred[[c(seq(4,3544,8))]]
st_pred_45 = st_pred[[c(seq(5,3544,8))]]
st_pred_55 = st_pred[[c(seq(6,3544,8))]]
st_pred_65 = st_pred[[c(seq(7,3544,8))]]
st_pred_75 = st_pred[[c(seq(8,3544,8))]]

#für jede tiefe


# spat_mean_05 <- mean(st_pred_05,na.rm=T)#
# st_mean_05_forest <- spat_mean_05
# st_mean_05_forest[!forest_mask] <- NA
# spat_mean_05_forest_v = mean(values(st_mean_05_forest),na.rm=T)

#plot(st_mean_05_forest)

spat_mean_05 = mean(st_pred_05,na.rm=T)#
spat_sd_05 = stdev(st_pred_05,na.rm=T)

spat_mean_05_v = mean(values(spat_mean_05),na.rm=T)
spat_sd_05_v = mean(values(spat_sd_05),na.rm=T)


spat_mean_15 = mean(st_pred_15,na.rm=T)#
spat_sd_15 = stdev(st_pred_15,na.rm=T)

spat_mean_15_v = mean(values(spat_mean_15),na.rm=T)
spat_sd_15_v = mean(values(spat_sd_15),na.rm=T)


spat_mean_25 = mean(st_pred_25,na.rm=T)#
spat_sd_25 = stdev(st_pred_25,na.rm=T)

spat_mean_25_v = mean(values(spat_mean_25),na.rm=T)
spat_sd_25_v = mean(values(spat_sd_25),na.rm=T)


spat_mean_35 = mean(st_pred_35,na.rm=T)#
spat_sd_35 = stdev(st_pred_35,na.rm=T)

spat_mean_35_v = mean(values(spat_mean_35),na.rm=T)
spat_sd_35_v = mean(values(spat_sd_35),na.rm=T)


spat_mean_45 = mean(st_pred_45,na.rm=T)#
spat_sd_45 = stdev(st_pred_45,na.rm=T)

spat_mean_45_v = mean(values(spat_mean_45),na.rm=T)
spat_sd_45_v = mean(values(spat_sd_45),na.rm=T)


spat_mean_55 = mean(st_pred_55,na.rm=T)#
spat_sd_55 = stdev(st_pred_55,na.rm=T)

spat_mean_55_v = mean(values(spat_mean_55),na.rm=T)
spat_sd_55_v = mean(values(spat_sd_55),na.rm=T)


spat_mean_65 = mean(st_pred_65,na.rm=T)#
spat_sd_65 = stdev(st_pred_65,na.rm=T)

spat_mean_65_v = mean(values(spat_mean_65),na.rm=T)
spat_sd_65_v = mean(values(spat_sd_65),na.rm=T)



spat_mean_75 = mean(st_pred_75,na.rm=T)#
spat_sd_75 = stdev(st_pred_75,na.rm=T)

spat_mean_75_v = mean(values(spat_mean_75),na.rm=T)
spat_sd_75_v = mean(values(spat_sd_75),na.rm=T)


###################################

st_mean_05_forest <- spat_mean_05
st_mean_05_meadow <- spat_mean_05
st_mean_05_arable <- spat_mean_05
st_sd_05_forest <- spat_sd_05
st_sd_05_meadow <- spat_sd_05
st_sd_05_arable <- spat_sd_05

st_mean_05_forest[!forest_mask] <- NA
st_sd_05_forest[!forest_mask] <- NA
st_mean_05_meadow[!meadow_mask] <- NA
st_sd_05_meadow[!meadow_mask] <- NA
st_mean_05_arable[!arable_mask] <- NA
st_sd_05_arable[!arable_mask] <- NA

spat_mean_05_forest_v = mean(values(st_mean_05_forest),na.rm=T)
spat_sd_05_forest_v = mean(values(st_sd_05_forest),na.rm=T)
spat_mean_05_meadow_v = mean(values(st_mean_05_meadow),na.rm=T)
spat_sd_05_meadow_v = mean(values(st_sd_05_meadow),na.rm=T)
spat_mean_05_arable_v = mean(values(st_mean_05_arable),na.rm=T)
spat_sd_05_arable_v = mean(values(st_sd_05_arable),na.rm=T)

#############

st_mean_15_forest <- spat_mean_15
st_mean_15_meadow <- spat_mean_15
st_mean_15_arable <- spat_mean_15
st_sd_15_forest <- spat_sd_15
st_sd_15_meadow <- spat_sd_15
st_sd_15_arable <- spat_sd_15

st_mean_15_forest[!forest_mask] <- NA
st_sd_15_forest[!forest_mask] <- NA
st_mean_15_meadow[!meadow_mask] <- NA
st_sd_15_meadow[!meadow_mask] <- NA
st_mean_15_arable[!arable_mask] <- NA
st_sd_15_arable[!arable_mask] <- NA

spat_mean_15_forest_v = mean(values(st_mean_15_forest),na.rm=T)
spat_sd_15_forest_v = mean(values(st_sd_15_forest),na.rm=T)
spat_mean_15_meadow_v = mean(values(st_mean_15_meadow),na.rm=T)
spat_sd_15_meadow_v = mean(values(st_sd_15_meadow),na.rm=T)
spat_mean_15_arable_v = mean(values(st_mean_15_arable),na.rm=T)
spat_sd_15_arable_v = mean(values(st_sd_15_arable),na.rm=T)

###############

st_mean_25_forest <- spat_mean_25
st_mean_25_meadow <- spat_mean_25
st_mean_25_arable <- spat_mean_25
st_sd_25_forest <- spat_sd_25
st_sd_25_meadow <- spat_sd_25
st_sd_25_arable <- spat_sd_25

st_mean_25_forest[!forest_mask] <- NA
st_sd_25_forest[!forest_mask] <- NA
st_mean_25_meadow[!meadow_mask] <- NA
st_sd_25_meadow[!meadow_mask] <- NA
st_mean_25_arable[!arable_mask] <- NA
st_sd_25_arable[!arable_mask] <- NA

spat_mean_25_forest_v = mean(values(st_mean_25_forest),na.rm=T)
spat_sd_25_forest_v = mean(values(st_sd_25_forest),na.rm=T)
spat_mean_25_meadow_v = mean(values(st_mean_25_meadow),na.rm=T)
spat_sd_25_meadow_v = mean(values(st_sd_25_meadow),na.rm=T)
spat_mean_25_arable_v = mean(values(st_mean_25_arable),na.rm=T)
spat_sd_25_arable_v = mean(values(st_sd_25_arable),na.rm=T)

##################

st_mean_35_forest <- spat_mean_35
st_mean_35_meadow <- spat_mean_35
st_mean_35_arable <- spat_mean_35
st_sd_35_forest <- spat_sd_35
st_sd_35_meadow <- spat_sd_35
st_sd_35_arable <- spat_sd_35

st_mean_35_forest[!forest_mask] <- NA
st_sd_35_forest[!forest_mask] <- NA
st_mean_35_meadow[!meadow_mask] <- NA
st_sd_35_meadow[!meadow_mask] <- NA
st_mean_35_arable[!arable_mask] <- NA
st_sd_35_arable[arable_mask] <- NA

spat_mean_35_forest_v = mean(values(st_mean_35_forest),na.rm=T)
spat_sd_35_forest_v = mean(values(st_sd_35_forest),na.rm=T)
spat_mean_35_meadow_v = mean(values(st_mean_35_meadow),na.rm=T)
spat_sd_35_meadow_v = mean(values(st_sd_35_meadow),na.rm=T)
spat_mean_35_arable_v = mean(values(st_mean_35_arable),na.rm=T)
spat_sd_35_arable_v = mean(values(st_sd_35_arable),na.rm=T)

########################

st_mean_45_forest <- spat_mean_45
st_mean_45_meadow <- spat_mean_45
st_mean_45_arable <- spat_mean_45
st_sd_45_forest <- spat_sd_45
st_sd_45_meadow <- spat_sd_45
st_sd_45_arable <- spat_sd_45

st_mean_45_forest[!forest_mask] <- NA
st_sd_45_forest[!forest_mask] <- NA
st_mean_45_meadow[!meadow_mask] <- NA
st_sd_45_meadow[!meadow_mask] <- NA
st_mean_45_arable[!arable_mask] <- NA
st_sd_45_arable[!arable_mask] <- NA

spat_mean_45_forest_v = mean(values(st_mean_45_forest),na.rm=T)
spat_sd_45_forest_v = mean(values(st_sd_45_forest),na.rm=T)
spat_mean_45_meadow_v = mean(values(st_mean_45_meadow),na.rm=T)
spat_sd_45_meadow_v = mean(values(st_sd_45_meadow),na.rm=T)
spat_mean_45_arable_v = mean(values(st_mean_45_arable),na.rm=T)
spat_sd_45_arable_v = mean(values(st_sd_45_arable),na.rm=T)

###########################


st_mean_55_forest <- spat_mean_55
st_mean_55_meadow <- spat_mean_55
st_mean_55_arable <- spat_mean_55
st_sd_55_forest <- spat_sd_55
st_sd_55_meadow <- spat_sd_55
st_sd_55_arable <- spat_sd_55

st_mean_55_forest[!forest_mask] <- NA
st_sd_55_forest[!forest_mask] <- NA
st_mean_55_meadow[!meadow_mask] <- NA
st_sd_55_meadow[!meadow_mask] <- NA
st_mean_55_arable[!arable_mask] <- NA
st_sd_55_arable[!arable_mask] <- NA

spat_mean_55_forest_v = mean(values(st_mean_55_forest),na.rm=T)
spat_sd_55_forest_v = mean(values(st_sd_55_forest),na.rm=T)
spat_mean_55_meadow_v = mean(values(st_mean_55_meadow),na.rm=T)
spat_sd_55_meadow_v = mean(values(st_sd_55_meadow),na.rm=T)
spat_mean_55_arable_v = mean(values(st_mean_55_arable),na.rm=T)
spat_sd_55_arable_v = mean(values(st_sd_55_arable),na.rm=T)

#################################


st_mean_65_forest <- spat_mean_65
st_mean_65_meadow <- spat_mean_65
st_mean_65_arable <- spat_mean_65
st_sd_65_forest <- spat_sd_65
st_sd_65_meadow <- spat_sd_65
st_sd_65_arable <- spat_sd_65

st_mean_65_forest[!forest_mask] <- NA
st_sd_65_forest[!forest_mask] <- NA
st_mean_65_meadow[!meadow_mask] <- NA
st_sd_65_meadow[!meadow_mask] <- NA
st_mean_65_arable[!arable_mask] <- NA
st_sd_65_arable[!arable_mask] <- NA

spat_mean_65_forest_v = mean(values(st_mean_65_forest),na.rm=T)
spat_sd_65_forest_v = mean(values(st_sd_65_forest),na.rm=T)
spat_mean_65_meadow_v = mean(values(st_mean_65_meadow),na.rm=T)
spat_sd_65_meadow_v = mean(values(st_sd_65_meadow),na.rm=T)
spat_mean_65_arable_v = mean(values(st_mean_65_arable),na.rm=T)
spat_sd_65_arable_v = mean(values(st_sd_65_arable),na.rm=T)

#############################


st_mean_75_forest <- spat_mean_75
st_mean_75_meadow <- spat_mean_75
st_mean_75_arable <- spat_mean_75
st_sd_75_forest <- spat_sd_75
st_sd_75_meadow <- spat_sd_75
st_sd_75_arable <- spat_sd_75

st_mean_75_forest[!forest_mask] <- NA
st_sd_75_forest[!forest_mask] <- NA
st_mean_75_meadow[!meadow_mask] <- NA
st_sd_75_meadow[!meadow_mask] <- NA
st_mean_75_arable[!arable_mask] <- NA
st_sd_75_arable[!arable_mask] <- NA

spat_mean_75_forest_v = mean(values(st_mean_75_forest),na.rm=T)
spat_sd_75_forest_v = mean(values(st_sd_75_forest),na.rm=T)
spat_mean_75_meadow_v = mean(values(st_mean_75_meadow),na.rm=T)
spat_sd_75_meadow_v = mean(values(st_sd_75_meadow),na.rm=T)
spat_mean_75_arable_v = mean(values(st_mean_75_arable),na.rm=T)
spat_sd_75_arable_v = mean(values(st_sd_75_arable),na.rm=T)

############################



mean = c(spat_mean_05_forest_v,spat_mean_05_meadow_v,spat_mean_05_arable_v,
         spat_mean_15_forest_v,spat_mean_15_meadow_v,spat_mean_15_arable_v,
         spat_mean_25_forest_v,spat_mean_25_meadow_v,spat_mean_25_arable_v,
         spat_mean_35_forest_v,spat_mean_35_meadow_v,spat_mean_35_arable_v,
         spat_mean_45_forest_v,spat_mean_45_meadow_v,spat_mean_45_arable_v,
         spat_mean_55_forest_v,spat_mean_55_meadow_v,spat_mean_55_arable_v,
         spat_mean_65_forest_v,spat_mean_65_meadow_v,spat_mean_65_arable_v,
         spat_mean_75_forest_v,spat_mean_75_meadow_v,spat_mean_75_arable_v)

sd= c(spat_sd_05_forest_v,spat_sd_05_meadow_v,spat_sd_05_arable_v,
      spat_sd_15_forest_v,spat_sd_15_meadow_v,spat_sd_15_arable_v,
      spat_sd_25_forest_v,spat_sd_25_meadow_v,spat_sd_25_arable_v,
      spat_sd_35_forest_v,spat_sd_35_meadow_v,spat_sd_35_arable_v,
      spat_sd_45_forest_v,spat_sd_45_meadow_v,spat_sd_45_arable_v,
      spat_sd_55_forest_v,spat_sd_55_meadow_v,spat_sd_55_arable_v,
      spat_sd_65_forest_v,spat_sd_65_meadow_v,spat_sd_65_arable_v,
      spat_sd_75_forest_v,spat_sd_75_meadow_v,spat_sd_75_arable_v)
      
      
depth = c(-3,-5,-7,-13,-15,-17,-23,-25,-27,-33,-35,-37,-43,-45,-47,-53,-55,-57,-63,-65,-67,-73,-75,-77)

landuse_cat = c("forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land")


dat = data.frame(depth,mean,sd,landuse_cat)

library(ggplot2)
ggplot(dat, aes(x = mean, y = depth, color=landuse_cat)) +
  geom_point(size=2)+
  #geom_ribbon(aes(xmin = mean - sd,
  #                xmax = mean + sd), alpha = 0.2)
  geom_errorbar(aes(xmin=mean-sd, xmax=mean+sd), width=1.2, 
                position=position_dodge(0.05),size=1)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14))+
  xlab("Soil moisture [%]")+
  scale_y_continuous(breaks = seq(-5, -75, by = -10))+
  scale_color_manual(values = c("forest" = "#238A8DFF", "meadow" = "#73D055FF", "arable_land" = "#FDE725FF"))+
  xlim(10, 24)
  theme(legend.position = c(0.12,0.8),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))








# 
# mean = c(spat_mean_05_v,spat_mean_15_v,spat_mean_25_v,spat_mean_35_v,spat_mean_45_v)
# sd= c(spat_sd_05_v,spat_sd_15_v,spat_sd_25_v,spat_sd_35_v,spat_sd_45_v) 
# depth = c(-5,-15,-25,-35,-45)
# 
# dat = data.frame(depth,mean,sd)
# 
# 
# library(ggplot2)
# ggplot(dat, aes(x = mean, y = depth)) +
#   geom_point() +
#   #geom_ribbon(aes(xmin = mean - sd,
#   #                xmax = mean + sd), alpha = 0.2)
#   geom_errorbar(aes(xmin=mean-sd, xmax=mean+sd), width=.2, 
#                 position=position_dodge(0.05)) 



#################################







st_pred_jan = st_pred[[substr(names(st_pred), 5,6)=="01"]]
st_pred_feb = st_pred[[substr(names(st_pred), 5,6)=="02"]]
st_pred_mar = st_pred[[substr(names(st_pred), 5,6)=="03"]]
st_pred_apr = st_pred[[substr(names(st_pred), 5,6)=="04"]]
st_pred_may = st_pred[[substr(names(st_pred), 5,6)=="05"]]
st_pred_jun = st_pred[[substr(names(st_pred), 5,6)=="06"]]
st_pred_jul = st_pred[[substr(names(st_pred), 5,6)=="07"]]
st_pred_aug = st_pred[[substr(names(st_pred), 5,6)=="08"]]
st_pred_sep = st_pred[[substr(names(st_pred), 5,6)=="09"]]
st_pred_oct = st_pred[[substr(names(st_pred), 5,6)=="10"]]
st_pred_nov = st_pred[[substr(names(st_pred), 5,6)=="11"]]
st_pred_dez = st_pred[[substr(names(st_pred), 5,6)=="12"]]





mean_jan <- mean(st_pred_jan,na.rm=T)
sd_jan <- stdev(st_pred_jan,na.rm=T)

mean_feb <- mean(st_pred_feb,na.rm=T)
sd_feb <- stdev(st_pred_feb,na.rm=T)

mean_mar <- mean(st_pred_mar,na.rm=T)
sd_mar <- stdev(st_pred_mar,na.rm=T)

mean_apr <- mean(st_pred_apr,na.rm=T)
sd_apr <- stdev(st_pred_apr,na.rm=T)

mean_may <- mean(st_pred_may,na.rm=T)
sd_may <- stdev(st_pred_may,na.rm=T)

mean_jun <- mean(st_pred_jun,na.rm=T)
sd_jun <- stdev(st_pred_jun,na.rm=T)

mean_jul <- mean(st_pred_jul,na.rm=T)#
sd_jul <- stdev(st_pred_jul,na.rm=T)#

mean_aug <- mean(st_pred_aug,na.rm=T)#
sd_aug <- stdev(st_pred_aug,na.rm=T)#

mean_sep <- mean(st_pred_sep,na.rm=T)#
sd_sep <- stdev(st_pred_sep,na.rm=T)#

mean_oct <- mean(st_pred_oct,na.rm=T)#
sd_oct <- stdev(st_pred_oct,na.rm=T)#

mean_nov <- mean(st_pred_nov,na.rm=T)#
sd_nov <- stdev(st_pred_nov,na.rm=T)#

mean_dez <- mean(st_pred_dez,na.rm=T)#
sd_dez <- stdev(st_pred_dez,na.rm=T)#

##########

mean_jan_forest <- mean_jan
mean_jan_forest[!forest_mask] <- NA
mean_jan_forest_v = mean(values(mean_jan_forest),na.rm=T)

sd_jan_forest <- sd_jan
sd_jan_forest[!forest_mask] <- NA
sd_jan_forest_v = mean(values(sd_jan_forest),na.rm=T)

mean_jan_meadow <- mean_jan
mean_jan_meadow[!meadow_mask] <- NA
mean_jan_meadow_v = mean(values(mean_jan_meadow),na.rm=T)

sd_jan_meadow <- sd_jan
sd_jan_meadow[!meadow_mask] <- NA
sd_jan_meadow_v = mean(values(sd_jan_meadow),na.rm=T)

mean_jan_arable <- mean_jan
mean_jan_arable[!arable_mask] <- NA
mean_jan_arable_v = mean(values(mean_jan_arable),na.rm=T)

sd_jan_arable <- sd_jan
sd_jan_arable[!arable_mask] <- NA
sd_jan_arable_v = mean(values(sd_jan_arable),na.rm=T)

##########

mean_feb_forest <- mean_feb
mean_feb_forest[!forest_mask] <- NA
mean_feb_forest_v = mean(values(mean_feb_forest),na.rm=T)

sd_feb_forest <- sd_feb
sd_feb_forest[!forest_mask] <- NA
sd_feb_forest_v = mean(values(sd_feb_forest),na.rm=T)

mean_feb_meadow <- mean_feb
mean_feb_meadow[!meadow_mask] <- NA
mean_feb_meadow_v = mean(values(mean_feb_meadow),na.rm=T)

sd_feb_meadow <- sd_feb
sd_feb_meadow[!meadow_mask] <- NA
sd_feb_meadow_v = mean(values(sd_feb_meadow),na.rm=T)

mean_feb_arable <- mean_feb
mean_feb_arable[!arable_mask] <- NA
mean_feb_arable_v = mean(values(mean_feb_arable),na.rm=T)

sd_feb_arable <- sd_feb
sd_feb_arable[!arable_mask] <- NA
sd_feb_arable_v = mean(values(sd_feb_arable),na.rm=T)

##########

mean_mar_forest <- mean_mar
mean_mar_forest[!forest_mask] <- NA
mean_mar_forest_v = mean(values(mean_mar_forest),na.rm=T)

sd_mar_forest <- sd_mar
sd_mar_forest[!forest_mask] <- NA
sd_mar_forest_v = mean(values(sd_mar_forest),na.rm=T)

mean_mar_meadow <- mean_mar
mean_mar_meadow[!meadow_mask] <- NA
mean_mar_meadow_v = mean(values(mean_mar_meadow),na.rm=T)

sd_mar_meadow <- sd_mar
sd_mar_meadow[!meadow_mask] <- NA
sd_mar_meadow_v = mean(values(sd_mar_meadow),na.rm=T)

mean_mar_arable <- mean_mar
mean_mar_arable[!arable_mask] <- NA
mean_mar_arable_v = mean(values(mean_mar_arable),na.rm=T)

sd_mar_arable <- sd_mar
sd_mar_arable[!arable_mask] <- NA
sd_mar_arable_v = mean(values(sd_mar_arable),na.rm=T)

##########
mean_apr_forest <- mean_apr
mean_apr_forest[!forest_mask] <- NA
mean_apr_forest_v = mean(values(mean_apr_forest),na.rm=T)

sd_apr_forest <- sd_apr
sd_apr_forest[!forest_mask] <- NA
sd_apr_forest_v = mean(values(sd_apr_forest),na.rm=T)

mean_apr_meadow <- mean_apr
mean_apr_meadow[!meadow_mask] <- NA
mean_apr_meadow_v = mean(values(mean_apr_meadow),na.rm=T)

sd_apr_meadow <- sd_apr
sd_apr_meadow[!meadow_mask] <- NA
sd_apr_meadow_v = mean(values(sd_apr_meadow),na.rm=T)

mean_apr_arable <- mean_apr
mean_apr_arable[!arable_mask] <- NA
mean_apr_arable_v = mean(values(mean_apr_arable),na.rm=T)

sd_apr_arable <- sd_apr
sd_apr_arable[!arable_mask] <- NA
sd_apr_arable_v = mean(values(sd_apr_arable),na.rm=T)

##########
mean_may_forest <- mean_may
mean_may_forest[!forest_mask] <- NA
mean_may_forest_v = mean(values(mean_may_forest),na.rm=T)

sd_may_forest <- sd_may
sd_may_forest[!forest_mask] <- NA
sd_may_forest_v = mean(values(sd_may_forest),na.rm=T)

mean_may_meadow <- mean_may
mean_may_meadow[!meadow_mask] <- NA
mean_may_meadow_v = mean(values(mean_may_meadow),na.rm=T)

sd_may_meadow <- sd_may
sd_may_meadow[!meadow_mask] <- NA
sd_may_meadow_v = mean(values(sd_may_meadow),na.rm=T)

mean_may_arable <- mean_may
mean_may_arable[!arable_mask] <- NA
mean_may_arable_v = mean(values(mean_may_arable),na.rm=T)

sd_may_arable <- sd_may
sd_may_arable[!arable_mask] <- NA
sd_may_arable_v = mean(values(sd_may_arable),na.rm=T)

##########
mean_jun_forest <- mean_jun
mean_jun_forest[!forest_mask] <- NA
mean_jun_forest_v = mean(values(mean_jun_forest),na.rm=T)

sd_jun_forest <- sd_jun
sd_jun_forest[!forest_mask] <- NA
sd_jun_forest_v = mean(values(sd_jun_forest),na.rm=T)

mean_jun_meadow <- mean_jun
mean_jun_meadow[!meadow_mask] <- NA
mean_jun_meadow_v = mean(values(mean_jun_meadow),na.rm=T)

sd_jun_meadow <- sd_jun
sd_jun_meadow[!meadow_mask] <- NA
sd_jun_meadow_v = mean(values(sd_jun_meadow),na.rm=T)

mean_jun_arable <- mean_jun
mean_jun_arable[!arable_mask] <- NA
mean_jun_arable_v = mean(values(mean_jun_arable),na.rm=T)

sd_jun_arable <- sd_jun
sd_jun_arable[!arable_mask] <- NA
sd_jun_arable_v = mean(values(sd_jun_arable),na.rm=T)

##########
mean_jul_forest <- mean_jul
mean_jul_forest[!forest_mask] <- NA
mean_jul_forest_v = mean(values(mean_jul_forest),na.rm=T)

sd_jul_forest <- sd_jul
sd_jul_forest[!forest_mask] <- NA
sd_jul_forest_v = mean(values(sd_jul_forest),na.rm=T)

mean_jul_meadow <- mean_jul
mean_jul_meadow[!meadow_mask] <- NA
mean_jul_meadow_v = mean(values(mean_jul_meadow),na.rm=T)

sd_jul_meadow <- sd_jul
sd_jul_meadow[!meadow_mask] <- NA
sd_jul_meadow_v = mean(values(sd_jul_meadow),na.rm=T)

mean_jul_arable <- mean_jul
mean_jul_arable[!arable_mask] <- NA
mean_jul_arable_v = mean(values(mean_jul_arable),na.rm=T)

sd_jul_arable <- sd_jul
sd_jul_arable[!arable_mask] <- NA
sd_jul_arable_v = mean(values(sd_jul_arable),na.rm=T)

##########
mean_aug_forest <- mean_aug
mean_aug_forest[!forest_mask] <- NA
mean_aug_forest_v = mean(values(mean_aug_forest),na.rm=T)

sd_aug_forest <- sd_aug
sd_aug_forest[!forest_mask] <- NA
sd_aug_forest_v = mean(values(sd_aug_forest),na.rm=T)

mean_aug_meadow <- mean_aug
mean_aug_meadow[!meadow_mask] <- NA
mean_aug_meadow_v = mean(values(mean_aug_meadow),na.rm=T)

sd_aug_meadow <- sd_aug
sd_aug_meadow[!meadow_mask] <- NA
sd_aug_meadow_v = mean(values(sd_aug_meadow),na.rm=T)

mean_aug_arable <- mean_aug
mean_aug_arable[!arable_mask] <- NA
mean_aug_arable_v = mean(values(mean_aug_arable),na.rm=T)

sd_aug_arable <- sd_aug
sd_aug_arable[!arable_mask] <- NA
sd_aug_arable_v = mean(values(sd_aug_arable),na.rm=T)

##########
mean_sep_forest <- mean_sep
mean_sep_forest[!forest_mask] <- NA
mean_sep_forest_v = mean(values(mean_sep_forest),na.rm=T)

sd_sep_forest <- sd_sep
sd_sep_forest[!forest_mask] <- NA
sd_sep_forest_v = mean(values(sd_sep_forest),na.rm=T)

mean_sep_meadow <- mean_sep
mean_sep_meadow[!meadow_mask] <- NA
mean_sep_meadow_v = mean(values(mean_sep_meadow),na.rm=T)

sd_sep_meadow <- sd_sep
sd_sep_meadow[!meadow_mask] <- NA
sd_sep_meadow_v = mean(values(sd_sep_meadow),na.rm=T)

mean_sep_arable <- mean_sep
mean_sep_arable[!arable_mask] <- NA
mean_sep_arable_v = mean(values(mean_sep_arable),na.rm=T)

sd_sep_arable <- sd_sep
sd_sep_arable[!arable_mask] <- NA
sd_sep_arable_v = mean(values(sd_sep_arable),na.rm=T)

##########


mean_oct_forest <- mean_oct
mean_oct_forest[!forest_mask] <- NA
mean_oct_forest_v = mean(values(mean_oct_forest),na.rm=T)

sd_oct_forest <- sd_oct
sd_oct_forest[!forest_mask] <- NA
sd_oct_forest_v = mean(values(sd_oct_forest),na.rm=T)

mean_oct_meadow <- mean_oct
mean_oct_meadow[!meadow_mask] <- NA
mean_oct_meadow_v = mean(values(mean_oct_meadow),na.rm=T)

sd_oct_meadow <- sd_oct
sd_oct_meadow[!meadow_mask] <- NA
sd_oct_meadow_v = mean(values(sd_oct_meadow),na.rm=T)

mean_oct_arable <- mean_oct
mean_oct_arable[!arable_mask] <- NA
mean_oct_arable_v = mean(values(mean_oct_arable),na.rm=T)

sd_oct_arable <- sd_oct
sd_oct_arable[!arable_mask] <- NA
sd_oct_arable_v = mean(values(sd_oct_arable),na.rm=T)

##########
mean_nov_forest <- mean_nov
mean_nov_forest[!forest_mask] <- NA
mean_nov_forest_v = mean(values(mean_nov_forest),na.rm=T)

sd_nov_forest <- sd_nov
sd_nov_forest[!forest_mask] <- NA
sd_nov_forest_v = mean(values(sd_nov_forest),na.rm=T)

mean_nov_meadow <- mean_nov
mean_nov_meadow[!meadow_mask] <- NA
mean_nov_meadow_v = mean(values(mean_nov_meadow),na.rm=T)

sd_nov_meadow <- sd_nov
sd_nov_meadow[!meadow_mask] <- NA
sd_nov_meadow_v = mean(values(sd_nov_meadow),na.rm=T)

mean_nov_arable <- mean_nov
mean_nov_arable[!arable_mask] <- NA
mean_nov_arable_v = mean(values(mean_nov_arable),na.rm=T)

sd_nov_arable <- sd_nov
sd_nov_arable[!arable_mask] <- NA
sd_nov_arable_v = mean(values(sd_nov_arable),na.rm=T)

##########
mean_dez_forest <- mean_dez
mean_dez_forest[!forest_mask] <- NA
mean_dez_forest_v = mean(values(mean_dez_forest),na.rm=T)

sd_dez_forest <- sd_dez
sd_dez_forest[!forest_mask] <- NA
sd_dez_forest_v = mean(values(sd_dez_forest),na.rm=T)

mean_dez_meadow <- mean_dez
mean_dez_meadow[!meadow_mask] <- NA
mean_dez_meadow_v = mean(values(mean_dez_meadow),na.rm=T)

sd_dez_meadow <- sd_dez
sd_dez_meadow[!meadow_mask] <- NA
sd_dez_meadow_v = mean(values(sd_dez_meadow),na.rm=T)

mean_dez_arable <- mean_dez
mean_dez_arable[!arable_mask] <- NA
mean_dez_arable_v = mean(values(mean_dez_arable),na.rm=T)

sd_dez_arable <- sd_dez
sd_dez_arable[!arable_mask] <- NA
sd_dez_arable_v = mean(values(sd_dez_arable),na.rm=T)

##########





mean = c(mean_jan_forest_v,mean_jan_meadow_v,mean_jan_arable_v,
         mean_feb_forest_v,mean_feb_meadow_v,mean_feb_arable_v,
         mean_mar_forest_v,mean_mar_meadow_v,mean_mar_arable_v,
         mean_apr_forest_v,mean_apr_meadow_v,mean_apr_arable_v,
         mean_may_forest_v,mean_may_meadow_v,mean_may_arable_v,
         mean_jun_forest_v,mean_jun_meadow_v,mean_jun_arable_v,
         mean_jul_forest_v,mean_jul_meadow_v,mean_jul_arable_v,
         mean_aug_forest_v,mean_aug_meadow_v,mean_aug_arable_v,
         mean_sep_forest_v,mean_sep_meadow_v,mean_sep_arable_v,
         mean_oct_forest_v,mean_oct_meadow_v,mean_oct_arable_v,
         mean_nov_forest_v,mean_nov_meadow_v,mean_nov_arable_v,
         mean_dez_forest_v,mean_dez_meadow_v,mean_dez_arable_v)

sd= c(sd_jan_forest_v,sd_jan_meadow_v,sd_jan_arable_v,
      sd_feb_forest_v,sd_feb_meadow_v,sd_feb_arable_v,
      sd_mar_forest_v,sd_mar_meadow_v,sd_mar_arable_v,
      sd_apr_forest_v,sd_apr_meadow_v,sd_apr_arable_v,
      sd_may_forest_v,sd_may_meadow_v,sd_may_arable_v,
      sd_jun_forest_v,sd_jun_meadow_v,sd_jun_arable_v,
      sd_jul_forest_v,sd_jul_meadow_v,sd_jul_arable_v,
      sd_aug_forest_v,sd_aug_meadow_v,sd_aug_arable_v,
      sd_sep_forest_v,sd_sep_meadow_v,sd_sep_arable_v,
      sd_oct_forest_v,sd_oct_meadow_v,sd_oct_arable_v,
      sd_nov_forest_v,sd_nov_meadow_v,sd_nov_arable_v,
      sd_dez_forest_v,sd_dez_meadow_v,sd_dez_arable_v)


month = c(0.8,1,1.2,1.8,2,2.2,2.8,3,3.2,3.8,4,4.2,4.8,5,5.2,5.8,6,6.2,6.8,7,7.2,7.8,8,8.2,8.8,9,9.2,9.8,10,10.2,10.8,11,11.2,11.8,12,12.2)

landuse_cat = c("forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land",
                "forest","meadow","arable_land")


dat2 = data.frame(month,mean,sd,landuse_cat)


library(ggplot2)
ggplot(dat2, aes(x = month, y = mean, color=landuse_cat)) +
  geom_point(size=3)+
  #geom_ribbon(aes(xmin = mean - sd,
  #                xmax = mean + sd), alpha = 0.2)
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, 
                position=position_dodge(0.05),size=1)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14))+
  ylab("Soil moisture [%]")+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
  theme(legend.position = c(0.12,10.8),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))+
  scale_color_manual(values = c("forest" = "#238A8DFF", "meadow" = "#73D055FF", "arable_land" = "#FDE725FF"))
  

  
  # theme(panel.border = element_blank(),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.line = element_line(size = 0.5, linetype = "solid",
  #                                colour = "black"),
  #       panel.background = element_rect(fill = "white",
  #                                       colour = "white",
  #                                       size = 0.5, linetype = "solid"))
  # 


var_depth_sm <-dat
var_time_sm <-dat2
setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/4d_variability_landuse")
saveRDS(var_depth_sm, "var_depth_sm.rds")
saveRDS(var_time_sm, "var_time_sm.rds")


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #create data
# x<-1:100
# mean_force<-0.5*x+rnorm(100)
# #assume constant standard deviation across the 
# sd<-5
# #determine error band
# psd<-mean_force+sd
# nsd<-mean_force-sd
# 
# plot(x, mean_force, ty="l", col="blue", 
#      ylab="force (mN)", 
#      xlab='time (ms)',
#      lty=1,lwd=3)
# #draw boundary and fill
# lines(x, psd)
# lines(x, nsd)
# polygon(x=c(x, rev(x)), y=c(psd, rev(nsd)), col="lightblue", density = 40, angle=90)
# #redraw line on top
# lines(x, mean_force, col="blue",lwd=3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #################################
# 
# 
# 
# 
# 
# spat_min_05 = min(st_pred_05,na.rm=T)
# spat_max_05 = max(st_pred_05,na.rm=T)
# spat_range_05 = spat_max_05-spat_min_05
# 
# spat_mean_75 = mean(st_pred_75,na.rm=T)
# spat_min_75 = min(st_pred_75,na.rm=T)
# spat_max_75 = max(st_pred_75,na.rm=T)
# spat_range_75 = spat_max_75-spat_min_75
# 
# ##################################
# 
# cols=viridisLite::viridis(14,direction = 1)
# b_mean <- seq(from=6.5,to=9.3,by=0.2)
# b_min <- seq(from=5,to=35,by=1)
# 
# cols=viridisLite::viridis(19,direction = 1)
# b_max <- seq(from=13,to=32,by=1)
# 
# cols=viridisLite::viridis(20,direction = 1)
# b_range <- seq(from=12,to=32,by=1)
# 
# plot(spat_mean_05,col = cols, breaks = b_mean)
# plot(spat_min_05)
# plot(spat_max_05,col = cols, breaks = b_max)
# plot(spat_range_05,col = cols, breaks = b_range)
# 
# min(spat_range_05)
# 
# ###################################
# 
# cols=viridisLite::viridis(14,direction = 1)
# b_mean <- seq(from=6.5,to=9.3,by=0.2)
# b_min <- seq(from=5,to=35,by=1)
# plot(spat_mean_75,col = cols, breaks = b_mean)
# 
# 
# 
# cols=viridisLite::viridis(21,direction = 1)
# b_max <- seq(from=11,to=32,by=1)
# 
# cols=viridisLite::viridis(20,direction = 1)
# b_range <- seq(from=12,to=32,by=1)
# 
# 
# 
# plot(spat_min_75)
# plot(spat_max_75,col = cols, breaks = b_max)
# plot(spat_range_75,col = cols, breaks = b_range)
# 
# min(spat_range_05)
# 
# ##################################
# 
# 
# 
# 
# 
# mean_05= c()
# var_05= c()
# sd_05= c()
# min_05= c()
# max_05= c()
# 
# 
# for(i in 1:443){
#   st_pred_subset = st_pred_05[[i]][]
#   mean = mean(st_pred_subset,na.rm=T)
#   mean_05 = c(mean_05,mean)
#   var = var(st_pred_subset,na.rm=T)
#   var_05 = c(var_05,var)
#   sd = sd(st_pred_subset,na.rm=T)
#   sd_05 = c(sd_05,sd)
#   min = min(st_pred_subset,na.rm=T)
#   min_05 = c(min_05,min)
#   max = max(st_pred_subset,na.rm=T)
#   max_05 = c(max_05,max)
# }
# 
# statistc_05 = data.frame(names(st_pred_05))
# statistc_05$datetime = substr(names(st_pred_05), 1,12)
# statistc_05$datetime = as.POSIXct(strptime(statistc_05$datetime,"%Y%m%d%H%M"))
# 
# statistc_05$mean = mean_05
# statistc_05$var = var_05
# statistc_05$sd = sd_05
# statistc_05$min = min_05
# statistc_05$max = max_05
# statistc_05$range = statistc_05$max - statistc_05$min
# 
# plot(statistc_05$datetime,statistc_05$mean)
# plot(statistc_05$datetime,statistc_05$var)
# plot(statistc_05$datetime,statistc_05$sd)
# plot(statistc_05$datetime,statistc_05$min)
# plot(statistc_05$datetime,statistc_05$max)
# plot(statistc_05$datetime,statistc_05$range)
# 
# 
# mean_75= c()
# var_75= c()
# sd_75= c()
# min_75= c()
# max_75= c()
# 
# for(i in 1:443){
#   st_pred_subset = st_pred_75[[i]][]
#   mean = mean(st_pred_subset,na.rm=T)
#   mean_75 = c(mean_75,mean)
#   var = var(st_pred_subset,na.rm=T)
#   var_75 = c(var_75,var)
#   sd = sd(st_pred_subset,na.rm=T)
#   sd_75 = c(sd_75,sd)
#   min = min(st_pred_subset,na.rm=T)
#   min_75 = c(min_75,min)
#   max = max(st_pred_subset,na.rm=T)
#   max_75 = c(max_75,max)
# }
# 
# 
# statistc_75 = data.frame(names(st_pred_75))
# statistc_75$datetime = substr(names(st_pred_75), 1,12)
# statistc_75$datetime = as.POSIXct(strptime(statistc_75$datetime,"%Y%m%d%H%M"))
# 
# statistc_75$mean = mean_75
# statistc_75$var = var_75
# statistc_75$sd = sd_75
# statistc_75$min = min_75
# statistc_75$max = max_75
# statistc_75$range = statistc_75$max - statistc_75$min
# 
# plot(statistc_75$datetime,statistc_75$mean,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$mean,col="red")
# plot(statistc_75$datetime,statistc_75$var,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$var,col="red")
# plot(statistc_75$datetime,statistc_75$sd,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$sd,col="red")
# plot(statistc_75$datetime,statistc_75$min,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$min,col="red")
# plot(statistc_75$datetime,statistc_75$max,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$max,col="red")
# plot(statistc_75$datetime,statistc_75$range,ylim=c(0,20))
# points(statistc_05$datetime,statistc_05$range,col="red")
# 
# 
# 
# 
# 
# 
