
library(sf)
library(terra)


setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/final_model_hyper_AOA/soil_temperature_12_AOA_DI/pred_st_AOA")

#C:\Users\maike\Desktop\Carbon4D\GitHub_soil_temperature_moisture_4d_plots\final_model_hyper\soil_temperature_12_mit_AOA


rastlist <- list.files(path = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/final_model_hyper_AOA/soil_temperature_12_AOA_DI/pred_st_AOA", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

StudyArea <- st_read("C:/Users/maike/Desktop/Carbon4D/Untersuchungsgebiet_Mix/Untersuchungsgebiet/StudyArea.gpkg")

village_mask_base <- rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/final_model_hyper_AOA/soil_moisture_12_AOA_DI/pred_sm_202202151200.tif")

village_mask <- village_mask_base$`202202151200_05`
#plot(village_mask)

village_mask[village_mask > 0] <- 1

village_mask = project(village_mask, "EPSG:32632")
village_mask= crop(village_mask,StudyArea$geom)


allrasters <- lapply(rastlist, rast)

stk1 <- terra::rast(rastlist)

crs(stk1)

#plot(stk1[[1]])

#plot(stk1$`202201151200_05`)

stk2 = project(stk1, "EPSG:32632")

stk2= crop(stk2,StudyArea$geom)

#plot(stk2$`202201151200_05`)



layer_1 = seq(1, 108, by = 9)
layer_2 = seq(2, 108, by = 9)
layer_3 = seq(3, 108, by = 9)
layer_4 = seq(4, 108, by = 9)
layer_5 = seq(5, 108, by = 9)
layer_6 = seq(6, 108, by = 9)
layer_7 = seq(7, 108, by = 9)
layer_8 = seq(8, 108, by = 9)
layer_9 = seq(9, 108, by = 9)


neworder = c(layer_1,layer_2,layer_3,layer_4,layer_5,layer_6,layer_7,layer_8,layer_9)



stk3 = stk2[[neworder]]

#min(stk3)
#max(stk3)



cols=viridisLite::viridis(20,direction = 1)#-1
b <- seq(from=0,to=20,by=1)

par(mfrow = c(1, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0),oma=c(0,0,0,0))
plot(stk3[[1]], col = cols, axes=F,legend=T,colNA="grey75")
par(new=TRUE)
plot(village_mask,colNA="grey35",col="transparent",legend=T,axes=F)


par(mfrow = c(1, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0),oma=c(0,0,0,0))#,cex=2
plot(stk3[[6]], col = cols, axes=F,legend=F,colNA="grey75")
#plot(stk3[[6]], legend.only=TRUE, col = cols, legend.shrink=1, legend.width=2, zlim=c(0, 1),line=0,
#     legend.args=list(text='soil temperature [°C]', side=4, font=2, line=0))
par(new=TRUE)
plot(village_mask,colNA="grey35",col="transparent",legend=F,axes=F)

#1000x1000

#legende 
r1 <- raster::raster(ncols=36, nrows=18)

values(r1) <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))*7+9.5
cols=viridisLite::viridis(20,direction = 1)#-1
b <- seq(from=10,to=16,by=10)
plot(r2,col = cols)



#grey70 gray50
#grey75 grey 45

#test= crop(stk3[[6]],StudyArea$geom)
#plot(test,colNA="grey50")



#par(mfrow = c(1, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0),oma=c(0,0,0,0))
#plot(raster(stk3[[6]]), col = cols, axes=F,legend=T)

#c <- seq(from=10,to=30,by=0.5)
#legend

#prediction

par(mfrow = c(9, 12),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
cols=viridisLite::viridis(20,direction = 1)#-1
b <- seq(from=0,to=20,by=1)
for (i in 1:nlyr(stk3)){
  plot(stk3[[i]], col = cols, breaks = b, axes=F,legend=F,colNA="grey75")
  par(new=T)
  plot(village_mask,colNA="grey35",col="transparent",axes=F,legend=F)
}

#grey70
#grey50


#DI

cols=viridisLite::viridis(25,direction = -1)#-1
b <- seq(from=0,to=5,by=0.2)

par(mfrow = c(9, 12),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
for (i in 1:nlyr(stk3)){plot(stk3[[i]], col = cols, breaks = b, axes=F,legend=F)}

############################


#legende 
r1 <- raster::raster(ncols=36, nrows=18)

values(r1) <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))*20
plot(r2,col = cols, breaks = b)




# 
# stk3 = stk2[[neworder]]
# 
# 
# 
# 
# 
# cols=viridisLite::viridis(20,direction = 1)#-1
# b <- seq(from=0,to=20,by=1)
# 
# par(mfrow = c(1, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0),oma=c(0,0,0,0))
# plot(stk3[[6]], col = cols, axes=F,legend=T)
# 
# #c <- seq(from=10,to=30,by=0.5)
# #legend
# 
# par(mfrow = c(9, 12),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
# for (i in 1:nlyr(stk3)){plot(stk3[[i]], col = cols, breaks = b, axes=F,legend=F)}
# 
# 
# par(mfrow = c(9, 12),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
# 
# stk4 <- stk3*0
# 
# plot(stk4[[1]],col="green")
# 
# for (i in 1:nlyr(stk4)){plot(stk4[[i]], col = cols, breaks = b, axes=F,legend=F)}
# 
# #########
# 
# static_raster= rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
# names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
#                          "topo_wetness","northness","eastness")
# 
# elevation_pro = project(static_raster$inclination, "EPSG:32632")
# plot(elevation_pro,col = grey.colors(100, start=0, end=1))
# 
# mapview::mapview(raster(elevation_pro))
# 
# ##############
# 
# 
# 
# 
# ##############
# ############
