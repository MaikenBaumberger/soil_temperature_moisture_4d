
library(sf)
library(terra)

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/ffs_selection/moisture_15_12")

rastlist <- list.files(path = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/ffs_selection/moisture_15_12", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

allrasters <- lapply(rastlist, rast)

stk1 <- terra::rast(rastlist)

crs(stk1)


plot(stk1$`202201151200_05`)

stk2 = project(stk1, "EPSG:32632")

plot(stk2$`202201151200_05`)

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



cols=viridisLite::viridis(30,direction = -1)#-1
b <- seq(from=0,to=30,by=1)

par(mfrow = c(1, 1),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0),oma=c(0,0,0,0))
plot(stk3[[6]], col = cols, axes=F,legend=T)

#c <- seq(from=10,to=30,by=0.5)
#legend


#par(mfrow = c(9, 12),mar = c(0, 0, 0, 0),mai = c(0, 0, 0, 0))
#for (i in 1:nlyr(stk3)){plot(stk3[[i]], col = cols, breaks = b, axes=F,legend=F)}


############################


#legende 
r1 <- raster::raster(ncols=36, nrows=18)

values(r1) <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))*30
plot(r2,col = cols, breaks = b)

