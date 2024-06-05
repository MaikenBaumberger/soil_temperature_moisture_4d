library(raster)
library(terra)

st_pred_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/ffs_selection/temperature_15_12"
sm_pred_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_plots/ffs_selection/moisture_15_12"
st_pred_files <- list.files(st_pred_folder,pattern=".tif$", full.names=TRUE)
sm_pred_files <- list.files(sm_pred_folder,pattern=".tif$", full.names=TRUE)
st_pred = terra::rast(st_pred_files)
sm_pred = terra::rast(sm_pred_files)

static_raster= terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")


ndvi = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndvi_daily_filled.tif")

ndvi_test = ndvi[[196]]

st_test <- st_pred[[55]]

sm_test <- sm_pred[[55]]

#plot(values(st_test),values(static_raster$elevation))

set.seed(42)

# Generate random coordinates for 100 points
random_coords <- cbind(runif(1000, xmin(st_test), xmax(st_test)), runif(100, ymin(st_test), ymax(st_test)))

# Extract values from both rasters at the random sample locations
values_raster1 <- extract(st_test, random_coords)
values_raster2 <- extract(static_raster$elevation, random_coords)
values_raster3 <- extract(static_raster$land_use, random_coords)
values_raster4 <- extract(static_raster$inclination, random_coords)
values_raster5 <- extract(static_raster$soil_type, random_coords)
values_raster6 <- extract(static_raster$exposition, random_coords)
values_raster7 <- extract(static_raster$topo_wetness, random_coords)
values_raster8 <- extract(ndvi_test$X2022.07.15, random_coords)
values_raster9 <- extract(sm_test, random_coords)


plot(values_raster2$elevation,values_raster1$`202207151200_05`,col = factor(values_raster3$land_use))

plot(values_raster4$inclination,values_raster1$`202207151200_05`,col = factor(values_raster3$land_use))

plot(st_test)

plot(values_raster7$topo_wetness,values_raster1$`202207151200_05`,col = factor(values_raster3$land_use))

plot(values_raster8$X2022.07.15,values_raster1$`202207151200_05`,col = factor(values_raster3$land_use))

plot(values_raster8$X2022.07.15,values_raster9$`202207151200_05`,col = factor(values_raster3$land_use))

plot(values_raster7$topo_wetness,values_raster9$`202207151200_05`,col = factor(values_raster3$land_use))

plot(values_raster4$inclination,values_raster9$`202207151200_05`,col = factor(values_raster3$land_use))


plot(static_raster$topo_wetness)




