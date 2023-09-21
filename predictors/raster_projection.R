

library(raster)
library(zoo)

soil_texture <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Texture_Fichtel_Mountains.grd")
soil_type <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Type_Fichtel_Mountains.grd")
elevation <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Elevation_Fichtel_Mountains.grd")
land_use <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Land_Use_Fichtel_Mountains.grd")
inclination <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Inclination_Fichtel_Mountains.tif")
exposition <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Exposition_Fichtel_Mountains.tif")
topo_wetness <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Topographic_Wettness_Fichtel_Mountains.tif")#calculated from dem using qgis


###########################
#projection


soil_texture_2 <- projectRaster(soil_texture,crs = 4326,method = "ngb")
soil_type_2 <- projectRaster(soil_type,crs = 4326,method = "ngb")
elevation_2 <- projectRaster(elevation,crs = 4326,method = "ngb")
land_use_2 <- projectRaster(land_use,crs = 4326,method = "ngb")
inclination_2 <- projectRaster(inclination,crs = 4326,method = "ngb")
exposition_2 <- projectRaster(exposition,crs = 4326,method = "ngb")
topo_wetness_2 <- projectRaster(topo_wetness,crs = 4326,method = "ngb")

northness <- cos(exposition_2 * pi / 180)
eastness <- sin(exposition_2 * pi / 180)

###########################

northness = resample(northness,elevation_2)
eastness = resample(eastness,elevation_2)
exposition_2 = resample(exposition_2,elevation_2)
inclination_2 = resample(inclination_2,elevation_2)


static_raster= stack(soil_texture_2,soil_type_2,elevation_2,land_use_2,inclination_2,exposition_2,
                     topo_wetness_2,northness,eastness)

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables")
writeRaster(static_raster,"static_raster.tif")


new_raster= stack("static_raster.tif")
names(new_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                      "topo_wetness","northness","eastness")

plot(new_raster[[5]])



mapview::mapview(new_raster[[5]])+mapview::mapview(elevation_2)


