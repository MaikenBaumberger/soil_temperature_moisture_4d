

probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

library(raster)

soil_texture <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Texture_Fichtel_Mountains.grd")
soil_type <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Soil_Type_Fichtel_Mountains.grd")
elevation <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Elevation_Fichtel_Mountains.grd")
land_use <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Land_Use_Fichtel_Mountains.grd")
inclination <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Inclination_Fichtel_Mountains.tif")
exposition <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Exposition_Fichtel_Mountains.tif")
topo_wetness <- raster("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/Topographic_Wettness_Fichtel_Mountains.tif")#calculated from dem using qgis


probes = data.frame(probe_meta_data$probe_id,probe_meta_data$lat,probe_meta_data$lon)

names(probes) = c("probe_id","lat","lon")

probes$coordinates <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)

#probes <- SpatialPointsDataFrame(coords = c(probes[,c("lon", "lat")]),
#                                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),data = probes)


#probes <- SpatialPointsDataFrame(coords = c(probes[,c("lon", "lat")]),
#                                 proj4string = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"),
#                                 data = probes)

soil_texture_2 <- projectRaster(soil_texture,crs = 4326,method = "ngb")
soil_type_2 <- projectRaster(soil_type,crs = 4326,method = "ngb")
elevation_2 <- projectRaster(elevation,crs = 4326,method = "ngb")
land_use_2 <- projectRaster(land_use,crs = 4326,method = "ngb")
inclination_2 <- projectRaster(inclination,crs = 4326,method = "ngb")
exposition_2 <- projectRaster(exposition,crs = 4326,method = "ngb")
topo_wetness_2 <- projectRaster(topo_wetness,crs = 4326,method = "ngb")

hist(soil_texture_2,breaks = 200)
hist(land_use_2,breaks = 200)

mapview::mapview(topo_wetness_2)+mapview::mapview(probes$coordinates)

plot(topo_wetness_2)
plot(probes$coordinates,add = TRUE)


ext_soil_texture = extract(soil_texture_2,probes$coordinates,df=T)
probes$soil_texture = ext_soil_texture$soil_texture

ext_soil_type = extract(soil_type_2,probes$coordinates,df=T)
probes$soil_type = ext_soil_type$soil_type

ext_elevation = extract(elevation_2,probes$coordinates,df=T)
probes$elevation = ext_elevation$X20220324_Gelaendehoehe

ext_land_use = extract(land_use_2,probes$coordinates,df=T)
probes$land_use = ext_land_use$landuse

ext_inclination = extract(inclination_2,probes$coordinates,df=T)
probes$inclination = ext_inclination$Inclination_Fichtel_Mountains

ext_exposition = extract(exposition_2,probes$coordinates,df=T)
probes$exposition = ext_exposition$Exposition_Fichtel_Mountains

ext_topo_wetness = extract(topo_wetness_2,probes$coordinates,df=T)
probes$topo_wetness = ext_topo_wetness$Topographic_Wettness_Fichtel_Mountains

#probes$coordinates = probes$coordinates$geometry

#SpatialPointsDataFrame

probes2 = probes

probes2 <- sf::st_as_sf(probes2$coordinates,coords = c("lon","lat"), crs = 4326)

probes2$topo_wetness = probes$topo_wetness

#names = names(probes)

#probes <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)

#names(probes) = c(names,"geometry")

mapview::mapview(probes2$geometry,zcol="topo_wetness")


probes3 <- SpatialPointsDataFrame(coords = c(probes[,c("lon", "lat")]),proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),data = probes)

mapview::mapview(probes3,zcol="topo_wetness")                                 


