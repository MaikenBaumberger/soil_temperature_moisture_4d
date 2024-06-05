

library(terra)
library(raster)
library(sf)
library(sp)
library(ggplot2)
library(tidyterra)
library(tmap)

static_raster= rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

static_raster2 = project(static_raster, "EPSG:32632",method="near")


#plot(static_raster$land_use,breaks=c(1000,1300,1500,1700,2000), col = rev(heat.colors(5, alpha = 1)))

#2100,2000,1900,1500,1000,1600,2200,1700,1800
#1000,1500,1600,1700,1800,1900,2000,2150,2250

ndwi = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndwi_daily_filled.tif")
ndvi = terra::rast("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/ndvi_ndwi_daily/ndvi_daily_filled.tif")

# 
# par(mfrow=c(2,3))
# par(xpd = FALSE, mar = c(0,0,0,15000),oma = c(0,0,0,0))
# plot(static_raster2$land_use,breaks=c(950,1450,1550,1650,1750,1850,1950,2050,2150,2250), 
#      col = c("darkgreen","gold","chartreuse3","brown","lightblue","grey70","grey50","grey20","blue2"),
#      legend=FALSE,axes = FALSE, main="Land use")
# par(xpd = TRUE)
# legend(714600,5563000,title.adj = 0.5,
#        legend =  c("forest","arable land","meadow","bog","swamp","no vegetation","village","street","water"), 
#        col=  c("darkgreen","gold","chartreuse3","brown","lightblue","grey70","grey50","grey20","blue2"),
#        pch=15)#,title="LAND USE"
# 
# #########################
# 
# 
# #par(xpd = FALSE, mar = c(0,0,0,15000),oma = c(0,0,0,0))
# plot(static_raster2$elevation,axes = FALSE,col = grey.colors(100, start=0, end=1), main="Elevation")
# 
# 
# plot(ndvi$X2022.06.15,axes = FALSE,col = brewer.pal(n = 9, name = "YlGn"), main="NDVI")
# 
# 
# ########################
# 
# #par(xpd = FALSE, mar = c(0,0,0,15000),oma = c(0,0,0,0))
# plot(static_raster2$soil_type,breaks=c(0.5,1.5,2.5,3.5,4.5,5.5), 
#      col = c("brown4","orange","gold","brown","lightblue"),
#      legend=FALSE,axes = FALSE, main="Soil Type")
# par(xpd = TRUE)
# legend(714600,5563000,title.adj = 0.5,
#        legend =  c("organic","semi-terrestrial","waterlogged", "terrestrial","water"), 
#        col = c("brown4","orange","gold","brown","lightblue"),
#        pch=15)#,title="SOIL TYPE"
# 
# 
# ######################
# 
# 
# ######################
# 
# #par(xpd = FALSE, mar = c(0,0,0,15000),oma = c(0,0,0,0))
# plot(static_raster2$topo_wetness,axes = FALSE,col = grey.colors(100, start=0, end=1), main="Topogrphic wettness index")
# 
# 
# plot(ndwi$X2022.06.15,axes = FALSE,col = brewer.pal(n = 9, name = "YlGnBu"), main="NDWI")
# 
# 
##############################

#tmap

#r = static_raster2$land_use
# cls <- data.frame(id=c(1000,1500,1600,1700,1800,1900,2000,2100,2200), 
#                   cover=c("forest","arable land","meadow","bog","swamp",
#                           "no vegetation","village","street","water"))
# levels(r) <- cls
# is.factor(r)


landuse = tm_shape(static_raster2$land_use)+
  tm_raster(style= "cat",palette = c("darkgreen","gold","chartreuse3","brown","blue2",
                                     "grey70","grey50","grey20","lightblue"),
            labels= c("forest","arable land","grassland","bog","swamp",
                      "no vegetation","village","street","water"),
            title="land use")+
  tm_layout(legend.bg.color = "white",
          inner.margins= c(0.0,0.0,0.0,0.0), 
          outer.margins = c(0,0,0,0), 
          legend.bg.alpha = 0.4,
          legend.title.size = 1.7,
          legend.text.size = 1.2,
          legend.position = c("left", "center"),
          frame = FALSE,
          legend.outside = TRUE,
          title="A")
landuse

#####################

# soiltype = tm_shape(static_raster2$soil_type)+
#   tm_raster(style= "cat",
#             labels =  c("organic","semi-terrestrial","stagnic", "terrestrial","water"), 
#             palette = c("palegreen4","orange2","darkslategray","brown","lightblue"),
#             #palette = c("brown4","orange","gold","brown","lightblue"),
#             title="aggregated \nsoil group")+
#   tm_layout(legend.bg.color = "white",
#             inner.margins= c(0.0,0.0,0.0,0.0), 
#             outer.margins = c(0,0,0,0), 
#             legend.bg.alpha = 0.4,
#             legend.title.size = 1.7,
#             legend.text.size = 1.2,
#             legend.position = c("left", "center"),
#             frame = FALSE,
#             legend.outside = TRUE,
#             title="B")
# soiltype


soiltype = tm_shape(static_raster2$soil_type)+
  tm_raster(style= "cat",
            labels =  c("organic","semi-terrestrial","stagnic", "terrestrial","water"), 
            palette = c("brown","gold","orange2","darkgreen","lightblue"),
            #palette = c("brown4","orange","gold","brown","lightblue"),
            title="aggregated \nsoil group")+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.0,0.0,0.0,0.0), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.title.size = 1.7,
            legend.text.size = 1.2,
            legend.position = c("left", "center"),
            frame = FALSE,
            legend.outside = TRUE,
            title="B")
soiltype


######################



ndvi_raster = tm_shape(ndvi$X2022.06.15)+
  tm_raster(style= "cont",title="NDVI",n=10,palette = c("orange2", "yellow2", "darkgreen"),midpoint=0.3)+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.title.size = 1.7,
            legend.text.size = 1.2,
            legend.position = c("left", "center"),
            frame = FALSE,
            legend.outside = TRUE,
            title="E")
ndvi_raster


########################


ndwi_raster = tm_shape(ndwi$X2022.06.15)+
  tm_raster(style= "cont",title="NDWI",n=10,palette = c("orange2", "yellow2", "darkgreen"),midpoint=0)+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.title.size = 1.7,
            legend.text.size = 1.2,
            legend.position = c("left", "center"),
            frame = FALSE,
            legend.outside = TRUE,
            title="F")
ndwi_raster

#YlGnBu

###############################



elevation = tm_shape(static_raster2$elevation)+
  tm_raster(style= "cont",title="elevation [m]",n=10,palette = c("darkgreen", "yellow2", "orange2"))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.0,0.0,0.0,0.0), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.title.size = 1.7,
            legend.text.size = 1.2,
            legend.position = c("left", "center"),
            frame = FALSE,
            legend.outside = TRUE,
            title="C")
elevation


###################################


topo_wetness = tm_shape(static_raster2$topo_wetness)+
  tm_raster(style= "cont",title="TWI",n=10,palette = c("orange2", "yellow2", "darkgreen","darkgreen"))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.0,0.0,0.0,0.0), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.title.size = 1.7,
            legend.text.size = 1.2,
            legend.position = c("left", "center"),
            frame = FALSE,
            legend.outside = TRUE,
            title="D")
topo_wetness

#RdYlBu

#########################

pdf("C:/Users/maike/Desktop/Carbon4D/Paper_Soil_Temperature_Moisture_4D/Grafiken/Predictor_Visualisation/predictors_23.pdf",
    width= 20,
    height= 10)


tmap_arrange(landuse, elevation, ndvi_raster, soiltype, topo_wetness, ndwi_raster, heights  = c(.75))

dev.off()





