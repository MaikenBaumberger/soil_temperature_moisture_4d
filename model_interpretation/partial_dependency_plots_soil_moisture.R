# library(raster)
# library(terra)
# library(CAST)
# library(caret)

#https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots

library(pdp)
library(ggplot2)

setwd("C:/Users/maike/Desktop/Carbon4D/Palma/hyperparameter_tuning_2")

load("test_set_sm.Rdata")
load("train_set_sm.Rdata")
load("rfmodel_sm.Rdata")


set.seed(12)
test_set_sm_sub <- dplyr::sample_n(test_set_sm, 1000)#test_set_st[1:50,]


pred <- predict(rfmodel, test_set_sm_sub, probability = TRUE)

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/partail_dependency/pdp_soil_moisture_1000")

# par_depths_depths <- partial(rfmodel, pred.var = c("depths"), chull = TRUE)
# saveRDS(par_depths_depths,file= "par_depths_depths.rds")
# 
# par_at3m_depths <- partial(rfmodel, pred.var = c("air_temperature_3_month","depths"), chull = TRUE)
# saveRDS(par_at3m_depths,file= "par_at3m_depths.rds")
# 
# par_landuse_depths <- partial(rfmodel, pred.var = c("land_use","depths"), cats="land_use")
# saveRDS(par_landuse_depths,file= "par_landuse_depths.rds")
# 
# par_twi_depths <- partial(rfmodel, pred.var = c("topo_wetness","depths"), chull = TRUE)
# saveRDS(par_twi_depths,file= "par_twi_depths.rds")
# 
# par_inclination_depths <- partial(rfmodel, pred.var = c("inclination","depths"), chull = TRUE)
# saveRDS(par_inclination_depths,file= "par_inclination_depths.rds")
# 
# par_prec3m_depths <- partial(rfmodel, pred.var = c("prec_sum_3_month","depths"), chull = TRUE)
# saveRDS(par_prec3m_depths,file= "par_prec3m_depths.rds")
# 
# par_texture_depths <- partial(rfmodel, pred.var = c("soil_texture","depths"), cats="soil_texture")
# saveRDS(par_texture_depths,file= "par_texture_depths.rds")
# #
# par_ndvi_depths <- partial(rfmodel, pred.var = c("ndvi","depths"), chull = TRUE)
# saveRDS(par_ndvi_depths,file= "par_ndvi_depths.rds")
# 
# par_type_depths <- partial(rfmodel, pred.var = c("soil_type","depths"), cats="soil_type")
# saveRDS(par_type_depths,file= "par_type_depths.rds")
# 
# par_at72h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_72","depths"), chull = TRUE)
# saveRDS(par_at72h_depths,file= "par_at72h_depths.rds")
# 
# par_radolansum0_24_depths <- partial(rfmodel, pred.var = c("radolan_sum_0_24","depths"), chull = TRUE)
# saveRDS(par_radolansum0_24_depths,file= "par_radolansum0_24_depths.rds")
# 
# par_radolansum0_6_depths <- partial(rfmodel, pred.var = c("radolan_sum_0_6","depths"), chull = TRUE)
# saveRDS(par_radolansum0_6_depths,file= "par_radolansum0_6_depths.rds")
# 
# par_radolansum18_24_depths <- partial(rfmodel, pred.var = c("radolan_sum_18_24","depths"), chull = TRUE)
# saveRDS(par_radolansum18_24_depths,file= "par_radolansum18_24_depths.rds")
# 
# par_radolansum12_18_depths <- partial(rfmodel, pred.var = c("radolan_sum_12_18","depths"), chull = TRUE)
# saveRDS(par_radolansum12_18_depths,file= "par_radolansum12_18_depths.rds")
# 
# par_rad_depths <- partial(rfmodel, pred.var = c("global_radiation","depths"), chull = TRUE)
# saveRDS(par_rad_depths,file= "par_rad_depths.rds")
# 
# par_prec_depths <- partial(rfmodel, pred.var = c("precipitation","depths"), chull = TRUE)
# saveRDS(par_prec_depths,file= "par_prec_depths.rds")
# 
# par_radolan_depths <- partial(rfmodel, pred.var = c("radolan","depths"), chull = TRUE)
# saveRDS(par_radolan_depths,file= "par_radolan_depths.rds")
# 



#par_landuse_depths <- partial(rfmodel, pred.var = c("land_use","depths"), cats="land_use")


####################################################################


par_depths_depths <- readRDS(file = "par_depths_depths.rds")
par_at3m_depths <- readRDS(file = "par_at3m_depths.rds")
par_landuse_depths <- readRDS(file = "par_landuse_depths.rds")
par_twi_depths <- readRDS(file = "par_twi_depths.rds")
par_inclination_depths <- readRDS(file = "par_inclination_depths.rds")
par_prec3m_depths <- readRDS(file = "par_prec3m_depths.rds")
par_texture_depths <- readRDS(file = "par_texture_depths.rds")
par_ndvi_depths <- readRDS(file = "par_ndvi_depths.rds")
par_type_depths <- readRDS(file = "par_type_depths.rds")
par_at72h_depths <- readRDS(file = "par_at72h_depths.rds")
par_radolansum0_24_depths <- readRDS(file = "par_radolansum0_24_depths.rds")
par_radolansum0_6_depths <- readRDS(file = "par_radolansum0_6_depths.rds")
par_radolansum18_24_depths <- readRDS(file = "par_radolansum18_24_depths.rds")
par_radolansum12_18_depths <- readRDS(file = "par_radolansum12_18_depths.rds")
par_rad_depths <- readRDS(file = "par_rad_depths.rds")
par_prec_depths <- readRDS(file = "par_prec_depths.rds")
par_radolan_depths <- readRDS(file = "par_radolan_depths.rds")



test_set_st_sub <- test_set_sm_sub

# plot_par_depths_depths <- autoplot(par_depths_depths, contour = FALSE, legend.title = "pdp") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 20)) +
#   scale_x_continuous(limits = c(min(test_set_st_sub$depths), max(test_set_st_sub$depths)), expand = c(0, 0)) +
#   scale_y_reverse(limits = c(80, 0), expand = c(0, 0))
# plot_par_depths_depths
# 
# plot_par_depths_depths <- autoplot(par_depths_depths, contour = FALSE,legend.title = "pdp")+
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 20))+
#   scale_x_continuous(limits = c(min(test_set_st_sub$depths),max(test_set_st_sub$depths)), expand = c(0, 0))
# plot_par_depths_depths

plot_par_at3m_depths <- autoplot(par_at3m_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_3_month), max(test_set_st_sub$air_temperature_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 3 months [°C]")
plot_par_at3m_depths


par_landuse_depths$land_use[par_landuse_depths$land_use==1000] <- "forest"
par_landuse_depths$land_use[par_landuse_depths$land_use==1500] <- "arable"
par_landuse_depths$land_use[par_landuse_depths$land_use==1600] <- "grassland"
plot_par_landuse_depths <- autoplot(par_landuse_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="land use")
plot_par_landuse_depths

plot_par_twi_depths <- autoplot(par_twi_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$topo_wetness), max(test_set_st_sub$topo_wetness)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="TWI")
plot_par_twi_depths

plot_par_inclination_depths <- autoplot(par_inclination_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$inclination), max(test_set_st_sub$inclination)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="inclination")
plot_par_inclination_depths

plot_par_prec3m_depths <- autoplot(par_prec3m_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$prec_sum_3_month), max(test_set_st_sub$prec_sum_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 3 month [mm]")
plot_par_prec3m_depths

par_type_depths$soil_type[par_type_depths$soil_type==1] <- "org"
par_type_depths$soil_type[par_type_depths$soil_type==2] <- "semi"
par_type_depths$soil_type[par_type_depths$soil_type==3] <- "stag"
par_type_depths$soil_type[par_type_depths$soil_type==4] <- "ter"
par_type_depths = par_type_depths[par_type_depths$soil_type!= "org",]
plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]")
plot_par_type_depths

par_texture_depths$soil_texture[par_texture_depths$soil_texture==10] <- "clay"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==20] <- "debris"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==30] <- "loam"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==40] <- "org"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==50] <- "sand"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==60] <- "silt"
par_texture_depths$soil_texture[par_texture_depths$soil_texture==70] <- "water"
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "debris",]
par_texture_depths = par_texture_depths[par_texture_depths$soil_texture!= "org",]
plot_par_texture_depths <- autoplot(par_texture_depths, contour = FALSE,legend.title = " sm [%]",direction=-1)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="soil texture")
plot_par_texture_depths


plot_par_ndvi_depths <- autoplot(par_ndvi_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$ndvi), max(test_set_st_sub$ndvi)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="NDVI")
plot_par_ndvi_depths

plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  #scale_x_continuous(limits = c(min(test_set_st_sub$soil_type), max(test_set_st_sub$soil_type)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="aggregated soil group")
plot_par_type_depths


plot_par_at72h_depths <- autoplot(par_at72h_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_72), max(test_set_st_sub$air_temperature_mountain_72)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 72h ago [°C]")
plot_par_at72h_depths
 

plot_par_radolansum0_24_depths <- autoplot(par_radolansum0_24_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$radolan_sum_0_24), max(test_set_st_sub$radolan_sum_0_24)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation sum 0-24h ago [mm]")
plot_par_radolansum0_24_depths

plot_par_radolansum0_6_depths <- autoplot(par_radolansum0_6_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$radolan_sum_0_6), max(test_set_st_sub$radolan_sum_0_6)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation sum 0-6h ago [mm]")

plot_par_radolansum18_24_depths <- autoplot(par_radolansum18_24_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$radolan_sum_18_24), max(test_set_st_sub$radolan_sum_18_24)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 18-24h ago [mm]")

plot_par_radolansum12_18_depths <- autoplot(par_radolansum12_18_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$radolan_sum_12_18), max(test_set_st_sub$radolan_sum_12_18)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 12-18h ago [mm]")

plot_par_rad_depths <- autoplot(par_rad_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$global_radiation), max(test_set_st_sub$global_radiation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="global radiation [W/m²]")
plot_par_rad_depths

plot_par_prec_depths <- autoplot(par_prec_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$precipitation), max(test_set_st_sub$precipitation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation (climate station) [mm]")
plot_par_prec_depths

plot_par_radolan_depths <- autoplot(par_radolan_depths, contour = FALSE, legend.title = " sm [%]",direction=-1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90)) +
  scale_x_continuous(limits = c(min(test_set_st_sub$radolan), max(test_set_st_sub$radolan)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80, 0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation (RADOLAN) [mm]")



# ggpubr::ggarrange(plot_par_depths_depths, plot_par_at3m_depths, plot_par_landuse_depths,plot_par_ndvi_depths,
#          plot_par_twi_depths, plot_par_inclination_depths, plot_par_prec3m_depths, plot_par_texture_depths,
#          plot_par_type_depths, plot_par_at72h_depths, plot_par_radolansum0_24_depths, plot_par_radolansum0_6_depths, 
#          plot_par_radolansum18_24_depths, plot_par_radolansum12_18_depths, plot_par_rad_depths, plot_par_prec_depths, plot_par_radolan_depths)



ggpubr::ggarrange(plot_par_prec3m_depths, 
                  plot_par_radolansum0_24_depths, 
                  plot_par_radolansum0_6_depths, 
                  plot_par_radolansum18_24_depths, 
                  plot_par_radolansum12_18_depths, 
                  plot_par_prec_depths, 
                  plot_par_radolan_depths,
                  plot_par_at3m_depths,
                  plot_par_at72h_depths,
                  plot_par_rad_depths,
                  plot_par_landuse_depths,
                  plot_par_ndvi_depths,
                  plot_par_twi_depths, 
                  plot_par_inclination_depths, 
                  plot_par_texture_depths,
                  plot_par_type_depths,
                  labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q"),
                  font.label = list(size = 20))


#plot_par_depths_depths








