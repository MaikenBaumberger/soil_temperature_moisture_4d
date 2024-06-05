# library(raster)
# library(terra)
# library(CAST)
# library(caret)

#https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots

library(pdp)
library(ggplot2)
library(ggbreak)

setwd("C:/Users/maike/Desktop/Carbon4D/Palma/hyperparameter_tuning_2")

load("test_set_st.Rdata")
load("train_set_st.Rdata")
load("rfmodel_st.Rdata")

head(train_set_st)

set.seed(12)
test_set_st_sub <- dplyr::sample_n(test_set_st, 1000)#test_set_st[1:50,]

pred <- predict(rfmodel, test_set_st_sub, probability = TRUE)


setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/partail_dependency/pdp_soil_temperature_1000")


rfmodel$modelInfo

# 
# 
# 
# par_prec4m_depths <- partial(rfmodel, pred.var = c("prec_sum_4_month","depths"), chull = TRUE)
# saveRDS(par_prec4m_depths,file= "par_prec4m_depths.rds")
# 
# par_at1m_depths <- partial(rfmodel, pred.var = c("air_temperature_month","depths"), chull = TRUE)
# saveRDS(par_at1m_depths,file= "par_at1m_depths.rds")
# 
# par_at3m_depths <- partial(rfmodel, pred.var = c("air_temperature_3_month","depths"), chull = TRUE)
# saveRDS(par_at3m_depths,file= "par_at3m_depths.rds")
# 
# par_landuse_depths <- partial(rfmodel, pred.var = c("land_use","depths"), cats="land_use")
# saveRDS(par_landuse_depths,file= "par_landuse_depths.rds")
# 
# par_trend3m_depths <- partial(rfmodel, pred.var = c("trend_3month","depths"), chull = TRUE)
# saveRDS(par_trend3m_depths,file= "par_trend3m_depths.rds")
# 
# par_elevation_depths <- partial(rfmodel, pred.var = c("elevation","depths"), chull = TRUE)
# saveRDS(par_elevation_depths,file= "par_elevation_depths.rds")
# 
# par_ndwi_depths <- partial(rfmodel, pred.var = c("ndwi","depths"), chull = TRUE)
# saveRDS(par_ndwi_depths,file= "par_ndwi_depths.rds")
# 
# par_trend1m_depths <- partial(rfmodel, pred.var = c("trend_month","depths"), chull = TRUE)
# saveRDS(par_trend1m_depths,file= "par_trend1m_depths.rds")
# 
# par_at3h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_3","depths"), chull = TRUE)
# saveRDS(par_at3h_depths,file= "par_at3h_depths.rds")
# 
# par_type_depths <- partial(rfmodel, pred.var = c("soil_type","depths"), cats="soil_type")
# saveRDS(par_type_depths,file= "par_type_depths.rds")
# 
# par_at_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain","depths"), chull = TRUE)
# saveRDS(par_at_depths,file= "par_at_depths.rds")
# 
# par_at6h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_6","depths"), chull = TRUE)
# saveRDS(par_at6h_depths,file= "par_at6h_depths.rds")
# 
# par_at12h_depths <- partial(rfmodel, pred.var = c("air_temperature_mountain_12","depths"), chull = TRUE)
# saveRDS(par_at12h_depths,file= "par_at12h_depths.rds")
# 
# par_rh_depths <- partial(rfmodel, pred.var = c("relative_humidity","depths"), chull = TRUE)
# saveRDS(par_rh_depths,file= "par_rh_depths.rds")
# 
# par_rad_depths <- partial(rfmodel, pred.var = c("global_radiation","depths"), chull = TRUE)
# saveRDS(par_rad_depths,file= "par_rad_depths.rds")
# 
# par_prec_depths <- partial(rfmodel, pred.var = c("precipitation","depths"), chull = TRUE)
# saveRDS(par_prec_depths,file= "par_prec_depths.rds")
# 
# par_depths_depths <- partial(rfmodel, pred.var = c("depths"), chull = TRUE)
# saveRDS(par_depths_depths,file= "par_depths_depths.rds")
# 

####################################################################
####################################################################

par_prec4m_depths <- readRDS(file = "par_prec4m_depths.rds")
par_at1m_depths <- readRDS(file= "par_at1m_depths.rds")
par_at3m_depths <- readRDS(file= "par_at3m_depths.rds")
par_landuse_depths <- readRDS(file = "par_landuse_depths.rds")
par_trend3m_depths <- readRDS(file = "par_trend3m_depths.rds")
par_elevation_depths <- readRDS(file = "par_elevation_depths.rds")
par_ndwi_depths <- readRDS(file = "par_ndwi_depths.rds")
par_prec4m_depths <- readRDS(file = "par_prec4m_depths.rds")
par_at3h_depths <- readRDS(file = "par_at3h_depths.rds")
par_type_depths <- readRDS(file = "par_type_depths.rds")
par_at_depths <- readRDS(file = "par_at_depths.rds")
par_at6h_depths <- readRDS(file = "par_at6h_depths.rds")
par_at12h_depths <- readRDS(file = "par_at12h_depths.rds")
par_rh_depths <- readRDS(file = "par_rh_depths.rds")
par_rad_depths <- readRDS(file = "par_rad_depths.rds")
par_prec_depths <- readRDS(file = "par_prec_depths.rds")
par_depths_depths <- readRDS(file = "par_depths_depths.rds")
par_trend1m_depths <- readRDS(file = "par_trend1m_depths.rds")

test_set_st_sub$land_use

plot_par_prec4m_depths <- autoplot(par_prec4m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$prec_sum_4_month),max(test_set_st_sub$prec_sum_4_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="precipitation sum 4 month [mm]")
plot_par_prec4m_depths

plot_par_at1m_depths <- autoplot(par_at1m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_month),max(test_set_st_sub$air_temperature_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 1 month")
plot_par_at1m_depths

plot_par_at3m_depths <- autoplot(par_at3m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_3_month),max(test_set_st_sub$air_temperature_3_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature mean 3 month")
plot_par_at3m_depths


par_landuse_depths$land_use[par_landuse_depths$land_use==1000] <- "forest"
par_landuse_depths$land_use[par_landuse_depths$land_use==1500] <- "arable"
par_landuse_depths$land_use[par_landuse_depths$land_use==1600] <- "grassland"

plot_par_landuse_depths <- autoplot(par_landuse_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="land use")
plot_par_landuse_depths


plot_par_trend3m_depths <- autoplot(par_trend3m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$trend_3month),max(test_set_st_sub$trend_3month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x = "trend of air temperature mean 3 month")

plot_par_elevation_depths <- autoplot(par_elevation_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$elevation),max(test_set_st_sub$elevation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="elevation [m]")

plot_par_ndwi_depths <- autoplot(par_ndwi_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$ndwi),max(test_set_st_sub$ndwi)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="NDWI")

plot_par_trend1m_depths <- autoplot(par_trend1m_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$trend_month),max(test_set_st_sub$trend_month)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x = "trend of air temperature mean 1 month")

plot_par_at3h_depths <- autoplot(par_at3h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_3),max(test_set_st_sub$air_temperature_mountain_3)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 3h ago [°C]")
plot_par_at3h_depths

par_type_depths$soil_type[par_type_depths$soil_type==1] <- "org"
par_type_depths$soil_type[par_type_depths$soil_type==2] <- "semi"
par_type_depths$soil_type[par_type_depths$soil_type==3] <- "stag"
par_type_depths$soil_type[par_type_depths$soil_type==4] <- "ter"
par_type_depths = par_type_depths[par_type_depths$soil_type!= "org",]
plot_par_type_depths <- autoplot(par_type_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="aggregated soil group")
plot_par_type_depths

plot_par_at_depths <- autoplot(par_at_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain),max(test_set_st_sub$air_temperature_mountain)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature [°C]")

plot_par_at6h_depths <- autoplot(par_at6h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_6),max(test_set_st_sub$air_temperature_mountain_6)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 6h ago [°C]")

plot_par_at12h_depths <- autoplot(par_at12h_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$air_temperature_mountain_12),max(test_set_st_sub$air_temperature_mountain_12)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]",x="air temperature 12h ago [°C]")

plot_par_rh_depths <- autoplot(par_rh_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$relative_humidity),max(test_set_st_sub$relative_humidity)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="relative humidity [%]")

plot_par_rad_depths <- autoplot(par_rad_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$global_radiation),max(test_set_st_sub$global_radiation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="global radiation [W/m²]")

plot_par_prec_depths <- autoplot(par_prec_depths, contour = FALSE,legend.title = " st [°C]")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        legend.title = element_text(size = 20, angle = 90))+
  scale_x_continuous(limits = c(min(test_set_st_sub$precipitation),max(test_set_st_sub$precipitation)), expand = c(0, 0)) +
  scale_y_reverse(limits = c(80,0), expand = c(0, 0))+
  labs(y = "depth [cm]", x="precipitation [mm]")

# plot_par_depths_depths <- autoplot(par_depths_depths, contour = FALSE,legend.title = " st [°C]")+
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 20),
#         legend.title = element_text(size = 20, angle = 90))+
#   scale_x_continuous(limits = c(min(test_set_st_sub$depths),max(test_set_st_sub$depths)), expand = c(0, 0))+
#   labs(y = "depth [cm]") 
# plot_par_depths_depths
# 

# ggpubr::ggarrange(plot_par_prec4m_depths,plot_par_at1m_depths,plot_par_at3m_depths,plot_par_landuse_depths,plot_par_trend3m_depths,plot_par_elevation_depths,
#                   plot_par_ndwi_depths,plot_par_trend1m_depths,plot_par_at3h_depths,plot_par_type_depths,plot_par_at_depths,plot_par_at6h_depths,plot_par_rh_depths,
#                   plot_par_rad_depths,plot_par_prec_depths)
# 

ggpubr::ggarrange(plot_par_at1m_depths,plot_par_at3m_depths,plot_par_at_depths,plot_par_at3h_depths,plot_par_at6h_depths,plot_par_at12h_depths,
                  plot_par_trend3m_depths,plot_par_trend1m_depths,plot_par_prec4m_depths,plot_par_prec_depths,
                  plot_par_rh_depths,
                  plot_par_rad_depths,
                  plot_par_landuse_depths,
                  plot_par_ndwi_depths,
                  plot_par_type_depths,
                  plot_par_elevation_depths,
                  labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P"),
                  font.label = list(size = 20))

#plot_par_depths_depths

#ggpubr::ggarrange(plot_par_at1m_depths,plot_par_ndwi_depths,plot_par_elevation_depths,plot_par_type_depths)
###
#length(unique(train_set_st$date_hour))
