
library(raster)
library(terra)
library(ggplot2)



setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/4d_variability_landuse")
var_depth_st = readRDS("var_depth_st.rds")
var_time_st = readRDS("var_time_st.rds")
var_depth_sm = readRDS("var_depth_sm.rds")
var_time_sm = readRDS("var_time_sm.rds")



col_forest = "darkgreen" #"#238A8DFF"
col_meadow = "chartreuse3" # "#73D055FF"
col_arable = "gold2" #"#FDE725FF"




plot_var_depth_st <- ggplot(var_depth_st, aes(x = mean, y = depth, color=landuse_cat)) +
  geom_point(size=2) +
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
  xlab("soil temperature [°C]")+
  ylab("depth [cm]")+
  scale_y_continuous(breaks = seq(-5, -75, by = -10),
                     labels = c("5","15","25","35","45","55","65","75"))+
  scale_color_manual(values = c("forest" = col_forest, "meadow" = col_meadow, "arable_land" = col_arable))+
  xlim(0, 18)+
  theme(legend.position = "none",
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))
  #scale_color_manual(values = c("forest" = "#238A8DFF", "meadow" = "#73D055FF", "arable_land" = "#FDE725FF"))+
  # xlim(0, 18)
  plot_var_depth_st



plot_var_time_st <- ggplot(var_time_st, aes(x = month, y = mean, color=landuse_cat)) +
  geom_point(size=2)+
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
  ylab("soil temperature [°C]")+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))+
  theme(legend.position = "none",
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))+
  scale_color_manual(values = c("forest" = col_forest, "meadow" = col_meadow, "arable_land" = col_arable))
plot_var_time_st

#legend.position = c(0.12,0.8),


plot_var_depth_sm <- ggplot(var_depth_sm, aes(x = mean, y = depth, color=landuse_cat)) +
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
  xlab("soil moisture [%]")+
  ylab("depth [cm]")+
  scale_y_continuous(breaks = seq(-5, -75, by = -10),
                     labels = c("5","15","25","35","45","55","65","75"))+
  scale_color_manual(values = c("forest" = col_forest, "meadow" = col_meadow, "arable_land" = col_arable))+
  xlim(10, 24)+
  theme(legend.position = "none",
      legend.key=element_blank(),
      legend.title = element_blank(),
      legend.text=element_text(size=14))
plot_var_depth_sm



plot_var_time_sm <- ggplot(var_time_sm, aes(x = month, y = mean, color=landuse_cat)) +
  geom_point(size=2)+
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
  ylab("soil moisture [%]")+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))+
  theme(legend.position = "none",
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))+
  scale_color_manual(values = c("forest" = col_forest, "meadow" = col_meadow, "arable_land" = col_arable))

plot_var_time_sm


var_time_st_legend <- var_time_st
var_time_st_legend$landuse_cat[var_time_st_legend$landuse_cat=="meadow"] <-  "grassland"
var_time_st_legend$landuse_cat[var_time_st_legend$landuse_cat=="arable_land"] <-  "arable land"
plot_for_legend <- ggplot(var_time_st_legend, aes(x = month, y = mean, color=landuse_cat)) +
  geom_point(size=2)+
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
  ylab("soil temperature [°C]")+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))+
  theme(legend.position = c(0.5,0.07),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=14))+
  scale_color_manual(values = c("forest" = col_forest, "grassland" = col_meadow, "arable land" = col_arable))
plot_for_legend



# Extract the legend. Returns a gtable
leg <- ggpubr::get_legend(plot_for_legend)

# Convert to a ggplot and print
legend = ggpubr::as_ggplot(leg)




ggpubr::ggarrange(plot_var_depth_st,plot_var_time_st,legend,plot_var_depth_sm,plot_var_time_sm,
                  ncol=3,nrow=2,labels = c("A", "B","","C","D"),widths = c(1,2,0.5))

