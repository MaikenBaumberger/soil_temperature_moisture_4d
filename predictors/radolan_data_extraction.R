


#################################################################################
#################################################################################

library(terra)
library(raster)

#meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
#meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))
#plot(meteo_data$datetime[5000:5500], meteo_data$precipitation[5000:5500])


static_raster= stack("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/static_raster_variables/static_raster.tif")
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")
raster_base = static_raster$elevation*0
#mapview::mapview(raster_base)


testfolder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data/"

#############################################################################
#############################################################################

Sys.time()

date <- 20220104#20220104


# Select all rasters from the selected date

files <- list.files(testfolder,pattern=".asc$", full.names=TRUE)

#select specific raster
files_date<- files[grepl(date,files)] # 1 tag


# Read raster and project

files1 <- terra::rast(files_date)#files

#set the projection and crop

# Assign the new CRS to the raster
crs(files1) <- "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.9330127 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# changing the crs of the raster to match the extent
new_crs <- "+proj=longlat +datum=WGS84 +no_defs" 

radolan_germany = files1#stack(files1)

extent_dwd <- extent(120000, 170000, -4350000, -4300000)

radolan_crop <- crop(radolan_germany, extent_dwd)


#radolan_crop = stack(radolan_crop)

radolan_crop_project = terra::project(radolan_crop,new_crs)

radolan_crop_project = stack(radolan_crop_project)

radolan_resample = resample(radolan_crop_project,raster_base)

radolan_study_site <- crop(radolan_resample, raster_base)


Sys.time()

mapview::mapview(radolan_study_site[[11]])




#mapview::mapview(radolan_study_site[[11]])+mapview::mapview(radolan_germany[[11]])+mapview::mapview(dwd_crop[[11]])

files <- list.files(testfolder,pattern=".asc$", full.names=TRUE)

#####################################################################
####################################################################


files_month<- files[grepl(202201,files)] 


#####################################################################

#LOOP

Sys.time()

dates <- c(20220101,20220102,20220103,20220104)

stack_radolan_study_site = raster_base


for(i in dates){
  
  print(i)

#Select all rasters from the selected date


#select specific raster
files_date<- files[grepl(i,files)] # 1 tag


# Read raster and project

files1 <- terra::rast(files_date)#files

#set the projection and crop

# Assign the new CRS to the raster
crs(files1) <- "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.9330127 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# changing the crs of the raster to match the extent
new_crs <- "+proj=longlat +datum=WGS84 +no_defs" 

radolan_germany = files1#stack(files1)

extent_dwd <- extent(120000, 170000, -4350000, -4300000)

radolan_crop <- crop(radolan_germany, extent_dwd)


#radolan_crop = stack(radolan_crop)

radolan_crop_project = terra::project(radolan_crop,new_crs)

radolan_crop_project = stack(radolan_crop_project)

radolan_resample = resample(radolan_crop_project,raster_base)

radolan_study_site <- crop(radolan_resample, raster_base)

stack_radolan_study_site = stack(stack_radolan_study_site,radolan_study_site)

}


stack_radolan_study_site = dropLayer(stack_radolan_study_site,1)




test = stack_radolan_study_site+static_raster$elevation

mapview::mapview(test)


files
