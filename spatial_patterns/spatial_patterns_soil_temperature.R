
library(raster)

st_pred_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_temperature_prediction_v2"
st_pred_files <- list.files(st_pred_folder,pattern=".tif$", full.names=TRUE)


st_pred = terra::rast(st_pred_files)

seq(2,443,8)


st_pred_05 = st_pred[[c(seq(1,443,8))]]
st_pred_15 = st_pred[[c(seq(2,443,8))]]
st_pred_25 = st_pred[[c(seq(3,443,8))]]
st_pred_35 = st_pred[[c(seq(4,443,8))]]
st_pred_45 = st_pred[[c(seq(5,443,8))]]
st_pred_55 = st_pred[[c(seq(6,443,8))]]
st_pred_65 = st_pred[[c(seq(7,443,8))]]
st_pred_75 = st_pred[[c(seq(8,443,8))]]



st_pred_subset = st_pred[[1]][]

mean(st_pred_subset,na.rm=T)
median(st_pred_subset,na.rm=T)
var(st_pred_subset,na.rm=T)
sd(st_pred_subset,na.rm=T)
min(st_pred_subset,na.rm=T)
max(st_pred_subset,na.rm=T)



st_pred_subset_2 = st_pred[[1:10]]

#für jede tiefe

plot(mean(st_pred_subset_2))
plot(sd(st_pred_subset_2,na.rm=T))
plot(min(st_pred_subset_2))
plot(max(st_pred_subset_2))




names(st_pred_subset)

test = terra::app(st_pred_subset, mean)

plot(test)





val = getValues(st_pred$`202201041100_05`)



plot(test)

st_pred$`202201041100_05`[1000,1200]


meanR <- raster::calc(st_pred, fun = mean)
