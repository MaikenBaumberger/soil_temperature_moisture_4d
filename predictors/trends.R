

library(zoo)


meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))


temp = meteo_data$air_temperature_mountain

Dataexample = data.frame(c(1:18240))
names(Dataexample)= "X"

Dataexample$Y <- as.numeric(meteo_data$air_temperature_mountain)

#https://stackoverflow.com/questions/71312435/finding-the-slope-of-a-linear-trend-line-in-a-moving-window-in-r

trend = rollapply(Dataexample, 2160, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)

num = c(1:18240)

plot(trend[,1],trend[,2])

trend = data.frame(trend)

plot(trend$X,type="l")
lines(meteo_data$air_temperature_mountain/10000)
