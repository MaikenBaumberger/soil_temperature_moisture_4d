

library(zoo)


meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))


temp = meteo_data$air_temperature_mountain

Dataexample = data.frame(c(1:18240))
names(Dataexample)= "X"

Dataexample$Y <- as.numeric(meteo_data$air_temperature_mountain)

#https://stackoverflow.com/questions/71312435/finding-the-slope-of-a-linear-trend-line-in-a-moving-window-in-r

trend_week = rollapply(Dataexample, 168, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)
trend_month = rollapply(Dataexample, 720, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)
trend_3month = rollapply(Dataexample, 2160, function(d)lm(Y~X, data.frame(d))$coefficients, by.column=FALSE,align = "right",fill = NA)

trend_week = data.frame(trend_week)
trend_month = data.frame(trend_month)
trend_3month = data.frame(trend_3month)

trends = cbind(meteo_data[1],trend_week$X,trend_month$X,trend_3month$X)

names(trends) = c("datetime","trend_week","trend_month","trend_3month")







plot(trend_week$X,type="l")
lines(meteo_data$air_temperature_mountain/10000)

plot(trend_month$X,type="l")
lines(meteo_data$air_temperature_mountain/10000)

plot(trend_3month$X,type="l")
lines(meteo_data$air_temperature_mountain/10000)
