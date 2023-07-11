
#air temperature height correction

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data")

meteo_data = read.table("meteo_data.csv", sep=",", dec=".",header=T)
meteo_data$datetime = as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

height_mountain = 700
height_valley = 500

temp_mountain = 20
temp_valley = 20
height_location = 600

temp_mountain - ((temp_mountain-temp_valley)/(height_mountain-height_valley))*(height_mountain-height_location)



plot(meteo_data$datetime[1:500],meteo_data$air_temperature_valley[1:500],col="red",type="l",
     xlab="Date",ylab="Temperature [°C]",lwd=2)
lines(meteo_data$datetime[1:500],meteo_data$air_temperature_mountain[1:500],col="blue",lwd=2)
legend("topleft", legend=c("Temperature Voitsumra", "Temperature Waldstein"),
       col=c("red", "blue"), lty=1:1, cex=1,lwd=2)

       