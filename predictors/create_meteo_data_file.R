## Daten von BayEOS herunterladen ##

dir = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data"

library(bayeos)
bayeos.connect('my_connection')

library(zoo)
library(dplyr)

#################################

startdate = "2021-06-01"
enddate = "2023-07-01"

#################################

### Allgemein ####

# In dem Webserver in den passenden Ordner navigieren (Bspw: Alle Messungen/Micrometeorology Dept/AWS Voitsumra/
# Brunnen (new site since 2019-11-26)/meteo/HMP_T_2m_Avg) ), dann rechts daneben auf "Copy ID" klicken (ID steht links drunter):

# Betrachtete Parameter:
# Niederschlag, Lufttemperatur, rel. Luftfeuchte, Windgeschwindigkeit, kurzw. Str. / Globalstrahlung


# Voitsumra-Station (im Flachland): -> Nicht weiter benutzt die Daten von hier!
# ID von Prec_pluvio_mm (Niederschlag): 133703
# ID von HMP_T_2m_Avg (Lufttemperatur): 133700
# ID von HMP_RH_2m_Avg (Rel. Luftfeuchte): 133702
# ID von windspeed_cup_2m_max (Windgeschwindigkeit): 133716
# ID von Rswd_Avg (Strahlung): 133707

# Weidenbrunnen Main Flux Tower (Wald; um die Baumkrone herum):
# Niederschlag unnoetig
# ID von 02 m Hoehe Lufttemperatur (HMP45): 14493  ("Temperatur im Wald")
# ID von 02 m Hoehe Luftfeuchte (HMP45): 14504
# ID von 02 m (Schalenstern) Wind Mittel (Windgeschwindigkeit): 14507  (unterhalb der Baumkrone)
# ID von Kurzw. Str. Globalstrahlung 30 m (CM14): 14517

# Pflanzgarten-Station (auf einer Lichtung im Wald):
# ID von Niederschlag (Kippwaage): 1985
# ID von Lufttemperatur 02 m Hoehe (HMP45): 1990  ("Temperatur auf Wiese")
# Relative Luftfeuchte stark fehlerhaft!
# ID von Windgeschwindigkeit: 1987
# ID von SDE kurzw. Str. Globalstrahlung: 122697



#Temperatur Pflanzgarten Top

Temp_Pflanzgarten = bayeos.getSeries(14493,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))
AirTempP=fortify.zoo(Temp_Pflanzgarten)
colnames(AirTempP)=c("datetime","AirTempP")
AirTempP$date <- format(AirTempP$datetime,'%Y-%m-%d')
AirTempP$hour <- format(AirTempP$datetime,'%H %Z')
AirTempP$date_hour <- paste(AirTempP$date,AirTempP$hour)
AirTempP$date_hour <- format(strptime(AirTempP$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
AirTempP$date_hour <- as.POSIXct(strptime(AirTempP$date_hour,"%Y-%m-%d %H:%M:%S"))
AirTempPHourely <- aggregate(AirTempP$AirTempP, by=list(Category=AirTempP$date_hour), FUN=mean)
names(AirTempPHourely) <- c("datetime","AirTempPHourely")
AirTempPHourely$AirTempPHourely[AirTempPHourely$AirTempPHourely < -10] = NA

plot(AirTempPHourely$datetime,AirTempPHourely$AirTempPHourely)

####################################################


#Temperatur Pflanzgarten Top

Temp_Voitsumra = bayeos.getSeries(133700,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))
AirTempV=fortify.zoo(Temp_Voitsumra)
colnames(AirTempV)=c("datetime","AirTempV")
AirTempV$date <- format(AirTempV$datetime,'%Y-%m-%d')
AirTempV$hour <- format(AirTempV$datetime,'%H %Z')
AirTempV$date_hour <- paste(AirTempV$date,AirTempV$hour)
AirTempV$date_hour <- format(strptime(AirTempV$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
AirTempV$date_hour <- as.POSIXct(strptime(AirTempV$date_hour,"%Y-%m-%d %H:%M:%S"))
AirTempVHourely <- aggregate(AirTempV$AirTempV, by=list(Category=AirTempV$date_hour), FUN=mean)
names(AirTempVHourely) <- c("datetime","AirTempVHourely")

plot(AirTempVHourely$datetime,AirTempVHourely$AirTempVHourely)

####################################################

plot(AirTempPHourely$datetime,AirTempPHourely$AirTempPHourely)
points(AirTempVHourely$datetime,AirTempVHourely$AirTempVHourely,col="red")

plot(AirTempVHourely$datetime,AirTempPHourely$AirTempPHourely-AirTempVHourely$AirTempVHourely,type="l")

#####################################################

#Globalstahlung
#Weidenbrunnen

Radiation_Pflanzgarten = bayeos.getSeries(14517,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))#main flux tower
Radiation=fortify.zoo(Radiation_Pflanzgarten)
colnames(Radiation)=c("datetime","Radiation")

Radiation$date <- format(Radiation$datetime,'%Y-%m-%d')
Radiation$hour <- format(Radiation$datetime,'%H %Z')
Radiation$date_hour <- paste(Radiation$date,Radiation$hour)
Radiation$date_hour <- format(strptime(Radiation$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
Radiation$date_hour <- as.POSIXct(strptime(Radiation$date_hour,"%Y-%m-%d %H:%M:%S"))
RadiationHourely <- aggregate(Radiation$Radiation, by=list(Category=Radiation$date_hour), FUN=mean)
names(RadiationHourely) <- c("datetime","RadiationHourely")

plot(RadiationHourely$datetime,RadiationHourely$RadiationHourely,type="l")


####################################################

#Pressure
#Pflanzgarten

Pressure_Pflanzgarten = bayeos.getSeries(1989,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))
AirPress=fortify.zoo(Pressure_Pflanzgarten)
colnames(AirPress)=c("datetime","AirPress")

AirPress$date <- format(AirPress$datetime,'%Y-%m-%d')
AirPress$hour <- format(AirPress$datetime,'%H %Z')
AirPress$date_hour <- paste(AirPress$date,AirPress$hour)
AirPress$date_hour <- format(strptime(AirPress$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
AirPress$date_hour <- as.POSIXct(strptime(AirPress$date_hour,"%Y-%m-%d %H:%M:%S"))
AirPressHourely <- aggregate(AirPress$AirPress, by=list(Category=AirPress$date_hour), FUN=mean)
names(AirPressHourely) <- c("datetime","AirPressHourely")

plot(AirPressHourely$datetime,AirPressHourely$AirPressHourely,type="l")



####################################################

#relative Luftfeuchte
#Weidebrunnen

rH_Weidebrunnen = bayeos.getSeries(14504,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))
rH=fortify.zoo(rH_Weidebrunnen)
colnames(rH)=c("datetime","rH")

rH$date <- format(rH$datetime,'%Y-%m-%d')
rH$hour <- format(rH$datetime,'%H %Z')
rH$date_hour <- paste(rH$date,rH$hour)
rH$date_hour <- format(strptime(rH$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
rH$date_hour <- as.POSIXct(strptime(rH$date_hour,"%Y-%m-%d %H:%M:%S"))
rHHourely <- aggregate(rH$rH, by=list(Category=rH$date_hour), FUN=mean)
names(rHHourely) <- c("datetime","rHHourely")
rHHourely$rHHourely[rHHourely$rHHourely > 103] = NA

plot(rHHourely$datetime,rHHourely$rHHourely,type="l")

####################################################

#windspeed
#Pflanzgarten


Wind_Pflanzgarten = bayeos.getSeries(1987,from=startdate,until=enddate,maxrows = 20000000,Sys.setenv(TZ="Etc/GMT-1"))
WindSpeed=fortify.zoo(Wind_Pflanzgarten)
colnames(WindSpeed)=c("datetime","WindSpeed")

WindSpeed$date <- format(WindSpeed$datetime,'%Y-%m-%d')
WindSpeed$hour <- format(WindSpeed$datetime,'%H %Z')
WindSpeed$date_hour <- paste(WindSpeed$date,WindSpeed$hour)
WindSpeed$date_hour <- format(strptime(WindSpeed$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
WindSpeed$date_hour <- as.POSIXct(strptime(WindSpeed$date_hour,"%Y-%m-%d %H:%M:%S"))
WindSpeedHourely <- aggregate(WindSpeed$WindSpeed, by=list(Category=WindSpeed$date_hour), FUN=mean)
names(WindSpeedHourely) <- c("datetime","WindSpeedHourely")

plot(WindSpeedHourely$datetime,WindSpeedHourely$WindSpeedHourely,type="l")


####################################################


#create empty time sequence with hourly resolution

ts_seq = data.frame(seq.POSIXt(AirTempP$datetime[1], 
                               AirTempP$datetime[length(AirTempP$datetime)], by="min"))
names(ts_seq)="datetime"

ts_hourly <- subset(ts_seq, format(datetime,'%M')=='00')



######################################################

#Precipitation

Prec_Pflanzgarten_1 = bayeos.getSeries(1985,from=startdate,until=enddate,maxrows = 300000,Sys.setenv(TZ="Etc/GMT-1"))#1985 #128037
Prec1=fortify.zoo(Prec_Pflanzgarten_1)
colnames(Prec1)=c("datetime","Prec1")
Prec1$Prec1[Prec1$Prec1 > 50] = NA
Prec1$date <- format(Prec1$datetime,'%Y-%m-%d')
Prec1$hour <- format(Prec1$datetime,'%H %Z')
Prec1$date_hour <- paste(Prec1$date,Prec1$hour)
Prec1$date_hour <- format(strptime(Prec1$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
Prec1$date_hour <- as.POSIXct(strptime(Prec1$date_hour,"%Y-%m-%d %H:%M:%S"))
Prec1Hourely <- aggregate(Prec1$Prec, by=list(Category=Prec1$date_hour), FUN=sum)
names(Prec1Hourely) <- c("datetime","Prec1Hourely")

plot(Prec1Hourely$datetime,Prec1Hourely$Prec1Hourely)

#PrecTime = ts_hourly
#PrecMerge1 <- plyr::join(PrecTime,Prec1Hourely)
#for(i in 1:length(PrecMerge[,2])) {
#  if(is.na(PrecMerge[i,2])) {
#    print(i)
#}}



####################################################


data_meteo1 <- merge(ts_hourly,AirTempPHourely,by="datetime",all.x=TRUE)
data_meteo2 <- merge(data_meteo1,AirTempVHourely,by="datetime",all.x=TRUE)
data_meteo3 <- merge(data_meteo2,Prec1Hourely,by="datetime",all.x = TRUE)
data_meteo4 <- merge(data_meteo3,RadiationHourely,by="datetime",all.x = TRUE)
data_meteo5 <- merge(data_meteo4,rHHourely,by="datetime",all.x = TRUE)
data_meteo6 <- merge(data_meteo5,AirPressHourely,by="datetime",all.x = TRUE)
data_meteo7 <- merge(data_meteo6,WindSpeedHourely,by="datetime",all.x = TRUE)

data_complete = data_meteo7

##################################################

#fill precipitaion with zero

data_complete$Prec1Hourely[is.na(data_complete$Prec1Hourely)] <- 0


#linear gap filling

for (i in c(2:length(data_complete))){
  data_complete[i] <- as.vector(na.approx(as.zoo(data_complete[i]),maxgap=1000, na.rm=F))
}


summary(data_complete)

data_complete <- cbind(data_complete[1],round(data_complete[2:length(data_complete)], digits = 2))


#################################################

names(data_complete) = c("datetime","air_temperature_mountain","air_temperature_valley","precipitation","global_radiation","relative_humidity","air_pressure","wind_speed")


write.csv(data_complete,"C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data.csv", row.names = FALSE)


###############################################




