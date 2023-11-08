# ############################################################
meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

Carbon4D::load_longterm_probe_data("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
waldstein = rbind(csv_list_longterm_probe_data$Waldstein_1_clean_loam,
                  csv_list_longterm_probe_data$Waldstein_2_clean_loam,
                  csv_list_longterm_probe_data$Waldstein_3_clean_loam)
waldstein$datetime <- as.POSIXct(strptime(waldstein$datetime,"%Y-%m-%d %H:%M:%S"))
voitsumra = csv_list_longterm_probe_data$Voitsumra_clean_loam
voitsumra$datetime <- as.POSIXct(strptime(voitsumra$datetime,"%Y-%m-%d %H:%M:%S"))


probe_data_waldstein = data.frame(waldstein$datetime,waldstein$M_org,waldstein$M_org)
names(probe_data_waldstein) = c("datetime","Waldstein_M_org","Waldstein_M_05")

probe_data_voitsumra = data.frame(voitsumra$datetime,voitsumra$M_05)
names(probe_data_voitsumra) = c("datetime","Voitsumra_M_org")

soil_data = merge(probe_data_waldstein,probe_data_voitsumra,by = "datetime", all.x = T)

plot(soil_data$datetime,soil_data$Waldstein_M_05)


meteo_soil_data = merge(meteo_data, soil_data, by.x = "datetime",by.y = "datetime", all.x = T)


plot(meteo_soil_data$datetime,meteo_soil_data$precipitation)


#write.csv(meteo_soil_data,"C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_soil_data.csv")

#######################################################################
#######################################################################

#exponential smoothing

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

t <- 1:4320      # these are your time points
a <- 0.001         # assume the size at t = 0 is 10
r <- 0.0008        # assume a growth constant
y <- a*exp(r*t) # generate some y observations from our exponential model

# visualise
plot(t, y*100)      # on the original scale

#plot(t,rev(y))


#weighet sum

#na3 <-data.frame(matrix(nrow=2,ncol=264))#rep(NA, 3)
#names(na3) <- names(radolan_table[, 2:265])
#seq3 <- rev(seq(0, 1, by=1/3)[2:4])
weighted_sum_3 <- function(z, wts = rev(y)) {
  notna <- !is.na(z)
  sum(z[notna] * wts[notna])# / sum(wts[notna])
}


rain = meteo_data$precipitation
#radolan_sum_3 <- radolan_table
rain2 <- zoo::rollapply(rain, t, weighted_sum_3,fill=NA,align="right")
#radolan_sum_3 <- rollapply(coredata(rbind(na3,radolan_table[, 2:265])), 3, weighted_sum_3)

#plot(rain2[1:1000]/300000,type="l",col="red")
#lines(rain[1:1000],col="black")

plot(meteo_data$datetime,rain,col="black",type="l",ylim=c(0,40))
lines(meteo_data$datetime,rain2*4,type="l",col="red")
lines(meteo_soil_data$datetime,meteo_soil_data$Waldstein_M_org,type="l",col="red")
#points(radolan_sum_3_test[90:110,3],col="blue")


