

######################################################################
######################################################################
load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/radolan_table_waldstein.RData")

radolan_table[2:length(radolan_table)] = (radolan_table[2:length(radolan_table)])/100

# 
# test = radolan_table$S15_033[90:110]
# 
# plot(test,ylim=c(0,5))
# points((zoo::rollsum(test,3,align = "right",fill=T)),col="red",cex=2)
# points(test)
# 
# 
# test_seq = data.frame(test)
# 
# points <- cbind(lon = c(9,8,7,6,5,4,3,2,1),
#                     lat = c(1,2,3,4,5,0,0,0,0),
#                     dist = c(1,1,1,1,1,1,1,1,1))
# mysmooth <- function(z, wts = c(0.6, 0.3, 0.1)) { 
#   notna <- !is.na(z)
#   sum(z[notna] * wts[notna])# / sum(wts[notna])
# }
# points2 <- points
# points2[, 1:2] <- rollapply(rbind(NA,NA, coredata(points)[, 1:2]), 3, mysmooth)
# 
# 
# plot(points[,2],ylim=c(0,15),xlim=c(0,10))
# points(points2[,2],col="red")
# 
# 
# points
# points2


###########################################################################
# 
# #weighet sum
# 
# na3 <-data.frame(matrix(nrow=2,ncol=264))#rep(NA, 3)
# names(na3) <- names(radolan_table[, 2:265])
# #seq3 <- rev(seq(0, 1, by=1/3)[2:4])
# weighted_sum_3 <- function(z, wts = c(0.75,0.5,0.25)) { 
#   notna <- !is.na(z)
#   sum(z[notna] * wts[notna])# / sum(wts[notna])
# }
# 
# radolan_sum_3 <- radolan_table
# radolan_sum_3 <- rollapply(rbind(na3,coredata(radolan_table)[, 2:265]), 3, weighted_sum_3)
# #radolan_sum_3 <- rollapply(coredata(rbind(na3,radolan_table[, 2:265])), 3, weighted_sum_3)
# 
# plot(radolan_table[90:110,3],ylim=c(0,3))
# points(radolan_sum_3[90:110,2],col="red")
# #points(radolan_sum_3_test[90:110,3],col="blue")
# 
# 
# data.frame(radolan_table[90:110,3],radolan_sum_3[90:110,2])
# 
# bind = rbind(na3,radolan_table[, 2:265])
# 

##########################################################################

#0-3 h

radolan_sum_0_6 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],6,align = "right",fill=NA))

na6 <-data.frame(matrix(nrow=6,ncol=17))#rep(NA, 3)

names(na6) <- names(radolan_sum_0_6)

radolan_sum_6_12 = rbind(na6,radolan_sum_0_6)[1:nrow(radolan_sum_0_6),]

radolan_sum_12_18 = rbind(na6,na6,radolan_sum_0_6)[1:nrow(radolan_sum_0_6),]

radolan_sum_18_24 = rbind(na6,na6,na6,radolan_sum_0_6)[1:nrow(radolan_sum_0_6),]


plot(radolan_sum_0_6[90:130,3])
points(radolan_sum_6_12[90:130,3],col="red")
points(radolan_sum_12_18[90:130,3],col="green")

radolan_sum_0_6 = cbind(radolan_table[1],radolan_sum_0_6)

radolan_sum_6_12 = cbind(radolan_table[1],radolan_sum_6_12)

radolan_sum_12_18 = cbind(radolan_table[1],radolan_sum_12_18)

radolan_sum_18_24 = cbind(radolan_table[1],radolan_sum_18_24)

##########################################################################

#0-24


radolan_sum_0_24 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],24,align = "right",fill=NA))

na24 <- data.frame(matrix(nrow=24,ncol=17))#rep(NA, 3)
names(na24) <- names(radolan_sum_0_24)
radolan_sum_24_48 = rbind(na24,radolan_sum_0_24)[1:nrow(radolan_sum_0_24),]

radolan_sum_48_72 = rbind(na24,na24,radolan_sum_0_24)[1:nrow(radolan_sum_0_24),]

radolan_sum_week = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],168, align = "right",fill=NA))


radolan_sum_0_24 = cbind(radolan_table[1],radolan_sum_0_24)

radolan_sum_24_48 = cbind(radolan_table[1],radolan_sum_24_48)

radolan_sum_48_72 = cbind(radolan_table[1],radolan_sum_48_72)

radolan_sum_week = cbind(radolan_table[1],radolan_sum_week)

##########################################################################

#############################################################################


radolan_sums = reshape2::melt(radolan_table, id = "datetime")
names(radolan_sums) = c("datetime","probe_name","precipitation")

radolan_sum_0_6 = reshape2::melt(radolan_sum_0_6, id = "datetime")
radolan_sums$prec_sum_0_6=radolan_sum_0_6$value

radolan_sum_6_12 = reshape2::melt(radolan_sum_6_12, id = "datetime")
radolan_sums$prec_sum_6_12=radolan_sum_6_12$value

radolan_sum_12_18 = reshape2::melt(radolan_sum_12_18, id = "datetime")
radolan_sums$prec_sum_12_18=radolan_sum_12_18$value

radolan_sums_18_24 = reshape2::melt(radolan_sum_18_24, id = "datetime")
radolan_sums$prec_sum_18_24=radolan_sums_18_24$value

radolan_sums_0_24 = reshape2::melt(radolan_sum_0_24, id = "datetime")
radolan_sums$prec_sum_0_24=radolan_sums_0_24$value

radolan_sums_24_48 = reshape2::melt(radolan_sum_24_48, id = "datetime")
radolan_sums$prec_sum_24_48=radolan_sums_24_48$value

radolan_sums_48_72 = reshape2::melt(radolan_sum_48_72, id = "datetime")
radolan_sums$prec_sum_48_72=radolan_sums_48_72$value

radolan_sums_week = reshape2::melt(radolan_sum_week, id = "datetime")
radolan_sums$radolan_sum_week=radolan_sums_week$value

names(radolan_sums)=  c("datetime","probe_name","radolan","radolan_sum_0_6","radolan_sum_6_12","radolan_sum_12_18","radolan_sum_18_24",
                        "radolan_sum_0_24","radolan_sum_24_48","radolan_sum_48_72","radolan_sum_week")


# 
# 
# #######################################################################
# 
# #exponential smoothing
# 
# meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
# meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))
# 
# t <- 1:120      # these are your time points
# a <- 1         # assume the size at t = 0 is 10
# r <- 0.1        # assume a growth constant
# y <- a*exp(r*t) # generate some y observations from our exponential model
# 
# # visualise
# plot(t, y)      # on the original scale
# 
# #plot(t,rev(y))
# 
# 
# #weighet sum
# 
# #na3 <-data.frame(matrix(nrow=2,ncol=264))#rep(NA, 3)
# #names(na3) <- names(radolan_table[, 2:265])
# #seq3 <- rev(seq(0, 1, by=1/3)[2:4])
# weighted_sum_3 <- function(z, wts = y) {
#   notna <- !is.na(z)
#   sum(z[notna] * wts[notna])# / sum(wts[notna])
# }
# 
# 
# rain = meteo_data$precipitation
# #radolan_sum_3 <- radolan_table
# rain2 <- zoo::rollapply(rain, 120, weighted_sum_3,fill=NA,align="right")
# #radolan_sum_3 <- rollapply(coredata(rbind(na3,radolan_table[, 2:265])), 3, weighted_sum_3)
# 
# plot(rain2[1:1000]/300000,type="l",col="red")
# lines(rain[1:1000],col="black")
# 
# plot(rain[1:1000],col="black",type="l",ylim=c(0,20))
# lines(rain2[1:1000]/300000,type="l",col="red")
# #points(radolan_sum_3_test[90:110,3],col="blue")
# 
# 
# data.frame(radolan_table[90:110,3],radolan_sum_3[90:110,2])
# 
# bind = rbind(na3,radolan_table[, 2:265])
# 
# 
# #######################################################################
# 
# 
# #exponential smoothing
# 
# meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
# meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))
# 
# t <- 1:1200      # these are your time points
# a <- 0.001         # assume the size at t = 0 is 10
# r <- 0.0005        # assume a growth constant
# y <- a*exp(r*t) # generate some y observations from our exponential model
# 
# # visualise
# plot(t, y)      # on the original scale
# 
# #plot(t,rev(y))
# 
# 
# #weighet sum
# 
# #na3 <-data.frame(matrix(nrow=2,ncol=264))#rep(NA, 3)
# #names(na3) <- names(radolan_table[, 2:265])
# #seq3 <- rev(seq(0, 1, by=1/3)[2:4])
# weighted_sum_3 <- function(z, wts = y) {
#   notna <- !is.na(z)
#   sum(z[notna] * wts[notna])# / sum(wts[notna])
# }
# 
# 
# rain = meteo_data$precipitation
# #radolan_sum_3 <- radolan_table
# rain2 <- zoo::rollapply(rain, t, weighted_sum_3,fill=NA,align="right")
# #radolan_sum_3 <- rollapply(coredata(rbind(na3,radolan_table[, 2:265])), 3, weighted_sum_3)
# 
# plot(rain2[1:10000],type="l",col="red")
# lines(rain[1:10000]/10,col="black")
# 
# plot(rain[1:1000]/10,col="black",type="l")
# lines(rain2[1:1000],type="l",col="red")
# #points(radolan_sum_3_test[90:110,3],col="blue")
# 
# 
# data.frame(radolan_table[90:110,3],radolan_sum_3[90:110,2])
# 
# bind = rbind(na3,radolan_table[, 2:265])
# 
# ############################################################

# #######################################################################
# 
# 
# #exponential smoothing
# 
# meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
# meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))
# 
# t <- 1:2160      # these are your time points
# a <- 0.001         # assume the size at t = 0 is 10
# r <- 0.003        # assume a growth constant
# y <- a*exp(r*t) # generate some y observations from our exponential model
# 
# # visualise
# plot(t, y)      # on the original scale
# 
# #plot(t,rev(y))
# 
# sum(y)
# 
# #weighet sum
# 
# #na3 <-data.frame(matrix(nrow=2,ncol=264))#rep(NA, 3)
# #names(na3) <- names(radolan_table[, 2:265])
# #seq3 <- rev(seq(0, 1, by=1/3)[2:4])
# weighted_sum_3 <- function(z, wts = y/sum(y)) {
#   notna <- !is.na(z)
#   sum(z[notna] * wts[notna])# / sum(wts[notna])
# }
# 
# 
# rain = meteo_data$precipitation
# #radolan_sum_3 <- radolan_table
# rain2 <- zoo::rollapply(rain, t, weighted_sum_3,fill=NA,align="right")
# #radolan_sum_3 <- rollapply(coredata(rbind(na3,radolan_table[, 2:265])), 3, weighted_sum_3)
# 
# plot(rain2[1:10000]*20,type="l",col="red")
# lines(rain[1:10000],col="black")
# 
# 
# plot(rain[1:1000],col="black",type="l")
# lines(rain2[1:1000]*10,type="l",col="red")
# #points(radolan_sum_3_test[90:110,3],col="blue")
# 
# 
# data.frame(radolan_table[90:110,3],radolan_sum_3[90:110,2])
# 
# bind = rbind(na3,radolan_table[, 2:265])
# 
# ############################################################
# ############################################################
meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")
meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))
 

rain = meteo_data$precipitation

rain_sum_4032 = data.frame(zoo::rollsum(rain, 168*24, align = "right", fill=NA))
rain_sum_3360 = data.frame(zoo::rollsum(rain, 168*20, align = "right", fill=NA))
rain_sum_2688 = data.frame(zoo::rollsum(rain, 168*16, align = "right", fill=NA))
rain_sum_2016 = data.frame(zoo::rollsum(rain, 168*12, align = "right", fill=NA))
rain_sum_1344 = data.frame(zoo::rollsum(rain, 168*8, align = "right", fill=NA))
rain_sum_672 = data.frame(zoo::rollsum(rain, 168*4, align = "right", fill=NA))

rain_sums_monthly = data.frame(meteo_data$datetime,rain_sum_672[,1],rain_sum_1344[,1],rain_sum_2016[,1]
                          ,rain_sum_2688[,1],rain_sum_3360[,1],rain_sum_4032[,1])


names(rain_sums_monthly) = c("datetime","prec_sum_1_month","prec_sum_2_month","prec_sum_3_month",
                             "prec_sum_4_month","prec_sum_5_month","prec_sum_6_month")

radolan_sums = merge(radolan_sums, rain_sums_monthly, by.x = "datetime",by.y = "datetime", all.x = T,sort = F)




# 
# plot(meteo_data$datetime[1:10000],rain_sum_2016[1:10000,1]/1200*100,type="l",col="red")
# lines(meteo_data$datetime[1:10000],rain[1:10000],col="black")
# lines(meteo_data$datetime[1:10000],rain_sum_1344[1:10000,1]/1200*100,type="l",col="green")
# lines(meteo_data$datetime[1:10000],rain_sum_672[1:10000,1]/1200*100,type="l",col="blue")

Carbon4D::load_longterm_probe_data("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
waldstein = rbind(csv_list_longterm_probe_data$Waldstein_1_clean_loam,
                  csv_list_longterm_probe_data$Waldstein_2_clean_loam,
                  csv_list_longterm_probe_data$Waldstein_3_clean_loam)
waldstein$datetime <- as.POSIXct(strptime(waldstein$datetime,"%Y-%m-%d %H:%M:%S"))
voitsumra = csv_list_longterm_probe_data$Voitsumra_clean_loam
voitsumra$datetime <- as.POSIXct(strptime(voitsumra$datetime,"%Y-%m-%d %H:%M:%S"))

plot(waldstein$datetime[8000:60000],waldstein$M_05[8000:60000],ylim=c(0,45),ylab="Bodenfeuchte/ Niederschlagssumme",xlab="Jahr")
points(voitsumra$datetime,voitsumra$M_05,col="blue")
#lines(meteo_data$datetime[1:18000],rain_sum_4032[1:18000,1]/1200*100,type="l",col="red")
#lines(meteo_data$datetime[1:18000],rain_sum_3360[1:18000,1]/1200*100,type="l",col="red")
lines(meteo_data$datetime[1:18000],rain_sum_2688[1:18000,1]/1200*100,type="l",col="red")
#lines(meteo_data$datetime[1:18000],rain_sum_2016[1:18000,1]/1200*100,type="l",col="red")
#lines(meteo_data$datetime[1:18000],rain[1:18000],col="black")
lines(meteo_data$datetime[1:18000],rain_sum_1344[1:18000,1]/1200*100,type="l",col="green")
#lines(meteo_data$datetime[1:18000],rain_sum_672[1:18000,1]/1200*100,type="l",col="blue")
legend("top", legend=c("Bodenfeuchte Waldstein", "Bodenfeuchte Voitsumra", "Niederschlagssumme 4 Monate", "Niederschlagssumme 2 Monate"),
       col=c("black", "blue", "red", "green"), lwd=2, cex=0.8)


setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area")
save(radolan_sums, file = "radolan_table_sums_4_waldstein.RData")


