

######################################################################
######################################################################
load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/radolan_table.RData")

radolan_table[2:length(radolan_table)] = (radolan_table[2:length(radolan_table)])/100

test = radolan_table$S15_033[90:110]

plot(test,ylim=c(0,5))
points((zoo::rollsum(test,3,align = "right",fill=T)),col="red",cex=2)
points(test)


test_seq = data.frame(test)

points <- cbind(lon = c(9,8,7,6,5,4,3,2,1),
                    lat = c(1,2,3,4,5,0,0,0,0),
                    dist = c(1,1,1,1,1,1,1,1,1))
mysmooth <- function(z, wts = c(0.6, 0.3, 0.1)) { 
  notna <- !is.na(z)
  sum(z[notna] * wts[notna])# / sum(wts[notna])
}
points2 <- points
points2[, 1:2] <- rollapply(rbind(NA,NA, coredata(points)[, 1:2]), 3, mysmooth)


plot(points[,2],ylim=c(0,15),xlim=c(0,10))
points(points2[,2],col="red")


points
points2


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

na6 <-data.frame(matrix(nrow=6,ncol=264))#rep(NA, 3)
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

na24 <- data.frame(matrix(nrow=24,ncol=264))#rep(NA, 3)
names(na24) <- names(radolan_sum_0_24)
radolan_sum_24_48 = rbind(na24,radolan_sum_0_24)[1:nrow(radolan_sum_0_24),]

radolan_sum_48_72 = rbind(na24,na24,radolan_sum_0_24)[1:nrow(radolan_sum_0_24),]


radolan_sum_0_24 = cbind(radolan_table[1],radolan_sum_0_24)

radolan_sum_24_48 = cbind(radolan_table[1],radolan_sum_24_48)

radolan_sum_48_72 = cbind(radolan_table[1],radolan_sum_48_72)

##########################################################################




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

radolan_sums_24_48 = reshape2::melt(radolan_sum_24_48, id = "datetime")
radolan_sums$prec_sum_24_48=radolan_sums_24_48$value

radolan_sums_48_72 = reshape2::melt(radolan_sum_48_72, id = "datetime")
radolan_sums$prec_sum_48_72=radolan_sums_48_72$value


names(radolan_sums)=  c("datetime","probe_name","prec","prec_sum_0_6","prec_sum_6_12","prec_sum_12_18","prec_sum_18_24",
                        "prec_sum_24_48","prec_sum_48_72")




#######################################################################



setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area")
#save(radolan_sums, file = "radolan_table_sums.RData")


