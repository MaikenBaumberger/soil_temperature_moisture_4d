##############################################################

#radolan

radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)


#radolan_date_time = substr(radolan_files, 116,126) 

radolan_date = as.numeric(substr(radolan_files, 117,124)) 

radolan_date_posix = as.POSIXct(strptime(radolan_date,"%Y%m%d"))


#create empty time sequence with hourly resolution

ts_seq = data.frame(seq.POSIXt(radolan_date_posix[1], 
                               radolan_date_posix[length(radolan_date_posix)], by="hour"))
names(ts_seq)="datetime"


#plots location
probe_meta_data_monthly <- Carbon4D::load_probe_meta_data_monthly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data_weekly <- Carbon4D::load_probe_meta_data_weekly("C:/Users/maike/Desktop/Carbon4D/DownloadGitData")
probe_meta_data = rbind(probe_meta_data_monthly,probe_meta_data_weekly)

probes = data.frame(probe_meta_data$probe_id,probe_meta_data$lat,probe_meta_data$lon)
names(probes) = c("probe_id","lat","lon")
probes$coordinates <- sf::st_as_sf(probes,coords = c("lon","lat"), crs = 4326)

#df <- data.frame()

df <- as.data.frame(matrix(NA, 0, 265))


for(i in radolan_date){
  
  print(i)
  
  #i=20220101
  
  files_day<- radolan_files[grepl(i,radolan_files)]
  
  radolan_date_day = substr(files_day, 117,124) 
  
  radolan_date_day_posix = as.POSIXct(strptime(radolan_date_day,"%Y%m%d"))
  
  # ts_seq_day = data.frame(seq.POSIXt(radolan_date_day_posix[1], 
  #                                    radolan_date_day_posix[length(radolan_date_day_posix)], by="hour"))
  
  ts_seq_day = data.frame(seq.POSIXt(radolan_date_day_posix, 
                                     radolan_date_day_posix+86399, by="hour"))#86399
  
  names(ts_seq_day)="datetime"
  
  
  radolan = stack(files_day)
  
  ext_radolan = extract(radolan,probes$coordinates,df=T)
  probes_radolan = cbind(probes[1],ext_radolan[2:25])
  probes_radolan_t = setNames(data.frame(t(probes_radolan[,-1])), probes_radolan[,1])
  
  radolan_date_time=ts_seq_day#PROBLEM
  
  probes_radolan_t= cbind(radolan_date_time,probes_radolan_t)
  
  names(df) = names(probes_radolan_t)
  #plot(probes_radolan_t$radolan_date_time,probes_radolan_t$S0005)
  
  
  #radolan_table = merge(ts_seq,probes_radolan_t,by.x="datetime",by.y="datetime",all.x=T)
  
  
  df = rbind(df,probes_radolan_t)
  
}

radolan_table = merge(ts_seq,df,by.x="datetime",by.y="datetime",all.x=T)


save(radolan_table, file = "radolan_table.RData")


######################################################################
######################################################################
load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/radolan_table.RData")

radolan_table[2:length(radolan_table)] = (radolan_table[2:length(radolan_table)])/100

test = radolan_table$S15_033[90:110]

plot(test,ylim=c(0,5))
points((zoo::rollsum(test,3,align = "right",fill=T)),col="red",cex=2)
points(test)

radolan_sum_3 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],3,align = "right",fill=T))
#names(radolan_sum_3) = paste0(names(radolan_sum_3),"_3")
radolan_sum_3 = cbind(radolan_table[1],radolan_sum_3)

radolan_sum_6 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],6,align = "right",fill=T))
#names(radolan_sum_6) = paste0(names(radolan_sum_6),"_6")
radolan_sum_6 = cbind(radolan_table[1],radolan_sum_6)

radolan_sum_12 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],12,align = "right",fill=T))
#names(radolan_sum_12) = paste0(names(radolan_sum_12),"_12")
radolan_sum_12 = cbind(radolan_table[1],radolan_sum_12)

radolan_sum_24 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],24,align = "right",fill=T))
#names(radolan_sum_24) = paste0(names(radolan_sum_24),"_24")
radolan_sum_24 = cbind(radolan_table[1],radolan_sum_24)

radolan_sum_48 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],48,align = "right",fill=T))
#names(radolan_sum_48) = paste0(names(radolan_sum_48),"_48")
radolan_sum_48 = cbind(radolan_table[1],radolan_sum_48)

radolan_sum_72 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],72,align = "right",fill=T))
#names(radolan_sum_72) = paste0(names(radolan_sum_72),"_72")
radolan_sum_72 = cbind(radolan_table[1],radolan_sum_72)

radolan_sum_96 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],96,align = "right",fill=T))
#names(radolan_sum_96) = paste0(names(radolan_sum_96),"_96")
radolan_sum_96 = cbind(radolan_table[1],radolan_sum_96)

radolan_sum_120 = data.frame(zoo::rollsum(radolan_table[2:length(radolan_table)],120,align = "right",fill=T))
#names(radolan_sum_120) = paste0(names(radolan_sum_120),"_120")
radolan_sum_120 = cbind(radolan_table[1],radolan_sum_120)


radolan_sums = reshape2::melt(radolan_table, id = "datetime")
names(radolan_sums) = c("datetime","probe_name","precipitation")

radolan_sums_3 = reshape2::melt(radolan_sum_3, id = "datetime")
radolan_sums$prec_sum_3=radolan_sums_3$value

radolan_sums_6 = reshape2::melt(radolan_sum_6, id = "datetime")
radolan_sums$prec_sum_6=radolan_sums_6$value

radolan_sums_12 = reshape2::melt(radolan_sum_12, id = "datetime")
radolan_sums$prec_sum_12=radolan_sums_12$value

radolan_sums_24 = reshape2::melt(radolan_sum_24, id = "datetime")
radolan_sums$prec_sum_24=radolan_sums_24$value

radolan_sums_48 = reshape2::melt(radolan_sum_48, id = "datetime")
radolan_sums$prec_sum_48=radolan_sums_48$value

radolan_sums_72 = reshape2::melt(radolan_sum_72, id = "datetime")
radolan_sums$prec_sum_72=radolan_sums_72$value

radolan_sums_96 = reshape2::melt(radolan_sum_96, id = "datetime")
radolan_sums$prec_sum_96=radolan_sums_96$value

radolan_sums_120 = reshape2::melt(radolan_sum_120, id = "datetime")
radolan_sums$prec_sum_120=radolan_sums_120$value


names(radolan_sums)=  c("datetime","probe_name","prec","prec_sum_3","prec_sum_6","prec_sum_12","prec_sum_24",
                               "prec_sum_48","prec_sum_72","prec_sum_96","prec_sum_120")



setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area")
save(radolan_sums, file = "radolan_table_sums.RData")


