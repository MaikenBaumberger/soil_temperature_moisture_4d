############################
#precipitation


radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)


#radolan_date_time = substr(radolan_files, 116,126) 

radolan_date = as.numeric(substr(radolan_files, 117,124)) 

radolan_date_posix = as.POSIXct(strptime(radolan_date,"%Y%m%d"))


#nur für 12 uhr

i=20220215

daybefore1 = i-1
daybefore2 = i-2
daybefore3 = i-3
daybefore4 = i-4
daybefore5 = i-5

files_day<- radolan_files[grepl(i,radolan_files)]
files_daybefore1<- radolan_files[grepl(daybefore1,radolan_files)]
files_daybefore2<- radolan_files[grepl(daybefore2,radolan_files)]
files_daybefore3<- radolan_files[grepl(daybefore3,radolan_files)]
files_daybefore4<- radolan_files[grepl(daybefore4,radolan_files)]
files_daybefore5<- radolan_files[grepl(daybefore5,radolan_files)]

radolan_day = stack(files_day)
radolan_daybefore1 = stack(files_daybefore1)
radolan_daybefore2 = stack(files_daybefore2)
radolan_daybefore3 = stack(files_daybefore3)
radolan_daybefore4 = stack(files_daybefore4)
radolan_daybefore5 = stack(files_daybefore5)


daily_sum = sum(radolan_day/100)
plot(radolan_day/100)
plot(daily_sum)

radolan_now_spatially = radolan_day[[12]]/100
radolan_3h_spatially = sum(radolan_day[[10:12]]/100)
radolan_6h_spatially = sum(radolan_day[[7:12]]/100)
radolan_12h_spatially = sum(radolan_day[[1:12]]/100)
radolan_24h_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[12:24]]/100)
radolan_2d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[12:24]]/100)
radolan_3d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,radolan_daybefore3[[12:24]]/100)
radolan_4d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
                 radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[12:24]]/100)
radolan_5d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
                 radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[1:24]]/100,radolan_daybefore5[[12:24]]/100)





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





