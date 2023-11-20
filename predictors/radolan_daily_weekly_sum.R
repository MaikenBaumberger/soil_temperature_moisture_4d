#precipitation


library(terra)
library(raster)

radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)


#radolan_date_time = substr(radolan_files, 116,126) 

radolan_date = as.numeric(substr(radolan_files, 117,124)) 

radolan_date_posix = as.POSIXct(strptime(radolan_date,"%Y%m%d"))

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area_daily_sum")

for (i in 301:400) { 
  
print(i)
  
radolan_stack = stack(radolan_files[i])

radolan_daily_sum = sum(radolan_stack[[1:24]])

date = radolan_date[i]

writeRaster(radolan_daily_sum,paste0("radolan_study_site_daily_sum_",date,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')

}

############################################################################
############################################################################


radolan_folder <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area/"
radolan_files <- list.files(radolan_folder,pattern=".tif$", full.names=TRUE)

radolan_folder_daily <- "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area_daily_sum/"
radolan_files_daily <- list.files(radolan_folder_daily,pattern=".tif$", full.names=TRUE)


#create empty time sequence with hourly resolution

start = as.POSIXct(strptime(202112010000,"%Y%m%d%H%M"))
end = as.POSIXct(strptime(202212312300,"%Y%m%d%H%M"))

ts_seq = data.frame(seq.POSIXt(start, end, by="hour"))
names(ts_seq)="datetime"

setwd("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/radolan_data_study_area_168h_sum")


#396*24
#radolan_files_daily

for (j in 51:396) { 
  
  print(j)
  
  day1 = stack(radolan_files[1+j])
  day2 = stack(radolan_files_daily[2+j])
  day3 = stack(radolan_files_daily[3+j])
  day4 = stack(radolan_files_daily[4+j])
  day5 = stack(radolan_files_daily[5+j])
  day6 = stack(radolan_files_daily[6+j])
  day7 = stack(radolan_files_daily[7+j])
  day8 = stack(radolan_files[8+j])
    
    for (k in 1:24){
      
      print(k)
    
      stack_week = stack(day1[[k:24]],day2,day3,day4,day5,day6,day7,day8[[1:k]])
      
      stack_week = (stack_week[[1:30]])
      
      names(stack_week)
      
      weekly_sum = sum(stack_week)
      
      #plot(weekly_sum)
      
      datetime = as.character(ts_seq[168+j*24+k-1,])
      
      datetime_chr = paste0(substr(datetime, 1,4),substr(datetime, 6,7),substr(datetime, 9,10),substr(datetime,12,13),substr(datetime,15,16))
      
      writeRaster(weekly_sum,paste0("radolan_168h_sum_",datetime_chr,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')

    }
  
}



#############################################################
#day 388

for (j in 388) { 
  
  print(j)
  
  day1 = stack(radolan_files[1+j])
  day2 = stack(radolan_files_daily[2+j])
  day3 = stack(radolan_files_daily[3+j])
  day4 = stack(radolan_files_daily[4+j])
  day5 = stack(radolan_files_daily[5+j])
  day6 = stack(radolan_files_daily[6+j])
  day7 = stack(radolan_files_daily[7+j])
  day8 = stack(radolan_files[8+j])
  
  for (k in 24:24){
    
    print(k)
    
    stack_week = stack(day1[[k:24]],day2,day3,day4,day5,day6,day7,day8[[1:k]])
    
    stack_week = (stack_week[[2:31]])
    
    names(stack_week)
    
    weekly_sum = sum(stack_week)
    
    #plot(weekly_sum)
    
    datetime = as.character(ts_seq[168+j*24+k,])
    
    datetime_chr = paste0(substr(datetime, 1,4),substr(datetime, 6,7),substr(datetime, 9,10),substr(datetime,12,13),substr(datetime,15,16))
    
    writeRaster(weekly_sum,paste0("radolan_168h_sum_",datetime_chr,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')
    
  }
  
}


  #writeRaster(weekly_sum,paste0("radolan_168h_sum_",datetime_chr,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')
  






# 
# 
# j=0
# 
# day1 = stack(radolan_files[1+j])
# day2 = stack(radolan_files_daily[2+j])
# day3 = stack(radolan_files_daily[3+j])
# day4 = stack(radolan_files_daily[4+j])
# day5 = stack(radolan_files_daily[5+j])
# day6 = stack(radolan_files_daily[6+j])
# day7 = stack(radolan_files_daily[7+j])
# day8 = stack(radolan_files[8+j])
# 
# 
# k=1
# 
# stack_week = stack(day1[[k:24]],day2,day3,day4,day5,day6,day7,day8[[1:k]])
# 
# stack_week = (stack_week[[1:30]])
# 
# names(stack_week)
# 
# weekly_sum = sum(stack_week)
# 
# plot(weekly_sum)
# 
# 
# #create empty time sequence with hourly resolution
# 
# start = as.POSIXct(strptime(202201010000,"%Y%m%d%H%M"))
# end = as.POSIXct(strptime(202212312300,"%Y%m%d%H%M"))
# 
# ts_seq = data.frame(seq.POSIXt(start, end, by="hour"))
# names(ts_seq)="datetime"
# 
# #ts_seq[168,]
# 
# #168+j*24+k-1
# 
# datetime = as.character(ts_seq[168+j*24+k-1,])
# 
# datetime_chr = paste0(substr(datetime, 1,4),substr(datetime, 6,7),substr(datetime, 9,10),substr(datetime,12,13),substr(datetime,15,16))
# 
# writeRaster(weekly_sum,paste0("radolan_168h_sum_",datetime_chr,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')
# 

########################################################################
########################################################################


#
# 
# 
# #writeRaster(stack_radolan_study_site,paste0("radolan_study_site_",i,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')
# 
# # i=1
# # 
# # radolan_stack = stack(radolan_files[i])
# # 
# # radolan_daily_sum = sum(radolan_stack[[1:24]])
# # 
# #plot(radolan_daily_sum)
# # 
# # date = radolan_date[i]
# # 
# # writeRaster(radolan_daily_sum,paste0("radolan_study_site_daily_sum_",date,".tif"),options=c('TFW=YES'),overwrite=TRUE,datatype='INT2U')
# 
# 
# 
# ###################
# ###################
# ##################
# 
# 
# #files <- radolan_files[grepl(20220515,radolan_files)]
# #radolan_daybefore1 = stack(files)
# #radolan_daybefore1
# 
# files <- stack(radolan_files[1:8])
# sum = sum(files[[1:168]])
# 
# plot(sum)
# 
# files[[167]]
# 
# 
# sum_24()
# 
# 
# 
# 
# #nur für 12 uhr
# 
# #i=20220215
# 
# daybefore1 = i-1
# daybefore2 = i-2
# daybefore3 = i-3
# daybefore4 = i-4
# daybefore5 = i-5
# 
# files_day<- radolan_files[grepl(i,radolan_files)]
# files_daybefore1<- radolan_files[grepl(daybefore1,radolan_files)]
# files_daybefore2<- radolan_files[grepl(daybefore2,radolan_files)]
# files_daybefore3<- radolan_files[grepl(daybefore3,radolan_files)]
# files_daybefore4<- radolan_files[grepl(daybefore4,radolan_files)]
# files_daybefore5<- radolan_files[grepl(daybefore5,radolan_files)]
# 
# radolan_day = stack(files_day)
# radolan_daybefore1 = stack(files_daybefore1)
# radolan_daybefore2 = stack(files_daybefore2)
# radolan_daybefore3 = stack(files_daybefore3)
# radolan_daybefore4 = stack(files_daybefore4)
# radolan_daybefore5 = stack(files_daybefore5)
# 
# 
# #daily_sum = sum(radolan_day/100)
# #plot(radolan_day/100)
# #plot(daily_sum)
# 
# radolan_now_spatially = radolan_day[[12]]/100
# radolan_3h_spatially = sum(radolan_day[[10:12]]/100)
# radolan_6h_spatially = sum(radolan_day[[7:12]]/100)
# radolan_12h_spatially = sum(radolan_day[[1:12]]/100)
# radolan_24h_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[12:24]]/100)
# radolan_2d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[12:24]]/100)
# radolan_3d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,radolan_daybefore3[[12:24]]/100)
# radolan_4d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
#                            radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[12:24]]/100)
# radolan_5d_spatially = sum(radolan_day[[1:12]]/100,radolan_daybefore1[[1:24]]/100,radolan_daybefore2[[1:24]]/100,
#                            radolan_daybefore3[[1:24]]/100,radolan_daybefore4[[1:24]]/100,radolan_daybefore5[[12:24]]/100)
# 
# 
# 
