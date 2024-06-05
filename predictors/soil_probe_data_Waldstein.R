
#create table of all soil probe data
#soil temperature and soil moisture

library(devtools)
library(Carbon4D)

dir= "C:/Users/maike/Desktop/Carbon4D/DownloadGitData"

#Carbon4D::get_carbon4d_data(dir)

#### weekly and monthly soil probe data ####
list_probes_m <- load_monthly_probe_data(dir)
list_probes_w <- load_weekly_probe_data(dir)
list_probes_l <- load_longterm_probe_data(dir)

# names of probes
names_m <- substr(names(list_probes_m),1,nchar(names(list_probes_m))-11)
names(list_probes_m)=names_m
names_w <- substr(names(list_probes_w),1,nchar(names(list_probes_w))-11)
names(list_probes_w)=names_w
names_l <- substr(names(list_probes_l),1,nchar(names(list_probes_l))-11)
names(list_probes_l)=names_l


# Datum/Uhrzeit-Spalte ins passende Format bringen:
list_probes_m <- lapply(list_probes_m, transform, datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))
list_probes_w <- lapply(list_probes_w, transform, datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))
list_probes_l <- lapply(list_probes_l, transform, datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))

#
list_probes_m <- lapply(list_probes_m, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H")))
list_probes_w <- lapply(list_probes_w, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H")))
list_probes_l <- lapply(list_probes_l, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H")))

# Spalte mit "Monthly" anfuegen:
list_probes_m <- lapply(X = list_probes_m , FUN = cbind , "measurement"="monthly")
list_probes_w <- lapply(X = list_probes_w , FUN = cbind , "measurement"="weekly")
list_probes_l <- lapply(X = list_probes_l , FUN = cbind , "measurement"="weekly")


#combine datasets
#list_probes <- c(list_probes_m,list_probes_w)
list_probes <- list_probes_l
names <- c(names_l)


#####################################

# probe data hourly
#list_probes <- lapply(list_probes, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H")))

list_probes = lapply(list_probes, transform, T_org = ave(T_05, date_hour))
list_probes = lapply(list_probes, transform, T_05 = ave(T_05, date_hour))
list_probes = lapply(list_probes, transform, T_15 = ave(T_15, date_hour))
list_probes = lapply(list_probes, transform, T_25 = ave(T_25, date_hour))
list_probes = lapply(list_probes, transform, T_35 = ave(T_35, date_hour))
list_probes = lapply(list_probes, transform, T_45 = ave(T_45, date_hour))
list_probes = lapply(list_probes, transform, T_55 = ave(T_55, date_hour))
list_probes = lapply(list_probes, transform, T_65 = ave(T_65, date_hour))
list_probes = lapply(list_probes, transform, T_75 = ave(T_75, date_hour))
list_probes = lapply(list_probes, transform, T_85 = ave(T_85, date_hour))
list_probes = lapply(list_probes, transform, T_95 = ave(T_95, date_hour))
list_probes = lapply(list_probes, transform, T_105 = ave(T_105, date_hour))
list_probes = lapply(list_probes, transform, T_115 = ave(T_115, date_hour))

list_probes = lapply(list_probes, transform, M_org = ave(M_org, date_hour))
list_probes = lapply(list_probes, transform, M_05 = ave(M_05, date_hour))
list_probes = lapply(list_probes, transform, M_15 = ave(M_15, date_hour))
list_probes = lapply(list_probes, transform, M_25 = ave(M_25, date_hour))
list_probes = lapply(list_probes, transform, M_35 = ave(M_35, date_hour))
list_probes = lapply(list_probes, transform, M_45 = ave(M_45, date_hour))
list_probes = lapply(list_probes, transform, M_55 = ave(M_55, date_hour))
list_probes = lapply(list_probes, transform, M_65 = ave(M_65, date_hour))
list_probes = lapply(list_probes, transform, M_75 = ave(M_75, date_hour))
list_probes = lapply(list_probes, transform, M_85 = ave(M_85, date_hour))
list_probes = lapply(list_probes, transform, M_95 = ave(M_95, date_hour))
list_probes = lapply(list_probes, transform, M_105 = ave(M_105, date_hour))
list_probes = lapply(list_probes, transform, M_115 = ave(M_115, date_hour))


#col_names = names(list_probes_m$S02_001)[2:27]
#
#for (i in col_names){
#  list_probes_m = lapply(list_probes_m, transform, i = ave(i, date_hour))
#}


######################################

#delete rows of minute resolution time

names(list_probes$S01_014)

list_probes <- lapply(list_probes, function(x) x[,-1])

names(list_probes$S01_014)

######################################

#delete double rows

list_probes = lapply(list_probes, function(x) unique(x,by=date_hour))


######################################

######################################


#save(list_probes,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_probes_data/list_probe_data.Rdata")
save(list_probes,file = "C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_probes_data/list_probe_data_longterm.Rdata")









