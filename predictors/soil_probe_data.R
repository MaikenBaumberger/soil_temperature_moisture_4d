
#create table of all soil probe data
#soil temperature and soil moisture

library(devtools)
library(Carbon4D)

dir= "C:/Users/maike/Desktop/Carbon4D/DownloadGitData"

#Carbon4D::get_carbon4d_data(dir)

#### weekly and monthly soil probe data ####
list_probes_m <- load_monthly_probe_data(dir)
list_probes_w <- load_weekly_probe_data(dir)

# names of probes
names_m <- substr(names(list_probes_m),1,nchar(names(list_probes_m))-11)
names(list_probes_m)=names_m
names_w <- substr(names(list_probes_w),1,nchar(names(list_probes_w))-11)
names(list_probes_w)=names_w


# Datum/Uhrzeit-Spalte ins passende Format bringen:
list_probes_m <- lapply(list_probes_m, transform, datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))
list_probes_w <- lapply(list_probes_w, transform, datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))

# Spalte mit "Monthly" anfuegen:
list_probes_m <- lapply(X = list_probes_m , FUN = cbind , "measurement"="monthly")
list_probes_w <- lapply(X = list_probes_w , FUN = cbind , "measurement"="weekly")

#combine datasets
list_probes <- c(list_probes_m,list_probes_w)
names <- c(names_m,names_w)


#####################################

# probe data hourly
list_probes_m <- lapply(list_probes_m, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H")))
list_probes_w <- lapply(list_probes_w, transform, date_hour = as.POSIXct(strptime(datetime, "%Y-%m-%d %H"))) 


list_probes_m= lapply(list_probes_m, transform, T_25_ave = ave(T_25, date_hour))

col_names = names(list_probes_m$S02_001)[2:27]

for (i in col_names){
  a = paste0(i,"_ave")
  list_probes_m = lapply(list_probes_m, transform,  i = ave(i, date_hour))
}

######################################


paste0(col_names[2],"_ave")



lapply(myList, function(x) { x["ID"] <- NULL; x })





















new = aggregate(list_probes_m$T_15, list_probes_m(list_probes_m$date_hour), FUN=mean) 




lapply(list_probes_m, function(x) aggregate(. ~ date_hour, data = x, FUN = "mean"))


list_probes_m_agg<-lapply(list_probes_m, function(i) {
  aggregate(i, by=list(Category=list_probes_m$date_hour), FUN=mean)
})


list_probes_m <- aggregate(list_probes_m, by=list(Category=list_probes_m$date_hour), FUN=mean)







Prec1Hourely <- aggregate(Prec1$Prec, by=list(Category=Prec1$date_hour), FUN=sum)
  



format(list_probes_m$datetime,'%Y-%m-%d')

Prec1$date <- format(Prec1$datetime,'%Y-%m-%d')
Prec1$hour <- format(Prec1$datetime,'%H %Z')
Prec1$date_hour <- paste(Prec1$date,Prec1$hour)
Prec1$date_hour <- format(strptime(Prec1$date_hour,"%Y-%m-%d %H"),"%Y-%m-%d %H:%M:%S %Z")
Prec1$date_hour <- as.POSIXct(strptime(Prec1$date_hour,"%Y-%m-%d %H:%M:%S"))
Prec1Hourely <- aggregate(Prec1$Prec, by=list(Category=Prec1$date_hour), FUN=sum)








