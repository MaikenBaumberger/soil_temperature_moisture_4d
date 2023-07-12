

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/soil_probes_data/list_probe_data.Rdata")

meteo_data = read.csv("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/meteo_data/meteo_data.csv")

meteo_data$datetime <- as.POSIXct(strptime(meteo_data$datetime,"%Y-%m-%d %H:%M:%S"))

##################################

for (n in names(list_probes))
  list_probes[[n]]['name'] = n
table_probes = do.call(rbind, list_probes)


##################################

#merge soil probe data and meteo data



table_probes = do.call("rbind",list_probes)





table_probes = dplyr::bind_rows(table_probes, .id = "variable")


table_probes = list_probes["variable"] = unlist(lapply(
  strsplit(row.names(list_probes), ".", fixed = TRUE), function(x) x[[1]])
)


for (n in names(list_probes))
  list_probes[[n]]['name'] = n
table_probes = do.call(rbind, list_probes)
