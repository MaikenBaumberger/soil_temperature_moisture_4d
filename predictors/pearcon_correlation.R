


load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")

predictor_set = predictor_set[ , -which(names(predictor_set) %in% c("T_95","T_105","T_115","M_95","M_105","M_115","probe_name","date_hour","measurement","id","plot_id","start_date","end_date"))]

summary(predictor_set)

res = cor(predictor_set, use="complete.obs", method = "pearson") #pairwise.complete.obs

res = round(res, 2)

library(corrplot)
corrplot(res,method = 'color')

corrplot(res[1:20,21:47],method = 'color',tl.col = "black",na.label=" ")


#library("PerformanceAnalytics")
#chart.Correlation(res, histogram=TRUE, pch=19)


###########    


res = cor(predictor_set[60000:61000,], use="pairwise.complete.obs", method = "pearson")

res = round(res, 2)

corrplot(res,method = 'color')

corrplot(res[1:10,28:37],method = 'color',tl.col = "black",na.label=" ")


#library("PerformanceAnalytics")
#chart.Correlation(res, histogram=TRUE, pch=19)




plot(predictor_set$air_temperature_mountain[60000:60120],type="l",col="blue",lwd=2,ylim=c(-2,30),ylab="Temperature [°C]")
lines(predictor_set$T_05[60000:60120],col="red",lwd=2)
lines(predictor_set$T_15[60000:60120],col="orange",lwd=2)
lines(predictor_set$T_25[60000:60120],col="yellow",lwd=2)

