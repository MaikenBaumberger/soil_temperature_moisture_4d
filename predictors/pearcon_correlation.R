


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
