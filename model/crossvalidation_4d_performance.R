

load("C:/Users/maike/Desktop/Carbon4D/GitHub_soil_temperature_moisture_4d_data/predictor_set.Rdata")


data = data.frame(predictor_set$T_05,predictor_set$air_temperature_mountain,predictor_set$land_use)
colnames(data) <- c("T_05","air_temperature_mountain","land_use")

data=data[complete.cases(data), ]

data$land_use = as.factor(data$land_use)

data$id = c(1:100937)

predictors <- c("air_temperature_mountain","land_use")
response <- "T_05"


IDs=unique(data$id)

random_sample = sample(IDs,size = length(IDs)*0.01)

data_subset = data[1:10000,]


library(caret)
library(CAST)

model <- train(data_subset[,predictors],
               data_subset[,response],
               method="rf",
               tuneLength=1,
               trControl=trainControl(method="cv",savePredictions = "final"))
model
#plot(model)

model$pred


#data_subset[,response]

model_prediction = data.frame(model$pred)

full_data = merge(data_subset,model_prediction,by.x = "id",by.y = "rowIndex")

plot(full_data$T_05,full_data$obs)

plot(full_data$T_05,full_data$pred)

full_data$dif= abs(full_data$T_05- full_data$pred)

plot(full_data$dif)

library(ggplot2)

ggplot(full_data, aes(x=land_use, y=dif, fill=land_use)) + 
  geom_boxplot()





