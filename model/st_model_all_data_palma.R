
.libPaths("/home/m/m_baum34/r_packages/")

library(doParallel)
library("CAST")
library("caret")

load("/home/m/m_baum34/soil_temperature/train_set.Rdata")

cv_spacetimefolds <- CAST::CreateSpacetimeFolds(train_set,spacevar = "plot_id",timevar ="date", k=5)

hyperparameter = expand.grid(mtry = 2,
                             min.node.size = 5,
                             splitrule = "variance")


predictors <- c("air_temperature_mountain","precipitation","global_radiation",
                "relative_humidity","air_pressure","wind_speed","air_temperature_mountain_1","air_temperature_mountain_2",
                "air_temperature_mountain_3","air_temperature_mountain_6","air_temperature_mountain_12","air_temperature_mountain_24",
                "air_temperature_mountain_48","air_temperature_mountain_72","air_temperature_mountain_96","air_temperature_mountain_120",
                "soil_texture","soil_type","elevation","land_use","inclination","northness","eastness","topo_wetness","ndvi","ndwi","radar","depths")

response <- "soil_temperature" 


cl <- makeCluster(10)
registerDoParallel(cl)



rfmodel = caret::train(x =  train_set[,predictors],
                       y =   train_set[,response],
                       method = "ranger",
                       tuneGrid = hyperparameter,
                       num.trees = 100,
                       trControl = trainControl(method = "cv", number = 5,
                                                index = cv_spacetimefolds$index, 
                                                indexOut = cv_spacetimefolds$indexOut,
                                                savePredictions = "final"),
                       importance = "permutation")


ffsmodel = CAST::ffs(predictors =  train_set[,predictors],
                     response =   train_set[,response],
                     method = "ranger",
                     tuneGrid = hyperparameter,
                     num.trees = 100,
                     trControl = trainControl(method = "cv", number = 5,
                                              index = cv_spacetimefolds$index, 
                                              indexOut = cv_spacetimefolds$indexOut,
                                              savePredictions = "final"),
                     importance = "permutation")

save(rfmodel,file = "/home/m/m_baum34/soil_temperature/rf_model_spacetimefolds.Rdata")
save(ffsmodel,file = "/home/m/m_baum34/soil_temperature/ffs_model_spacetimefolds.Rdata")

stopCluster(cl)




