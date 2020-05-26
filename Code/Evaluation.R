require(surveillance)
require(readxl)
require(tscount)
require(Metrics)
require(lubridate)



citation(package = "surveillance")


predictions <- read_excel("DATA/predictions.xlsx")

predictions <- subset(predictions,year(predictions$Date)==2018)

### Continuous Pooled
# predictions$`Continuous Pooled`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`Continuous Pooled`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`Continuous Pooled`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`Continuous Pooled`, which = c("logs", "dss")))

### Continuous Random
#predictions$`Continuous Random Effect`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`Continuous Random Effect`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`Continuous Random Effect`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`Continuous Random Effect`, which = c("logs", "dss")))


### Continuous fixed
# predictions$`Continuous Fixed Effect`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`Continuous Fixed Effect`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`Continuous Fixed Effect`)
#MSE 
mse(predictions$`Actual Vehicle Fires`, predictions$`Continuous Fixed Effect`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`Continuous Fixed Effect`, which = c("logs", "dss")))





### Poisson Pooled
# predictions$`Poisson Pooled`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`Poisson Pooled`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`Poisson Pooled`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`Poisson Pooled`, which = c("logs", "dss")))

### PoissonFixed
# predictions$`Poisson Fixed Effect`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`Poisson Fixed Effect`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`Poisson Fixed Effect`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`Poisson Fixed Effect`, which = c("logs", "dss")))


###NegBin Pooled = 3.5316
#predictions$`NegBin Pooled`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`NegBin Pooled`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`NegBin Pooled`)
#MSE 
mse(predictions$`Actual Vehicle Fires`, predictions$`NegBin Pooled`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`NegBin Pooled`, size = 3.5316 ,which = c("logs", "dss")))


### NegBin Fixed = 12.7824
#predictions$`NegBin Fixed Effect`
#RMSE
rmse(predictions$`Actual Vehicle Fires`, predictions$`NegBin Fixed Effect`)
#MAE
mae(predictions$`Actual Vehicle Fires`, predictions$`NegBin Fixed Effect`)
# Dss & logs
summary(scores(predictions$`Actual Vehicle Fires`, predictions$`NegBin Fixed Effect`, size = 12.7824 ,which = c("logs", "dss")))






