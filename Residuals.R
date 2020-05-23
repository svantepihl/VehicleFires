library(Metrics)



predictions <- read_excel("~/Desktop/predictions.xlsx")

# RMSE Continous
rmse(predictions$`Actual Vehicle Fires`,predictions$`Continuous Pooled`)
rmse(predictions$`Actual Vehicle Fires`,predictions$`Continuous Fixed Effect`)
rmse(predictions$`Actual Vehicle Fires`,predictions$`Continuous Random Effect`)


#RMSE Poisson
rmse(predictions$`Actual Vehicle Fires`,predictions$`Poisson Pooled`)
rmse(predictions$`Actual Vehicle Fires`,predictions$`Poisson Fixed Effect`)


# RMSE NegBin
rmse(predictions$`Actual Vehicle Fires`,predictions$`NegBin Pooled`)
rmse(predictions$`Actual Vehicle Fires`,predictions$`NegBin Fixed Effect`)


