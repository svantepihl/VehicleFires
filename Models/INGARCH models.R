require(tidyverse)
require(tscount)
require(lubridate)


mega_list <- split.data.frame(dat_days_stockholm, as.factor(dat_days_stockholm$Municipality_Name))


# example botkyrka 
timeseries <- mega_list [[1]] [,12] # same as to say mega_list$botkyrka [,12]
covariate <- Botkyrka [ ,c(4,20:22,26:39)]


ts_model <- tsglm(timeseries, model = list(past_obs = c (1, 2)), link = "log", distr = "poisson", xreg = covariate)
summary(ts_model)
plot(ts_model)


ts_model_2 <- tsglm(timeseries, model = list(past_obs = c (1, 2)), link = "log", distr = "nbin", xreg = covariate)
summary(ts_model_2)
plot(ts_model_2)

# I do not really know how good those models are atm. 


#Predictions example
preds_botkyrka <- ts_model_2$fitted.values

high_risk_botkyrka = numeric(2557)   
# && is 'and'; or is || 
for(i in 1:2557){
  if(preds_botkyrka[i] > 0.30 && Botkyrka$Number_of_Fires[i] > 1){  # && means "and". The statement in if() will only be TRUE if both booleans are true
    # this is if model predicts 1, and the true value is 1
    high_risk_botkyrka[i] = 1
  } 
  else if(preds_botkyrka[i] < 0.30 && Botkyrka$Number_of_Fires[i] > 1){
    # this is if the model predicts 0, and the true value is 0
    high_risk_botkyrka[i] = -1
  }
  #else if(preds_botkyrka[i] < 0.45 && Botkyrka$Number_of_Fires[i] == 0){
     #this is if the model predicts 0, and the true value is 0
   # high_risk_botkyrka[i] = 0
  #}
}
high_risk_botkyrka
summary(high_risk_botkyrka)
summary(as.factor(high_risk_botkyrka))
mean(preds_botkyrka > 0.3)
summary(Botkyrka$Number_of_Fires)
summary(as.factor(Botkyrka$Number_of_Fires))
mean(preds > 0.3)
mean(preds)
57/122
