require(tidyverse)
require(tscount)
require(lubridate)


dat_days_stockholm$Month <- month(dat_days_stockholm$Date)

# transorm the mega dataframe dat_days_stockholm in a list of data frames, where each municipality has its own data frame
# you can call the separate data frames through double square brackets, and single collumns or rows within singe data frames through
# single squared brackets, in the usual way . So for instance [[1]] [12] is the column "count x"- the number of car fires - in botkyrka
mega_list <- split.data.frame(dat_days_stockholm, as.factor(dat_days_stockholm$Municipality_Name))


# example botkyrka 
timeseries <- mega_list [[1]] [,12] # same as to say mega_list$botkyrka [,12]
covariate <- mega_list [[1]] [ ,c(3:6,17)]
covariate$Weekday <- covariate$Weekday %>% factor(levels = c("måndag","tisdag", "onsdag", "torsdag", "fredag", "lördag", "söndag"))%>% as.integer() %>% as.numeric()
covariate$Holidays <- covariate$Holidays %>% factor(levels= c("FALSE", "TRUE"))%>% as.integer() %>% as.numeric()
str(covariate$Weekday)
str(covariate$Holidays)


ts_model <- tsglm(timeseries, model = list(past_obs = c (1, 2)), link = "log", distr = "poisson", xreg = covariate)
summary(ts_model)
plot(ts_model)


ts_model_2 <- tsglm(timeseries, model = list(past_obs = c (1, 2)), link = "log", distr = "nbin", xreg = covariate)
summary(ts_model)
plot(ts_model)

# I do not really know what whether the models are good atm. 
# The covariates ,though, do not look very useful as none of them is statistically significant (0 effect within CI) apart for weekday.Month is not significant atm cos i haven't succeeded yet getting around the fact that dummies are not allowed in the model. 
dat_days_stockholm[[1]] [12]
