require(tidyverse)
require(lubridate)
require(forecast)
require(tseries)
require(urca)


#Exploratory time series  
mega_list <- split.data.frame(dat_days_stockholm, as.factor(dat_days_stockholm$Municipality_Name))
sums <- means <- lapply(mega_list, "[", ,12) %>% lapply(sum)
means <- lapply(mega_list, "[", ,12) %>% lapply(mean)
means_dat <- as.data.frame(means)
means_dat <- t(means_dat)
colnames(means_dat) <- "average_number_fires"

mega_list$Botkyrka[12]%>% ts(frequency = 365)%>%auto.arima()
mega_list [[1]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[2]] [12] %>% ts() %>% auto.arima()
mega_list [[3]] [12] %>% ts() %>% auto.arima()
mega_list [[4]] [12] %>% ts() %>% auto.arima()
mega_list [[5]] [12] %>% ts() %>% auto.arima()
mega_list [[6]] [12] %>% ts() %>% auto.arima()
mega_list [[7]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[8]] [12] %>% ts() %>% auto.arima()
mega_list [[9]] [12] %>% ts() %>% auto.arima()
mega_list [[10]] [12] %>% ts() %>% auto.arima()
mega_list [[11]] [12] %>% ts() %>% auto.arima()
mega_list [[12]] [12] %>% ts() %>% auto.arima()
mega_list [[13]] [12] %>% ts() %>% auto.arima()
mega_list [[14]] [12] %>% ts() %>% auto.arima()
mega_list [[15]] [12] %>% ts() %>% auto.arima()
mega_list [[16]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[17]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[18]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[19]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[20]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[21]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[22]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[23]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list [[24]] [12] %>% ts() %>% auto.arima()
mega_list [[25]] [12] %>% ts() %>% auto.arima()
mega_list [[26]] [12] %>% ts(frequency = 365) %>% auto.arima()


mega_list [[1]] [12] %>% ts(frequency = 365) %>% decompose() %>% plot()
mega_list [[1]] [12] %>% ts(frequency = 365) %>% diff() %>% acf()

dat_months_stockholm%>%group_by(Year,Month) %>% select(dat_days_stockholm$Number_of_Fires_Month)%>%lapply(mean)

dat_means <- dat_months_stockholm[, c(2,3,17)] %>% group_by(Year,Month) %>% 
  summarize(Number_of_Fires=sum(Number_of_Fires_Month))%>% 
  as.data.frame

Fires_months <- ts(dat_means$Number_of_Fires, frequency = 12) 
autoplot(Fires_months)
Fires_months%>%diff()%>%auto.arima()
Fires_months%>%diff()%>%autoplot()

adf.test(Fires_months)
Fires_months%>%ur.kpss()%>%summary()
Fires_months%>%diff()%>%ur.kpss()%>%summary()

rm(means, means_dat, mega_list, sums, dat_means, Fires_months)
