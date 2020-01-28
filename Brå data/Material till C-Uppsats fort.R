library(downloader)
library(ggplot2)
library(forecast)
library(tidyverse)

qua_sve <- ts(bilbrander_2015_2018_quarters_r[,1], start = c(2016, 1), frequency = 4)
qua_sve
auto.arima(qua_sve)
dec_qua_sve <-decompose(qua_sve)
autoplot (dec_qua_sve)
ggtsdisplay(qua_sve)
diff_qua_sve <- diff(qua_sve, lag = 4)
diff_diff_qua_sve<- diff(diff_qua_sve)
plot(diff_diff_qua_sve)


mon_sve <- ts(Bilbrander_2015_2018_months_r[,1], start = c(2016, 1), frequency = 12)
mon_sve
auto.arima(mon_sve)
dec_mon_sve <-decompose(mon_sve)
autoplot (dec_mon_sve)
ggtsdisplay(mon_sve)
diff_mon_sve <- diff(mon_sve, lag = 12)
ggtsdisplay(diff_mon_sve)
diff_diff_mon_sve<- diff(diff_mon_sve)
plot(diff_diff_mon_sve)


mon_nor <- ts(Bilbrander_2015_2018_months_r[,2], start = c(2016, 1), frequency = 12)
mon_nor
auto.arima(mon_nor)
dec_mon_nor <-decompose(mon_nor)
autoplot (dec_mon_nor)
ggtsdisplay(mon_nor)
diff_mon_nor <- diff(mon_nor, lag = 12)
ggtsdisplay(diff_mon_nor)
diff_diff_mon_nor<- diff(diff_mon_nor)
plot(diff_diff_mon_nor)

mon_mit <- ts(Bilbrander_2015_2018_months_r[,3], start = c(2016, 1), frequency = 12)
mon_mit
auto.arima(mon_mit)
dec_mon_mit <-decompose(mon_mit)
autoplot (dec_mon_mit)
ggtsdisplay(mon_mit)
diff_mon_mit <- diff(mon_mit, lag = 12)
ggtsdisplay(diff_mon_mit)
diff_diff_mon_mit<- diff(diff_mon_mit)
plot(diff_diff_mon_mit)

mon_sto <- ts(Bilbrander_2015_2018_months_r[,4], start = c(2016, 1), frequency = 12)
mon_sto
auto.arima(mon_sto)
dec_mon_sto <-decompose(mon_sto)
autoplot (dec_mon_sto)
ggtsdisplay(mon_sto)
diff_mon_sto <- diff(mon_sto, lag = 12)
ggtsdisplay(diff_mon_sto)
diff_diff_mon_sto<- diff(diff_mon_sto)
plot(diff_diff_mon_sto)
