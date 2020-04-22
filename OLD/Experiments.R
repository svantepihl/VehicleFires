require(tidyverse)
require(survival)
require(ranger)
require(ggfortify)
require(lubridate)
require(anytime)
require(forecast)
require(urca)
require(stats)

#Create dataset with only years 2012-2019
dat_subset_12_19 <- filter(dat_stockholm, dat_stockholm$Year >=2012 )


# Select only fires with suspected arson
dat_subset_12_19 <- filter(dat_subset_12_19, dat_subset_12_19$Reason == "Arson")
summary(dat_subset_12_19$Holidays)


#Data cleaning- create separate year, quarter, month, day, hour and minute variables. 
dat_stockholm$Year <- year(dat_stockholm$Date)
dat_stockholm$Quarter <- quarter(dat_stockholm$Date)
dat_stockholm$Month <- month(dat_stockholm$Date)
dat_stockholm$Day <- day(dat_stockholm$Date)
dat_stockholm$Hour <- hour(dat_stockholm$DateTime)
dat_stockholm$Minute <- minute(dat_stockholm$DateTime)
dat_stockholm$Week <- isoweek(dat_stockholm$Date)




# differences between holiday and non-holiday days
dat_stockholm$Year<- year(dat_stockholm$Date)
dat_stockholm$Weekday <- weekdays(dat_stockholm$Date)
x <- filter(dat_stockholm, dat_stockholm$Year == 2012| dat_stockholm$Year == 2013 | dat_stockholm$Year == 2014 | dat_stockholm$Year == 2015|dat_stockholm$Year == 2016|dat_stockholm$Year == 2017|dat_stockholm$Year == 2018)
x<- filter(x, x$Reason == "Arson" & x$Year == 2015)
x_1 <- filter(x, x$Holidays == TRUE | x$Weekday == "lördag" | x$Weekday == "söndag")
x_2 <- filter( x, x$Holidays == FALSE & x$Weekday != "lördag" & x$Weekday != "söndag")
x_3 <- filter(x, x$week == 13)


breaks_weekdays = seq(1, 7, by=1)
labels_weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dat_stockholm %>% 
  ggplot(aes(Week)) + geom_bar(aes(fill = as.factor(Week)))


x %>% 
  ggplot(aes(Week)) + geom_bar(aes(fill = as.factor(Week)))

+
  scale_x_discrete (breaks=breaks_weekdays, labels = labels_weekdays) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels_weekdays)

dat_msb$Year <- year(dat_msb$Date)
dat_msb$Quarter <- quarter(dat_msb$Date)
dat_msb$Month <- month(dat_msb$Date)
dat_msb$Day <- day(dat_msb$Date)
dat_msb$Hour <- hour(dat_msb$DateTime)
dat_msb$Minute <- minute(dat_msb$DateTime)
dat_msb$Week <- isoweek(dat_msb$Date)


dat_skåne <- filter(dat_msb, dat_msb$Region_Code == 12)
dat_göteborg <- filter(dat_msb, dat_msb$Region_Code == 14)
dat_stockholm <- filter(dat_msb, dat_msb$Region_Code == 01)
 
y <- filter(dat_göteborg, dat_göteborg$Year == 2012| dat_göteborg$Year == 2013 | dat_göteborg$Year == 2014 | dat_göteborg$Year == 2015|dat_göteborg$Year == 2016|dat_göteborg$Year == 2017|dat_göteborg$Year == 2018)
y <- filter(y, y$Reason == "Arson")

y %>%
  ggplot(aes(Week)) + geom_bar(aes(fill = as.factor(Week)))

z <- filter(dat_skåne, dat_skåne$Year == 2012| dat_skåne$Year == 2013 | dat_skåne$Year == 2014 | dat_skåne$Year == 2015|dat_skåne$Year == 2016|dat_skåne$Year == 2017|dat_skåne$Year == 2018)
z <- filter(z, z$Reason == "Arson")

z %>%
  ggplot(aes(Week)) + geom_bar(aes(fill = as.factor(Week)))

dat_huddinge <- filter(dat_msb, dat_msb$Municipality_Code == "0126")

h <- filter(dat_huddinge, dat_huddinge$Year== 2012| dat_huddinge$Year == 2013 | dat_huddinge$Year == 2014 | dat_huddinge$Year == 2015|dat_huddinge$Year == 2016|dat_huddinge$Year == 2017|dat_huddinge$Year == 2018)
h <- filter(dat_huddinge, dat_huddinge$Month == 08 & dat_huddinge$Year == 2018)
h %>%
  ggplot(aes(Day)) + geom_bar(aes(fill = as.factor(Day)))

dat_järfalla <- filter(dat_msb, dat_msb$Municipality_Code == "0123")

j <- filter(dat_järfalla, dat_järfalla$Year== 2012| dat_järfalla$Year == 2013 | dat_järfalla$Year == 2014 | dat_järfalla$Year == 2015|dat_järfalla$Year == 2016|dat_järfalla$Year == 2017|dat_järfalla$Year == 2018)
j <- filter(dat_järfalla, dat_järfalla$Month == 04 & dat_järfalla$Year == 2018)
j %>%
  ggplot(aes(Day)) + geom_bar(aes(fill = as.factor(Day)))


#ExplorTORY TIME series 
mega_list_2 <- split.data.frame(dat_days_stockholm, as.factor(dat_days_stockholm$Municipality_Name))
sums <- means <- lapply(mega_list_2, "[", ,12) %>% lapply(sum)
means <- lapply(mega_list_2, "[", ,12) %>% lapply(mean)
means_dat <- as.data.frame(means)
means_dat <- t(means_dat)
colnames(means_dat) <- "average_number_fires"

mega_list_2$Botkyrka[12]%>% ts(frequency = 365)%>%auto.arima()
mega_list_2 [[1]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[2]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[3]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[4]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[5]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[6]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[7]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[8]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[9]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[10]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[11]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[12]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[13]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[14]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[15]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[16]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[17]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[18]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[19]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[20]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[21]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[22]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[23]] [12] %>% ts(frequency = 365) %>% auto.arima()
mega_list_2 [[24]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[25]] [12] %>% ts() %>% auto.arima()
mega_list_2 [[26]] [12] %>% ts(frequency = 365) %>% auto.arima()

                                              
mega_list_2 [[1]] [12] %>% ts(frequency = 365) %>% decompose() %>% plot()
mega_list_2 [[1]] [12] %>% ts(frequency = 365) %>% diff() %>% acf()

str(dat_days_stockholm)

# exploratory models
dat_days_stockholm$Municipality_Name <- as.factor(dat_days_stockholm$Municipality_Name)
str(dat_days_stockholm)

a <- lm (dat_days_stockholm$Count.x ~ dat_days_stockholm$Municipality_Name + dat_days_stockholm$Temperature + dat_days_stockholm$Precipitation + dat_days_stockholm$Holidays + dat_days_stockholm$Weekday, dat_days_stockholm )
summary(a)
a$fitted.values
dat_days_stockholm$Count.x
plot(a$residuals)
cor(a$residuals, diff(a$residuals))
min(dat_days_stockholm$Count.x)
plot(dat_days_stockholm$Count.x)
cor(as.numeric(dat_days_stockholm$Municipality_Name), a$residuals)
dat_days_stockholm$ln_count <- log(dat_days_stockholm$Count.x)
dat_days_stockholm$ln_count[dat_days_stockholm$ln_count < 0] <- 0
var(dat_days_stockholm$Count.x)
mean(dat_days_stockholm$Count.x)

min(dat_days_stockholm$ln_count)
re <- diff(a$residuals)
cor(a$residuals[2:300], re[1:299])
plot(a$residuals [1:200])
