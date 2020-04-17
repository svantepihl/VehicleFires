require(tidyverse)
require(survival)
require(ranger)
require(ggfortify)
require(lubridate))
require(anytime)


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
