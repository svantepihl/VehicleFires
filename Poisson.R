require(tidyverse)
require(ggplot2)

dat_stockholm <- filter(dat_stockholm, dat_stockholm$Year > 2011)

# Number of cases by date and municipality 
dat_municipality_day <- dat_stockholm %>%
  group_by(Date, Municipality_Code, Municipality_Name) %>%
  count()


dat_municipality_day <- dat_municipality_day %>% arrange(Municipality_Name)


Botkyrka <- filter(dat_municipality_day, Municipality_Name == "Botkyrka")
mean(Botkyrka$n)
var(Botkyrka$n)


Date <- seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "day")
Date <- data.frame(Date)
Date$Municipality_Name = "Botkyrka"

Botkyrka$Date <- as.Date(Botkyrka$Date)
Botkyrka_Poisson <- left_join(Date, Botkyrka, by = "Date")

str(Date)
str(Botkyrka)
