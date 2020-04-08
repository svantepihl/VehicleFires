require(tidyverse)
require(ggplot2)
require(survival)

dat_stockholm <- filter(dat_stockholm, dat_stockholm$Year > 2011)

# Number of cases by date and municipality 
dat_municipality_day <- dat_stockholm %>%
  group_by(Date, Municipality_Code, Municipality_Name) %>%
  count()


dat_municipality_day <- dat_municipality_day %>% arrange(Municipality_Name)


Botkyrka <- filter(dat_municipality_day, Municipality_Name == "Botkyrka")

Date <- seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "day")
Date <- data.frame(Date)
Date$Municipality_Name = "Botkyrka"

Botkyrka$Date <- as.Date(Botkyrka$Date)
Botkyrka_Poisson <- left_join(Date, Botkyrka, by = c("Date", "Municipality_Name"))
Botkyrka_Poisson$Municipality_Code <- "0127"
Botkyrka_Poisson$n[is.na(Botkyrka_Poisson$n)] <- 0
str(Date)
str(Botkyrka)
dat_stockholm$Date <- as.Date(dat_stockholm$Date)

summary(as.factor(Botkyrka_Poisson$n))

Botkyrka_Poisson <- left_join(Botkyrka_Poisson,dat_stockholm[ ,c(1,4,10:19)], by=c("Date", "Municipality_Code"))

summary(m1 <- glm(n ~ prog + math, family="poisson", data=p))





for (i in 1:nrow(Botkyrka_Poisson)) {
  if(is.na(Botkyrka_Poisson$Weekday[i])) {
    Botkyrka_Poisson$Weekday[i] <- weekdays.Date(Botkyrka_Poisson$Date[i])
  }
  
  Botkyrka_Poisson$Region_Code[i] <- "1"
  
  for(j in 1:nrow(dat_holidays)){
    if(year(Botkyrka_Poisson$Date[i]) == dat_holidays$Year[j]) {
      if(Botkyrka_Poisson$Municipality_Code[i] == dat_holidays$Municipality_Code[j]) {
        # Check christmas holidays (start of year)
        if(Botkyrka_Poisson$Date[i] <= dat_holidays$Christmas_Holidays_End[j]) {
          Botkyrka_Poisson$Christmas_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
        # Check sport holidays
        if(Botkyrka_Poisson$Date[i] >= dat_holidays$Sport_Holidays_Start[j] & Botkyrka_Poisson$Date[i] <= dat_holidays$Sport_Holidays_End[j]) {
          Botkyrka_Poisson$Sport_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
        # Check Easter holidays
        if(Botkyrka_Poisson$Date[i] >= dat_holidays$Easter_Holidays_Start[j] & Botkyrka_Poisson$Date[i] <= dat_holidays$Easter_Holidays_End[j]) {
          Botkyrka_Poisson$Easter_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
        # Check Summer holidays
        if(Botkyrka_Poisson$Date[i] >= dat_holidays$Summer_Holidays_Start[j] & Botkyrka_Poisson$Date[i] <= dat_holidays$Summer_Holidays_End[j]) {
          Botkyrka_Poisson$Summer_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
        # Check autumn holidays
        if(Botkyrka_Poisson$Date[i] >= dat_holidays$Autumn_Holidays_Start[j] & Botkyrka_Poisson$Date[i] <= dat_holidays$Autumn_Holidays_End[j]) {
          Botkyrka_Poisson$Autumn_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
        # Check christmas holidays (end of year)
        if(Botkyrka_Poisson$Date[i] >= dat_holidays$Christmas_Holidays_Start[j]) {
          Botkyrka_Poisson$Christmas_Holidays[i] <- TRUE
          Botkyrka_Poisson$Holidays[i] <- TRUE
          break()
        }
      }
    }
  }
}

dat_weather_poisson <- dat_weather
dat_weather_poisson$Date <- as.Date(dat_weather_poisson$Date)
Botkyrka_Poisson[is.na(Botkyrka_Poisson)] <- F

Botkyrka_Poisson <- left_join(Botkyrka_Poisson[,-c(13,14)], dat_weather_poisson, by=c("Date" = "Date", "Municipality_Code" = "Municipality_Code"))






