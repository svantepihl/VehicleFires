require(tidyverse)
require(ggplot2)
require(survival)
require(lubridate)
require(MASS)
require(lmtest)
require(xts)
require(plm)
require(zoo)
require(pglm)
require(readxl)
require(svMisc)


dat_months_stockholm <- dat_days_stockholm[, c(2, 4:13, 15:25)] %>% group_by(Year,Month, Municipality_Name, Municipality_Code) %>% 
  summarize(Temperature=mean(Temperature), Precipitation = mean(Precipitation), Holidays = sum(Holidays), Christmas_Holidays = sum(Christmas_Holidays),
            Sport_Holidays = sum(Sport_Holidays), Easter_Holidays = sum(Easter_Holidays),
            Summer_Holidays = sum(Summer_Holidays), Autumn_Holidays = sum(Autumn_Holidays),
            First_Quarter = mean(First_Quarter), Second_Quarter=mean(Second_Quarter), 
            Third_Quarter = mean(Third_Quarter),
            Fourth_Quarter = mean(Fourth_Quarter),
            Number_of_Fires = sum(Number_of_Fires),
            Number_Weekend_Days = sum(Weekend))%>% 
  as.data.frame




#Data cleaning for this file 
dat_months_stockholm$Date <- as.yearmon(paste(dat_months_stockholm$Year, dat_months_stockholm$Month), "%Y %m")
dat_months_stockholm$Date <- as.yearmon(paste(dat_months_stockholm$Year, dat_months_stockholm$Month), "%Y %m")

dat_months_stockholm <-merge (dat_months_stockholm, dat_stockholm_kolada,  by= c("Municipality_Name", "Year"), all.y=TRUE)


colnames(dat_months_stockholm) [17] <- "Number_of_Fires_Month"
dat_months_stockholm$Month <- as.factor(dat_months_stockholm$Month)


# Check for equal mean and variance
var(dat_months_stockholm$Number_of_Fires_Month, na.rm = TRUE)
mean(dat_months_stockholm$Number_of_Fires_Month, na.rm =TRUE)

# Check that the number of fires displayed in both columns is correct
sum(dat_months_stockholm$Number_of_Fires_Month, na.rm = TRUE)
sum(dat_months_stockholm$Number_of_Fires_Year, na.rm = TRUE) /12 


# Eliminate Na rows
dat_months_stockholm <- dat_months_stockholm [!(dat_months_stockholm$Municipality_Name == "Stockholms läns kommuner (ovägt medel)"),]
