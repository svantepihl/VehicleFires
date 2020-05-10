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
require(forecast)
require(stats)




dat_months_stockholm_arson <- dat_days_stockholm_arson[, c(2, 4:13, 15:25)] %>% group_by(Year,Month, Municipality_Name, Municipality_Code) %>% 
  summarize(Temperature=mean(Temperature), Precipitation = mean(Precipitation), Holidays = sum(Holidays), Christmas_Holidays = sum(Christmas_Holidays),
            Sport_Holidays = sum(Sport_Holidays), Easter_Holidays = sum(Easter_Holidays),
            Summer_Holidays = sum(Summer_Holidays), Autumn_Holidays = sum(Autumn_Holidays),
            First_Quarter = mean(First_Quarter), Second_Quarter=mean(Second_Quarter), 
            Third_Quarter = mean(Third_Quarter),
            Fourth_Quarter = mean(Fourth_Quarter),
            Number_of_Fires = sum(Number_of_Fires),
            Number_Weekend_Days = sum(Weekend))%>% 
  as.data.frame


# Merge with Kolada

dat_months_stockholm_arson$Date <- as.yearmon(paste(dat_months_stockholm_arson$Year, dat_months_stockholm_arson$Month), "%Y %m")
dat_months_stockholm_arson <-merge (dat_months_stockholm_arson, dat_stockholm_kolada,  by= c("Municipality_Name", "Year"), all.y=TRUE)


colnames(dat_months_stockholm_arson) [17] <- "Number_of_Fires_Month"
dat_months_stockholm_arson$Month <- as.factor(dat_months_stockholm_arson$Month)



# Eliminate Na rows
dat_months_stockholm <- dat_months_stockholm [!(dat_months_stockholm$Municipality_Name == "Stockholms läns kommuner (ovägt medel)"),]
