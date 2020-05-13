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

dat_months_stockholm_2018 <- subset(dat_months_stockholm,dat_months_stockholm$Year == 2018)




## Create dataset for models
dat_months_stockholm_full <- dat_months_stockholm

dat_months_stockholm <- dat_months_stockholm[ ,c(1,2,3,5,17,19,20,21,23)]

# Create first difference
loops <- 1::nrow(dat_months_stockholm)
for (i in 1:nrow(dat_months_stockholm)) {
  PastMonth <- as.yearmon(dat_months_stockholm$Date[i] - 1/12)
  PastYear <- as.yearmon(dat_months_stockholm$Date[i] - 1)
  tempMonth <- dat_months_stockholm[dat_months_stockholm$Date== PastMonth & dat_months_stockholm$Municipality_Name == dat_months_stockholm$Municipality_Name[i], ]
  if(nrow(tempMonth)!=0){
    dat_months_stockholm$Diff_Number_of_Fires_Month[i] <- dat_months_stockholm$Number_of_Fires_Month[i]- tempMonth$Number_of_Fires_Month
    dat_months_stockholm$Diff_Temperature[i] <- dat_months_stockholm$Temperature[i]- tempMonth$Temperature
    dat_months_stockholm$Diff_Median_Income_20plus[i] <- dat_months_stockholm$Median_Income_20plus[i] - tempMonth$Median_Income_20plus
    dat_months_stockholm$Diff_Total_Number_of_Residents[i] <- dat_months_stockholm$Total_Number_of_Residents[i] - tempMonth$Total_Number_of_Residents
    dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64[i] <- dat_months_stockholm$Percentage_of_Unemployed_18_64[i] - tempMonth$Percentage_of_Unemployed_18_64
  } else {
    dat_months_stockholm$Diff_Number_of_Fires_Month[i] <- NA
    dat_months_stockholm$Diff_Temperature[i] <- NA
    dat_months_stockholm$Diff_Median_Income_20plus[i] <- NA
    dat_months_stockholm$Diff_Total_Number_of_Residents[i] <- NA
    dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64[i] <- NA
  }
}

# Interpolate montly data from yearly change
for (i in 1:nrow(dat_months_stockholm)) {
  MunicipalityJan <- dat_months_stockholm[dat_months_stockholm$Year == dat_months_stockholm$Year[i] + 1 & dat_months_stockholm$Month  == 1 & dat_months_stockholm$Municipality_Name == dat_months_stockholm$Municipality_Name[i], ]
  if(nrow(MunicipalityJan)!=0){
    ResChange = MunicipalityJan$Diff_Total_Number_of_Residents/12
    UnEmpChange = MunicipalityJan$Diff_Percentage_of_Unemployed_18_64/12
    IncChange <- MunicipalityJan$Diff_Median_Income_20plus /12
    
    dat_months_stockholm$Diff_Total_Number_of_Residents[i] = ResChange
    dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64[i] = UnEmpChange
    dat_months_stockholm$Diff_Median_Income_20plus[i] = IncChange
    
    dat_months_stockholm$Total_Number_of_Residents[i] = dat_months_stockholm$Total_Number_of_Residents[i] + ResChange * as.numeric(dat_months_stockholm$Month[i])
    dat_months_stockholm$Percentage_of_Unemployed_18_64[i] = dat_months_stockholm$Percentage_of_Unemployed_18_64[i] + UnEmpChange * as.numeric(dat_months_stockholm$Month[i])
    dat_months_stockholm$Diff_Median_Income_20plus[i] = dat_months_stockholm$Diff_Median_Income_20plus[i] + IncChange * as.numeric(dat_months_stockholm$Month[i])
  } else {
    dat_months_stockholm$Diff_Total_Number_of_Residents[i] = NA
    dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64[i] = NA
    dat_months_stockholm$Diff_Median_Income_20plus[i] = NA
  }
}

#Remove 2018
dat_months_stockholm <- subset(dat_months_stockholm,dat_months_stockholm$Year != 2018)
