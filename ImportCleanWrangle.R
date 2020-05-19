require(tidyverse)
require(readxl)
require(lubridate)
#require(svMisc)

dat_msb <- read_excel("MSB/msb_2019.xlsx", col_types = c("date", 
                                                         "skip", "date", "text", "skip", "text", 
                                                         "text", "numeric", "text"))

#encoding = "ISO-8859-1" <- För leo att lägga till 

#colnames
colnames(dat_msb) <- c("Date","DateTime", "isCar", "Municipality_Code", "Municipality_Name", "Type_of_Municipality_Code", "Reason")



#Do we want to have only cars or all type of vehicles? If we want to eliminate the other veichles add code here 
#Data cleaning - create separate year, quarter, month, day, hour and minute variables. 
#at_msb$Year <- year(dat_msb$Date)
#dat_msb$Quarter <- quarter(dat_msb$Date)
dat_msb$Month <- month(dat_msb$Date)
#dat_msb$Day <- day(dat_msb$Date)
dat_msb$Weekday <- weekdays(dat_msb$Date)
#dat_msb$Hour <- hour(dat_msb$Date)
#dat_msb$Minute <- minute(dat_msb$Date)
dat_msb$Region_Code <- as.integer(as.integer(dat_msb$Municipality_Code)/100)
dat_msb$Week <- isoweek(dat_msb$Date)
dat_msb$Date <- as.Date(dat_msb$Date)
dat_msb$isCar[dat_msb$isCar=="Ja"] <- 1
dat_msb$isCar[dat_msb$isCar=="Nej"] <- 0


# We rename the Reasons behind the carfires to english
dat_msb$Reason[dat_msb$Reason != "Fel i utrustning"& dat_msb$Reason != "Avsiktlig brand"& dat_msb$Reason != "Okänd"] <-"Other Reason"
dat_msb$Reason[dat_msb$Reason == "Avsiktlig brand"] <- "Arson"
dat_msb$Reason[dat_msb$Reason == "Fel i utrustning"] <- "Technical Malfunctioning"
dat_msb$Reason[dat_msb$Reason == "Okänd"] <- "Unknown"
dat_msb$Reason <- as.factor(dat_msb$Reason)


#Remove NA values
dat_msb <- na.omit(dat_msb)

# Extract municipality info
dat_msb$Count <- 1

#temp <- subset(dat_msb, dat_msb$Reason=="Other Reason") 
#Dat_Municipalities_OtherReason <- aggregate(Count~Municipality_Code+Municipality_Name+Type_of_Municipality_Code+Region_Code,temp, FUN = sum)

#temp <- subset(dat_msb, dat_msb$Reason=="Arson") 
#Dat_Municipalities_Arson <- aggregate(Count~Municipality_Code+Municipality_Name+Type_of_Municipality_Code+Region_Code,temp, FUN = sum)

#temp <- subset(dat_msb, dat_msb$Reason=="Technical Malfunctioning") 
#Dat_Municipalities_Tech <- aggregate(Count~Municipality_Code+Municipality_Name+Type_of_Municipality_Code+Region_Code,temp, FUN = sum)

#temp <- subset(dat_msb, dat_msb$Reason=="Unknown") 
#Dat_Municipalities_Unkown <- aggregate(Count~Municipality_Code+Municipality_Name+Type_of_Municipality_Code+Region_Code,temp, FUN = sum)
#remove(temp)

Dat_Municipalities <- aggregate(Count~Municipality_Code+Municipality_Name+Type_of_Municipality_Code+Region_Code,dat_msb, FUN = sum)

#write.csv(Dat_Municipalities_AllFires, "Dat_Municipalities_AllFires.csv")
#write.csv(Dat_Municipalities_Arson, "Dat_Municipalities_Arson.csv")
#write.csv(Dat_Municipalities_OtherReason, "Dat_Municipalities_Other.csv")
#write.csv(Dat_Municipalities_Tech, "Dat_Municipalities_Tech.csv")
#write.csv(Dat_Municipalities_Unkown, "Dat_Municipalities_Unkown.csv") 

#For use in Kolada

dat_msb_for_Kolada <- dat_msb


#Stockholm
dat_msb <- subset(dat_msb, year(dat_msb$Date)>2004)


#Remove municipality info from dat_msb
dat_msb <- dat_msb[,-c(5,6,8,9)]

Municipalities <- c("0127", "0162", "0125", "0136", "0126", "0123","0186", "0182","0188","0140","0192", "0128", "0191", "0163", "0184", "0180", "0183","0181", "0138", "0160", "0139", "0114", "0115", "0187", "0120","0117")

Dates <- data.frame(Date=as.Date(character(), format="%Y-%m-%d"),
                    Municipality_Code=character(),
                    stringsAsFactors=F) 


for (i in 1:length(Municipalities)) {
  temp <- data.frame(seq(as.Date("2005/1/1"), as.Date("2019/12/31"), "day"))
  temp[,2] <- Municipalities[i]
  names(temp) <- c("Date", "Municipality_Code")
  Dates <- rbind(Dates, temp)
  rm(temp)
}

#### Add weekday info
for (i in 1:nrow(Dates)) {
  Dates$Weekday[i] <- weekdays.Date(Dates$Date[i])
}


#### ADD WEATHER
dat_weather <- read_excel("Weather/Weather.xlsx", 
                          sheet = "ALL", col_types = c("skip", 
                                                       "text", "date", "text", "text", 
                                                       "skip"))

dat_weather$Date <- as.Date(dat_weather$Date)

dat_weather_2019 <- subset(dat_weather, year(dat_weather$Date) == 2018)
dat_weather_2019$Date <- dat_weather_2019$Date + years(1)
dat_weather <- full_join(dat_weather,dat_weather_2019)
rm(dat_weather_2019)
Dates <- left_join(Dates, dat_weather, by=c("Date","Municipality_Code"))

Dates$Temperature <- as.numeric(Dates$Temperature)
Dates$Precipitation <- as.numeric(Dates$Precipitation)

#### Load holiday info
# Create columns for holiday info YES/NO
Dates$Holidays <- numeric(length(Dates$Date))
Dates$Holidays <- as.logical(Dates$Holidays)

Dates$Christmas_Holidays <- numeric(length(Dates$Date))
Dates$Christmas_Holidays <- as.logical(Dates$Christmas_Holidays)

Dates$Sport_Holidays <- numeric(length(Dates$Date))
Dates$Sport_Holidays <- as.logical(Dates$Sport_Holidays)

Dates$Easter_Holidays <- numeric(length(Dates$Date))
Dates$Easter_Holidays <- as.logical(Dates$Easter_Holidays)

Dates$Summer_Holidays <- numeric(length(Dates$Date))
Dates$Summer_Holidays <- as.logical(Dates$Summer_Holidays)

Dates$Autumn_Holidays <- numeric(length(Dates$Date))
Dates$Autumn_Holidays <- as.logical(Dates$Autumn_Holidays)

dat_holidays <- read_excel("Holidays/Skollov.xlsx", 
                           col_types = c("numeric", "text", "text", 
                                         "date", "date", "date", "date", 
                                         "date", "date", "date", 
                                         "date", "date", "date"), na = "NA")

#Remove NA values
dat_holidays <- na.omit(dat_holidays)

# Loop through datset and check if a valid holiday value
for(i in 1:nrow(Dates)) {
  for(j in 1:nrow(dat_holidays)){
    if(year(Dates$Date[i]) == dat_holidays$Year[j]) {
      if(Dates$Municipality_Code[i] == dat_holidays$Municipality_Code[j]) {
        # Check christmas holidays (start of year)
        if(Dates$Date[i] <= dat_holidays$Christmas_Holidays_End[j]) {
          Dates$Christmas_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
        # Check sport holidays
        if(Dates$Date[i] >= dat_holidays$Sport_Holidays_Start[j] & Dates$Date[i] <= dat_holidays$Sport_Holidays_End[j]) {
          Dates$Sport_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
        # Check Easter holidays
        if(Dates$Date[i] >= dat_holidays$Easter_Holidays_Start[j] & Dates$Date[i] <= dat_holidays$Easter_Holidays_End[j]) {
          Dates$Easter_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
        # Check Summer holidays
        if(Dates$Date[i] >= dat_holidays$Summer_Holidays_Start[j] & Dates$Date[i] <= dat_holidays$Summer_Holidays_End[j]) {
          Dates$Summer_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
        # Check autumn holidays
        if(Dates$Date[i] >= dat_holidays$Autumn_Holidays_Start[j] & Dates$Date[i] <= dat_holidays$Autumn_Holidays_End[j]) {
          Dates$Autumn_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
        # Check christmas holidays (end of year)
        if(Dates$Date[i] >= dat_holidays$Christmas_Holidays_Start[j]) {
          Dates$Christmas_Holidays[i] <- TRUE
          Dates$Holidays[i] <- TRUE
          break()
        }
      }
    }
  }
}


# Subset fires in Stockholm
dat_stockholm <- filter(dat_msb, as.integer(as.integer(dat_msb$Municipality_Code)/100) == 01)




FireCount <- aggregate(Count~Date+Municipality_Code, dat_stockholm, FUN = sum)
Dates <- left_join(Dates,FireCount, by=c("Date", "Municipality_Code"))
Dates$Count[is.na(Dates$Count)] <- 0

dat_fires_stockholm <- left_join(dat_stockholm[ ,-c(6)], Dates, by=c("Date", "Municipality_Code"))
dat_days_stockholm <- left_join(Dates,Dat_Municipalities[,-6], by=c("Municipality_Code"))

#rm(FireCount,dat_holidays,dat_weather,i,j,Municipalities,dat_stockholm, dat_msb)



# cleaning  necessary for municipality based models and time-series model , non-panel data. Use this part of cleaning also for the INGARCH models.
colnames(dat_days_stockholm) [12] <- "Number_of_Fires"
colnames(dat_days_stockholm)[16] <- "Region_Code"

dat_days_stockholm$Week <- isoweek(dat_days_stockholm$Date)
dat_days_stockholm$Year <- year(dat_days_stockholm$Date)
dat_days_stockholm$Month <- month(dat_days_stockholm$Date)
dat_days_stockholm$Quarter <- quarter(dat_days_stockholm$Date)

split(dat_days_stockholm, as.factor(dat_days_stockholm$Weekday)) %>%lapply( "[", ,12)%>%lapply(mean)
split(dat_days_stockholm, as.factor(dat_days_stockholm$Month)) %>%lapply( "[", ,12)%>%lapply(mean)
split(dat_days_stockholm, as.factor(dat_days_stockholm$Quarter)) %>%lapply( "[", ,12)%>%lapply(mean)

dat_days_stockholm$First_Quarter <- 0
dat_days_stockholm$Second_Quarter <- 0
dat_days_stockholm$Third_Quarter <-0
dat_days_stockholm$Fourth_Quarter <- 0
dat_days_stockholm$Weekend <- 0

dat_days_stockholm[dat_days_stockholm$Quarter == 1, "First_Quarter"] <- 1
dat_days_stockholm[dat_days_stockholm$Quarter == 2, "Second_Quarter"] <- 1
dat_days_stockholm[dat_days_stockholm$Quarter == 3, "Third_Quarter"] <- 1
dat_days_stockholm[dat_days_stockholm$Quarter == 4, "Fourth_Quarter"] <- 1
dat_days_stockholm[dat_days_stockholm$Weekday == "lördag" | dat_days_stockholm$Weekday == "söndag", "Weekend"] <- 1



require(tidyverse)
require(reshape2)
require(reshape)
require(MASS)
require(readxl)

Kolada_2005 <- read_excel("Kommundata/Kolada_2005.xlsx", 
                          skip = 1)

Kolada_2005 <- data.frame(Kolada_2005[1:2], stack(Kolada_2005[3:ncol(Kolada_2005)]))
colnames(Kolada_2005) <- c("Variable", "Municipality", "Values", "Year")
Kolada_2005 <- Kolada_2005%>% arrange(Kolada_2005$Municipality)
Kolada_2005 <- reshape(data=Kolada_2005,idvar= c("Municipality", "Year"),
                       v.names = "Values",
                       timevar = "Variable",
                       direction="wide")

colnames(Kolada_2005) <- c("Municipality_Name", "Year", "Percentage_of_Unemployed_18_64", "Total_Number_of_Residents", "Percentage_of_Unemployed_or_in_Unemployemt_Programs_16_64","Median_Income_20plus")

Kolada_2005$Year <- Kolada_2005$Year%>% as.character()%>%as.integer()

Kolada_2005 <- Kolada_2005 [!(Kolada_2005$Municipality_Name == "Stockholms l�ns kommuner (ov�gt medel)"),]  

dat_stockholm_kolada <- filter(Kolada_2005, Kolada_2005$Year < 2019)
#rm(Kolada_2005, Kolada_2005_kolada, Dates, dat_msb_for_Kolada, dat_fires_stockholm, Dat_Municipalities)

#Number_of_Fires <- data.frame(dat_msb_for_Kolada %>%
#group_by(year(Date), Municipality_Code, Municipality_Name) %>%
#count())

#colnames(Number_of_Fires) <- c("Year", "Municipality_code", "Municipality_Name", "Number_of_Fires")
#Number_of_Fires$Year <- as.factor(Number_of_Fires$Year)

#Kolada_2005 <- left_join(Kolada_2005, Number_of_Fires, by=c("Municipality_Name", "Year"))

#Kolada_2005$Number_of_Fires [is.na(Kolada_2005$Number_of_Fires)]<- 0
#Kolada_2005$Number_of_Fires_per_Thousand <- (Kolada_2005$Number_of_Fires/Kolada_2005$Total_Number_of_Residents) * 1000

#Kolada_2005$Municipality_code<- as.integer(Kolada_2005$Municipality_code)
#Kolada_2005_kolada <- filter(Kolada_2005, Municipality_code < 200)

#Kommuner_stockholm$Year <- Kommuner_stockholm$Year%>% as.character()%>%as.integer()


#dat_stockholm_kolada <- left_join(Kommuner_stockholm, dat_days_stockholm_kolada)

#rm(Kommuner_stockholm, dat_days_stockholm_kolada, dat_msb_for_Kolada, Kolada_2005)


#dat_stockholm_kolada <- left_join(dat_kolada_3, dat_stockholm_kolada, by = c("Year", "Municipality"))
#rm(dat_kolada_3)

#rm(Number_of_Fires, Dat_Municipalities, Kommuner)

#colnames(dat_stockholm_kolada) [1] <- "Municipality_Name"
#colnames(dat_stockholm_kolada) [30] <-"Number_of_Fires_Year"


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


## Create dataset for models
dat_months_stockholm <- dat_months_stockholm[ ,c(1,2,3,5,17,19,20,21,23)]

# Create first difference
n <- nrow(dat_months_stockholm)
for (i in 1:n) {
  PastMonth <- as.yearmon(dat_months_stockholm$Date[i] - 1/12)
  PastYear <- as.yearmon(dat_months_stockholm$Date[i] - 1)
  tempMonth <- dat_months_stockholm[dat_months_stockholm$Date== PastMonth & dat_months_stockholm$Municipality_Name == dat_months_stockholm$Municipality_Name[i], ]
  if(nrow(tempMonth)!=0){
    dat_months_stockholm$Diff_Number_of_Fires_Month[i] <- dat_months_stockholm$Number_of_Fires_Month[i]- tempMonth$Number_of_Fires_Month[1]
    dat_months_stockholm$Diff_Temperature[i] <- dat_months_stockholm$Temperature[i]- tempMonth$Temperature[1]
    dat_months_stockholm$Diff_Median_Income_20plus[i] <- dat_months_stockholm$Median_Income_20plus[i] - tempMonth$Median_Income_20plus[1]
    dat_months_stockholm$Diff_Total_Number_of_Residents[i] <- dat_months_stockholm$Total_Number_of_Residents[i] - tempMonth$Total_Number_of_Residents[1]
    dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64[i] <- dat_months_stockholm$Percentage_of_Unemployed_18_64[i] - tempMonth$Percentage_of_Unemployed_18_64[1]
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

# Adjust income for inflation
library(readr)
KPI <- read_delim("Kommundata/KPI.csv", ";", escape_double = FALSE, trim_ws = TRUE)


for (i in 1:nrow(dat_months_stockholm)) {
  Year = dat_months_stockholm$Year[i]
  Mon = as.numeric(dat_months_stockholm$Month[i])
  KPI_corr <- KPI[KPI$År==Year, c(Mon+1)]
  value <- as.character(KPI_corr[1,1])
  value <- gsub("\\s", "", value) 
  value <- as.double(value)
  dat_months_stockholm$Adj_Median_income_20plus[i] <- dat_months_stockholm$Median_Income_20plus[i] / value
}


dat_months_stockholm$Total_Number_of_1000_Residents <- (dat_months_stockholm$Total_Number_of_Residents) /1000

dat_months_stockholm <- dat_months_stockholm[,-c(2,3,8,9,10,11,12,13,14)]

#Remove 2018
dat_months_stockholm_all <- dat_months_stockholm
dat_months_stockholm_2018 <- subset(dat_months_stockholm, year(dat_months_stockholm$Date) == 2018)
dat_months_stockholm <- subset(dat_months_stockholm,year(dat_months_stockholm$Date) != 2018)

write_csv(dat_months_stockholm,"DATA/dat_months_stockholm.csv")
write_csv(dat_months_stockholm_all,"DATA/dat_months_stockholm_all.csv")
write_csv(dat_months_stockholm_2018,"DATA/dat_months_stockholm_2018.csv")

dat_months_stockholm <- subset(dat_months_stockholm,dat_months_stockholm$Year != 2018)

# Remove all stored variables 
rm(Year,Mon,KPI_corr,value,i,IncChange,PastMonth,PastYear,UnEmpChange,ResChange,KPI,tempMonth,MunicipalityJan, dat_days_stockholm,dat_stockholm_kolada, Kolada_2005,FireCount,Dates,dat_weather,Dat_Municipalities,dat_stockholm_kolada,dat_msb_for_Kolada,dat_msb,dat_holidays,dat_fires_stockholm, dat_stockholm, Municipalities,j,n)
