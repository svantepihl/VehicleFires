require(tidyverse)
require(readxl)
require(lubridate)


dat_msb <- read_excel("msb.xlsx", 
                      col_types = c("date", "date", "text", 
                                    "text", "text", "numeric", "text", 
                                    "text"))


#colnames
colnames(dat_msb) <- c("Date", "Time", "Type_of_Vehicle", "Municiplaity_Code", "Municipality_Name", "Type_of_Municipality_Code", "Type_of_Muncipality", "Reason")

#Do we want to have only cars or all type of vehicles? If we want to eliminate the other veichles add code here 

#Data cleaning - create separate year, quarter, month, day, hour and minute variables. 
dat_msb$Year <- year(dat_msb$Date)
dat_msb$Quarter <- quarter(dat_msb$Date)
dat_msb$Month <- month(dat_msb$Date)
dat_msb$Day <- day(dat_msb$Date)
dat_msb$Weekday <- weekdays(dat_msb$Date)
dat_msb$Weekday <- factor(dat_msb$Weekday, levels = c("måndag", "tisdag", "onsdag", "torsdag", "fredag", "lördag", "söndag"))
dat_msb$Hour <- hour(dat_msb$Date)
dat_msb$Minute <- minute(dat_msb$Date)
dat_msb$Region_Code <- as.integer(as.integer(dat_msb$Municiplaity_Code)/100)



# We rename the Reasons behind the carfires to english
dat_msb$Reason [dat_msb$Reason != "Fel i utrustning"& dat_msb$Reason != "Avsiktlig brand"& dat_msb$Reason != "Okänd"] <-"Other Reason"
dat_msb$Reason [dat_msb$Reason == "Avsiktlig brand"] <- "Arson"
dat_msb$Reason [dat_msb$Reason == "Fel i utrustning"] <- "Technical Malfunctioning"
dat_msb$Reason [dat_msb$Reason == "Okänd"] <- "Unknown"



# Subset fires from Skåne, Västra Götland and Stockholm

dat_skåne <- filter(dat_msb, dat_msb$Region_Code == 12)
dat_göteborg <- filter(dat_msb, dat_msb$Region_Code == 14)
dat_stockholm <- filter(dat_msb, dat_msb$Region_Code == 01)