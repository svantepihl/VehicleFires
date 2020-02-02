require(tidyverse)
require(readxl)
require(lubridate)
dat_msb <- read_excel("msb.xlsx", 
                      col_types = c("date", "date", "text", 
                                    "text", "text", "numeric", "text", 
                                    "text"))


#colnames
colnames(dat_msb) [8] <- "Reason"


#Data cleaning- create separate year, quarter, month, day, hour and minute variables. 
dat_msb$year <- year(dat_msb$datum)
dat_msb$quarter <- quarter(dat_msb$datum)
dat_msb$month <- month(dat_msb$datum)
dat_msb$day <- day(dat_msb$datum)
dat_msb$weekday <- weekdays(dat_msb$datum)
dat_msb$hour <- hour(dat_msb$tid)
dat_msb$minute <- minute(dat_msb$tid)


# We rename the Reasons behind the carfires to english
dat_msb$Reason [dat_msb$Reason != "Fel i utrustning"& dat_msb$Reason != "Avsiktlig brand"& dat_msb$Reason != "Okänd"] <-"Other Reason"
dat_msb$Reason [dat_msb$Reason == "Avsiktlig brand"] <- "Arson"
dat_msb$Reason [dat_msb$Reason == "Fel i utrustning"] <- "Technical Malfunctioning"
dat_msb$Reason [dat_msb$Reason == "Okänd"] <- "Unknown"

# subset: anlagd, fel i utrustning, okänd, annat
Arson <- filter(dat_msb, dat_msb$Reason == "Arson")
Technical_malfunctioning <- filter(dat_msb, dat_msb$Reason == "Technical Malfunctioning")
Unknown <- filter(dat_msb, dat_msb$Reason == "Unknown")
Others <- filter(dat_msb, dat_msb$Reason != "Arson", dat_msb$Reason != "Technical Malfunctioning", dat_msb$Reason != "Unknown")


######### Anlagda Bilbränder  ############


