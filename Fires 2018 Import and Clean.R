require(tidyverse)
require(readxl)
require(lubridate)

dat_2018 <- read_excel("dat_msb_2018.xlsx")

colnames(dat_2018) <- c("Code", "Date", "Time", "Origin_of_Fire", "Consequence_of_Fire", 
                        "Region_Code", "Region_Name","Municiplaity_Code", "Municipality_Name",
                        "Municiplaity_Type_Code", "Municipality_Type", "Deso_Code", 
                        "Type_of_Environment", "Environment", "Number_of_Cars", 
                        "number_of_buses", "number_of_trucks",
                        "Number_of_Campers", "Number_of_Other_Vehicles", 
                        "Number_of_Agricultural_Machines", "Number_of_Forestry_Machines",
                        "Number_of_Other_Work_Machines", "Number_of_Trains", "Number_of_Boats",
                        "Number_of_Helicopters_and_Planes", "Number_of_Other_Appliances_or_Machines",
                        "Type_of_Object_Catching_Fire_First", "Object_Catching_Fire_First",
                        "Type_of_Source_of_Fire_Source", "Fire_Source", "Reason") 


#Data cleaning- create separate year, quarter, month, day, hour and minute variables. 
dat_2018$Year <- year(dat_2018$Date)
dat_2018$Quarter <- quarter(dat_2018$Date)
dat_2018$Month <- month(dat_2018$Date)
dat_2018$Day <- day(dat_2018$Date)
dat_2018$Hour <- hour(dat_2018$Time)
dat_2018$Minute <- minute(dat_2018$Time)

# We rename the Reasons behind the carfires to english
dat_2018$Reason [dat_2018$Reason != "Fel i utrustning"& dat_2018$Reason != "Avsiktlig händelse"& dat_2018$Reason != "Gick inte att bedöma"] <-"Other Reason"
dat_2018$Reason [dat_2018$Reason == "Avsiktlig händelse"] <- "Arson"
dat_2018$Reason [dat_2018$Reason == "Fel i utrustning"] <- "Technical Malfunctioning"
dat_2018$Reason [dat_2018$Reason == "Gick inte att bedöma"] <- "Unknown"

# Subset fires from Skåne, Västra Götland and Stockholm
dat_skåne <- filter(dat_2018, dat_2018$Region_Code == 12)
dat_göteborg <- filter(dat_2018, dat_2018$Region_Code == 14)
dat_stockholm <- filter(dat_2018, dat_2018$Region_Code == 01)

