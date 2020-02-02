require(tidyverse)
require(readxl)
require(lubridate)

dat_2018 <- read_excel("dat_msb_2018.xlsx")

colnames(dat_2018) <- c("code", "date", "time", "originary_fire", "consequence_fire", 
                        "län_code", "län_name","kommun_code", "kommun_name",
                        "kommun-type_code", "kommun_type", "deso_code", 
                        "type_of_environment", "environment", "number_of_cars", 
                        "number_of_buses", "number_of_trucks",
                        "number_of_campers", "number_of_other_vehicles", 
                        "number_of_agricultural_machines", "number_of_forestry_machines",
                        "number_of_other_work_machines", "number_of_trains", "number_of_boats",
                        "number_of_helicopters_and_planes", "number_of_other_appliances_or_machines",
                        "type_of_object_catching_fire_first", "object_catching_fire_first",
                        "type_of_source_of_fire_source", "fire_source", "cause") 


#Data cleaning- create separate year, quarter, month, day, hour and minute variables. 
dat_2018$year <- year(dat_2018$date)
dat_2018$quarter <- quarter(dat_2018$date)
dat_2018$month <- month(dat_2018$date)
dat_2018$day <- day(dat_2018$date)
dat_2018$hour <- hour(dat_2018$time)
dat_2018$minute <- minute(dat_2018$time)

# We rename the causes behind the carfires to english
dat_2018$cause [dat_2018$cause != "Fel i utrustning"& dat_2018$cause != "Avsiktlig händelse"& dat_2018$cause != "Gick inte att bedöma"] <-"Other cause"
dat_2018$cause [dat_2018$cause == "Avsiktlig händelse"] <- "Arson"
dat_2018$cause [dat_2018$cause == "Fel i utrustning"] <- "Technical Malfunctioning"
dat_2018$cause [dat_2018$cause == "Gick inte att bedöma"] <- "Unknown"

# subset: anlagd, fel i utrustning, Gick inte att bedöma, annat
Arson_2018 <- filter(dat_2018, dat_2018$cause == "Arson")
Technical_malfunctioning_2018 <- filter(dat_2018, dat_2018$cause == "Technical Malfunctioning")
Unknown_2018 <- filter(dat_2018, dat_2018$cause == "Unknown")
Others_2018 <- filter(dat_2018, dat_2018$cause != "Arson", dat_2018$cause != "Technical Malfunctioning", dat_2018$cause != "Unknown")

