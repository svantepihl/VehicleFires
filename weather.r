# Load weather for stockholm region 1998-2018
library(readxl)
require(tidyverse)
dat_weather <- read_excel("Weather/Weather.xlsx", 
                          sheet = "ALL", col_types = c("skip", 
                                                       "text", "date", "text", "text", 
                                                       "skip"))



# Add weather data to observations (takes a while to run)
#for(i in 1:nrow(dat_stockholm_relevant)) {
#  progress(i/(nrow(dat_stockholm_relevant)/100))
#  for(j in 1:nrow(dat_weather_relevant)) {
#    # check if correct municipality
#    if(dat_stockholm_relevant$Municipality_Code[i] == dat_weather_relevant$Municipality_Code[j]) {
#      if(dat_stockholm_relevant$Date[i] == dat_weather_relevant$Date[j]) {
#        dat_stockholm_relevant$Temperature[i] <- dat_weather_relevant$Temperature[j]
#        dat_stockholm_relevant$Precipitation[i] <- dat_weather_relevant$Precipitation[j]
#        break()
#      }
#    }
#  }
#  gc()
#}


# Add weather data 
dat_stockholm <- left_join(dat_stockholm, dat_weather, by=c("Date" = "Date", "Municipality_Code" = "Municipality_Code"))





