# Load weather for stockholm region 1998-2018
library(readxl)
require(svMisc)
require(tidyverse)
dat_weather <- read_excel("Weather/Weather.xlsx", 
                          sheet = "ALL", col_types = c("text", 
                                                       "text", "date", "numeric", "numeric", 
                                                       "text"))

# Create subset with only data after 2012
dat_stockholm_relevant <- filter(dat_stockholm, dat_stockholm$Year >=2012 )

# Select only relevant weather data (data after 2012)
dat_weather_relevant <- subset(dat_weather, year(dat_weather$Date) >= 2012)


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
#}

write_csv(dat_stockholm_relevant, path = "exports/dat_after2012.csv")




