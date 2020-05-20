require(readr)
require(zoo)
require(lubridate)
#for Leo's computer
#getOption("encoding")
#options(encoding = "ISO-8859-1")
#Sys.setlocale(category = "LC_ALL", locale = "Swedish")

##### IMPORT NOTE IT TAKES 5-15 MINUTES TO RUN #####
if(file.exists("DATA/dat_months_stockholm_all.csv")){
  dat_months_stockholm_all <- read_csv("DATA/dat_months_stockholm_all.csv")
  dat_months_stockholm_all$Date <- as.yearmon(dat_months_stockholm_all$Date)
  
  dat_months_stockholm_2018 <- subset(dat_months_stockholm_all, year(dat_months_stockholm_all$Date) == 2018)
  dat_months_stockholm <- subset(dat_months_stockholm_all, year(dat_months_stockholm_all$Date) != 2018)
} else {
  print("LOADING DATA DURIATION APPROX 15 MIN")
  source("ImportCleanWrangle.R")
}

# Get predictions for model
source("Predict.R")

