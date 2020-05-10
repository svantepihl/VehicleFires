#for Leo's computer

#getOption("encoding")
#options(encoding = "ISO-8859-1")

#Sys.setlocale(category = "LC_ALL", locale = "Swedish")

# Load data 
source("all fires Import and Clean.r")

# Add municipality info (colada)
source("Kolada_Kommuner_Stockholm_import.R")

# Add months info (fires by month)
source("Cleaning_Month.R")

