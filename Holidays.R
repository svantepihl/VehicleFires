library(readxl)
dat_holidays <- read_excel("Holidays/SkollovStockholm.xlsx", 
                                col_types = c("numeric", "text", "text", 
                                              "date", "date", "date", "date", "date", 
                                              "date", "date", "date", "date", "date"), 
                                na = "NA")


#Remove NA values
dat_holidays <- na.omit(dat_holidays)

# Loop through datset and check if a valid holiday value
for(i in 1:nrow(dat_stockholm)) {
  for(j in 1:nrow(dat_holidays)){
    if(dat_stockholm$Year[i] == dat_holidays$Year[j]) {
      if(dat_stockholm$Municiplaity_Code[i] == dat_holidays$Municipality_Code[j]) {
        # Check christmas holidays (start of year)
        if(dat_stockholm$Date[i] <= dat_holidays$Christmas_Holidays_End[j]) {
          dat_stockholm$Christmas_Holidays[i] <- TRUE
          break()
        }
        # Check sport holidays
        if(dat_stockholm$Date[i] >= dat_holidays$Sport_Holidays_Start[j] & dat_stockholm$Date[i] <= dat_holidays$Sport_Holidays_End[j]) {
          dat_stockholm$Sport_Holidays[i] <- TRUE
          break()
        }
        # Check Easter holidays
        if(dat_stockholm$Date[i] >= dat_holidays$Easter_Holidays_Start[j] & dat_stockholm$Date[i] <= dat_holidays$Easter_Holidays_End[j]) {
          dat_stockholm$Easter_Holidays[i] <- TRUE
          break()
        }
        # Check Summer holidays
        if(dat_stockholm$Date[i] >= dat_holidays$Summer_Holidays_Start[j] & dat_stockholm$Date[i] <= dat_holidays$Summer_Holidays_End[j]) {
          dat_stockholm$Summer_Holidays[i] <- TRUE
          break()
        }
        # Check autumn holidays
        if(dat_stockholm$Date[i] >= dat_holidays$Autumn_Holidays_Start[j] & dat_stockholm$Date[i] <= dat_holidays$Autumn_Holidays_End[j]) {
          dat_stockholm$Autumn_Holidays[i] <- TRUE
          break()
        }
        # Check christmas holidays (end of year)
        if(dat_stockholm$Date[i] >= dat_holidays$Christmas_Holidays_Start[j]) {
          dat_stockholm$Christmas_Holidays[i] <- TRUE
          break()
        }
      }
    }
  }
}

# Create Holidays General Column
dat_stockholm$Holidays [dat_stockholm$Christmas_Holidays == TRUE | dat_stockholm$Sport_Holidays == TRUE | dat_stockholm$Easter_Holidays == TRUE | dat_stockholm$Summer_Holidays == TRUE | dat_stockholm$Autumn_Holidays == TRUE ]   <- TRUE

