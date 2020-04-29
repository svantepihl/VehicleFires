dat_months_stockholm_arson <- dat_days_stockholm_arson[, c(2, 4:13, 15:25)] %>% group_by(Year,Month, Municipality_Name, Municipality_Code) %>% 
  summarize(Temperature=mean(Temperature), Precipitation = mean(Precipitation), Holidays = sum(Holidays), Christmas_Holidays = sum(Christmas_Holidays),
            Sport_Holidays = sum(Sport_Holidays), Easter_Holidays = sum(Easter_Holidays),
            Summer_Holidays = sum(Summer_Holidays), Autumn_Holidays = sum(Autumn_Holidays),
            First_Quarter = mean(First_Quarter), Second_Quarter=mean(Second_Quarter), 
            Third_Quarter = mean(Third_Quarter),
            Fourth_Quarter = mean(Fourth_Quarter),
            Number_of_Fires = sum(Number_of_Fires),
            Number_Weekend_Days = sum(Weekend))%>% 
  as.data.frame


# Merge with Kolada
dat_months_stockholm_arson <-merge (dat_months_stockholm_arson, dat_stockholm_kolada,  by= c("Municipality_Name", "Year"), all.y=TRUE)
dat_months_stockholm_arson <- dat_months_stockholm_arson [, -c(43,44)]


colnames(dat_months_stockholm_arson) [17] <- "Number_of_Fires"
dat_months_stockholm_arson$Month <- as.factor(dat_months_stockholm_arson$Month)

# Check for equal mean and variance

# Check for equal mean and variance
var(dat_months_stockholm_arson$Number_of_Fires_Month, na.rm = TRUE)
mean(dat_months_stockholm_arson$Number_of_Fires_Month, na.rm =TRUE)

# Check that the number of fires displayed in both columns is correct
sum(dat_months_stockholm_arson$Number_of_Fires_Month, na.rm = TRUE)
sum(dat_months_stockholm_arson$Number_of_Fires_Year, na.rm = TRUE) /12 


# Add lagged variables 

dat_months_stockholm_arson <- dat_months_stockholm_arson %>% 
  group_by(Municipality_Name) %>%
  mutate(First_Difference = Number_of_Fires_Month - lag(Number_of_Fires_Month))%>%
  ungroup

dat_months_stockholm_arson <- dat_months_stockholm_arson %>% 
  group_by(Municipality_Name) %>%
  mutate(Past_Month_Fires = lag(Number_of_Fires_Month))%>%
  ungroup

