require(tidyverse)
require(readxl)
require(lubridate)
require(svMisc)




str(dat_days_stockholm)


dat_months_stockholm <- dat_days_stockholm[, c(2, 4:13, 15:24)] %>% group_by(Year,Month, Municipality_Name, Municipality_Code) %>% 
  summarize(Temperature=mean(Temperature), Precipitation = mean(Precipitation), Holidays = sum(Holidays), Christmas_Holidays = sum(Christmas_Holidays),
            Sport_Holidays = sum(Sport_Holidays), Easter_Holidays = sum(Easter_Holidays),
            Summer_Holidays = sum(Summer_Holidays), Autumn_Holidays = sum(Autumn_Holidays),
            first_quarter = mean(first_quarter), second_quarter=mean(second_quarter), 
            third_quarter = mean(third_quarter),
            fourth_quarter = mean(fourth_quarter),
            Number_of_Fires = sum(Number_of_Fires),
            Number_Weekend_Days = sum(weekend))%>% 
  as.data.frame


dat_months_stockholm_arson <- dat_days_stockholm_arson[, c(2, 4:13, 15:24)] %>% group_by(Year,Month, Municipality_Name, Municipality_Code) %>% 
  summarize(Temperature=mean(Temperature), Precipitation = mean(Precipitation), Holidays = sum(Holidays), Christmas_Holidays = sum(Christmas_Holidays),
            Sport_Holidays = sum(Sport_Holidays), Easter_Holidays = sum(Easter_Holidays),
            Summer_Holidays = sum(Summer_Holidays), Autumn_Holidays = sum(Autumn_Holidays),
            first_quarter = mean(first_quarter), second_quarter=mean(second_quarter), 
            third_quarter = mean(third_quarter),
            fourth_quarter = mean(fourth_quarter),
            Number_of_Fires = sum(Number_of_Fires),
            Number_Weekend_Days = sum(weekend))%>% 
  as.data.frame

