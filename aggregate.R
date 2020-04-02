require(tidyverse)
require(lubridate)

##### By municipality
# Number of cases by date and municipality 
dat_municipality_day <- dat_stockholm %>%
  group_by(Date, Municipality_Code, Municipality_Name) %>%
  count()

# Number of cases by week,year and municipality 
dat_municipality_week <- dat_stockholm %>%
  group_by(year(Date), isoweek(Date), Municipality_Code, Municipality_Name) %>%
  count()

# Number of cases by month,year and municipality 
dat_municipality_month <- dat_stockholm %>%
  group_by(year(Date), month(Date), Municipality_Code, Municipality_Name) %>%
  count()

# Number of cases by month,year and municipality 
dat_municipality_quarter <- dat_stockholm %>%
  group_by(year(Date), quarter(Date), Municipality_Code, Municipality_Name) %>%
  count()

# Number of cases by year and municipality 
dat_municipality_year <- dat_stockholm %>%
  group_by(year(Date), Municipality_Code, Municipality_Name) %>%
  count()

##### Whole region
# Number of cases by date 
dat_all_day <- dat_stockholm %>%
  group_by(Date) %>%
  count()

# Number of cases by week and year
dat_all_week <- dat_stockholm %>%
  group_by(isoweek(Date),year(Date)) %>%
  count()

# Number of cases by month year
dat_all_month <- dat_stockholm %>%
  group_by(month(Date), year(Date)) %>%
  count()

# Number of cases by quarter and year
dat_all_quarter <- dat_stockholm %>%
  group_by(quarter(Date), year(Date)) %>%
  count()

# Number of cases by year 
dat_all_Year <- dat_stockholm %>%
  group_by(year(Date)) %>%
  count()

