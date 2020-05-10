require(tidyverse)
require(survival)
require(ranger)
require(ggfortify)
require(lubridate)
require(anytime)
require(forecast)
require(urca)
require(stats)
require(MASS)

# exploratory models
dat_days_stockholm$Municipality_Name <- as.factor(dat_days_stockholm$Municipality_Name)
str(dat_days_stockholm)

a <- lm (dat_days_stockholm$Number_of_Fires ~ dat_days_stockholm$Municipality_Name + dat_days_stockholm$Temperature + dat_days_stockholm$Precipitation + dat_days_stockholm$Holidays + dat_days_stockholm$Weekday, dat_days_stockholm )
summary(a)
a$fitted.values
dat_days_stockholm$Number_of_Fires
plot(a$residuals)
cor(a$residuals, diff(a$residuals))
min(dat_days_stockholm$Number_of_Fires)
plot(dat_days_stockholm$Number_of_Fires)
cor(as.numeric(dat_days_stockholm$Municipality_Name), a$residuals)
dat_days_stockholm$ln_count <- log(dat_days_stockholm$Number_of_Fires)
dat_days_stockholm$ln_count[dat_days_stockholm$ln_count < 0] <- 0
var(dat_days_stockholm$Number_of_Fires)
mean(dat_days_stockholm$Number_of_Fires)

min(dat_days_stockholm$ln_count)
re <- diff(a$residuals)
cor(a$residuals[2:300], re[1:299])
plot(a$residuals [1:200])

colnames(dat_days_stockholm) [12] <- "Number_of_Fires"
y_yesterday_botkyrka <-  mega_list [[1]] [1:2556, 12]
y_day_before_yesterday <- mega_list [[1]] [1:2555, 12]

Botkyrka <- filter(dat_days_stockholm, dat_days_stockholm$Municipality_Name=="Botkyrka")
Botkyrka$Yesterday_fires <- c(0, y_yesterday_botkyrka)
Botkyrka$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday)
Botkyrka$Month

prot_botkyrka <- lm(formula = Botkyrka$Number_of_Fires ~ Botkyrka$Temperature + Botkyrka$Yesterday_fires
   + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$Weekday + Botkyrka$Holidays)

prot_botkyrka_2 <- glm(formula = Botkyrka$Number_of_Fires ~ Botkyrka$Temperature + Botkyrka$Yesterday_fires
                    + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$Weekday + Botkyrka$Holidays, family = "poisson")

prot_botkyrka_3 <- glm.nb(formula = Botkyrka$Number_of_Fires ~ Botkyrka$Temperature + Botkyrka$Yesterday_fires
                       + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$Weekday + Botkyrka$Holidays)




# Add first difference variables  

dat_months_stockholm <- dat_months_stockholm %>% 
  group_by(Municipality_Name) %>%
  mutate(First_Difference = Number_of_Fires_Month - lag(Number_of_Fires_Month))%>%
  ungroup


dat_months_stockholm <- dat_months_stockholm %>% 
  group_by(Municipality_Name) %>%
  mutate(Past_Month_Fires = lag(Number_of_Fires_Month))%>%
  ungroup

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
