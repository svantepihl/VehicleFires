require(tidyverse)
require(ggplot2)
require(survival)
require(lubridate)
require(MASS)
require(lmtest)
require(xts)
require(plm)
require(zoo)
require(pglm)

#Data cleaning for this file 
dat_months_stockholm$Date <- as.yearmon(paste(dat_months_stockholm$Year, dat_months_stockholm$Month), "%Y %m")

colnames(dat_stockholm_kolada) [1] <- "Municipality_Name"

colnames(dat)

dat_months_stockholm <-merge (dat_months_stockholm, dat_stockholm_kolada,  by= c("Municipality_Name", "Year"), all.y=TRUE)
dat_months_stockholm <- dat_months_stockholm [, -c(43,44)]
dat_months_stockholm <- dat_months_stockholm [, -c(22,23)]

colnames(dat_months_stockholm) [17] <- "Number_of_Fires"
colnames(dat_months_stockholm) [22:31] <- c("Percentage_of_Under_Twenty_Living_in_Poor_Household", 
"Percentage_of_16_to_64_not_Studying_or_Workin_or_Looking_for_work",
"Percentgae_of_16_to_84_lacking_trust_in_others",
"Percentage_of_17_to_24_not_Studying_or_Workin_or_Looking_for_work",
"Percentage_of_16_to_64_with_low_income",
"Amount_of_Benefits_Claimed",
"Percentage_of_16_to_24_in_Long_Term_Unemployment",
"Median_Income_Twenty_Plus",
"Percentage_of_People_Born_Outside_of_Sweden",
"Percentage_of_Adults_claiming_Benefits_for_a_Long_period_of_Time"
)

dat_months_stockholm$Month <- as.factor(dat_months_stockholm$Month)

dat_months_stockholm_arson <-merge (dat_months_stockholm_arson, dat_stockholm_kolada,  by= c("Municipality_Name", "Year"), all.y=TRUE)
dat_months_stockholm_arson <- dat_months_stockholm_arson [, -c(43,44)]


colnames(dat_months_stockholm_arson) [17] <- "Number_of_Fires"

dat_months_stockholm_arson$Month <- as.factor(dat_months_stockholm_arson$Month)

# Check for equal mean and variance
var(dat_days_stockholm$Number_of_Fires)
mean(dat_days_stockholm$Number_of_Fires)

var(dat_months_stockholm$Number_of_Fires)
mean(dat_months_stockholm$Number_of_Fires)

# Add lagged variables 

dat_months_stockholm_arson <- dat_months_stockholm_arson %>% 
  group_by(Municipality_Name) %>%
  mutate(First_Difference = Number_of_Fires - lag(Number_of_Fires))%>%
  ungroup

dat_months_stockholm_arson <- dat_months_stockholm_arson %>% 
  group_by(Municipality_Name) %>%
  mutate(Past_Month_Fires = lag(Number_of_Fires))%>%
  ungroup

dat_months_stockholm <- dat_months_stockholm %>% 
  group_by(Municipality_Name) %>%
  mutate(First_Difference = Number_of_Fires - lag(Number_of_Fires))%>%
  ungroup


dat_months_stockholm <- dat_months_stockholm %>% 
  group_by(Municipality_Name) %>%
  mutate(Past_Month_Fires = lag(Number_of_Fires))%>%
  ungroup

# Models - Panels by Hand, useful only to understand functioning of plm/pglm

model_months <- lm(formula = dat_months_stockholm$Number_of_Fires ~
                    + dat_months_stockholm$Municipality_Name + dat_months_stockholm$Holidays
                    + dat_months_stockholm$Temperature + dat_months_stockholm$Past_Month_Fires)

model_months_2 <- glm(formula = dat_months_stockholm$Number_of_Fires ~
                     + dat_months_stockholm$Municipality_Name + dat_months_stockholm$Holidays
                   + dat_months_stockholm$Temperature + dat_months_stockholm$Past_Month_Fires, family = "poisson")

model_months_3 <- glm.nb(formula = dat_months_stockholm$Number_of_Fires ~
                        + dat_months_stockholm$Municipality_Name + dat_months_stockholm$Holidays
                      + dat_months_stockholm$Temperature + dat_months_stockholm$Past_Month_Fires)


model_months_arson <- lm(formula = dat_months_stockholm_arson$Number_of_Fires ~
                     + dat_months_stockholm_arson$Municipality_Name + dat_months_stockholm_arson$Temperature +dat_months_stockholm$Year
                     + dat_months_stockholm_arson$Past_Month_Fires)

cor(dat_months_stockholm$Number_of_Fires, dat_months_stockholm$Past_Month_Fires, use = "complete.obs")


summary(model_months)
summary(model_months_2)
summary(model_months_3)


summary(model_months_arson)


# Panel models, "the good(decent) models" 

form <- (dat_months_stockholm$Number_of_Fires ~  dat_months_stockholm$Temperature 
+ dat_months_stockholm$Holidays + dat_months_stockholm$Percentage_of_Under_Twenty_Living_in_Poor_Household 
+ dat_months_stockholm$Percentage_of_16_to_64_not_Studying_or_Workin_or_Looking_for_work
+ dat_months_stockholm$Percentgae_of_16_to_84_lacking_trust_in_others
+ dat_months_stockholm$Total_Number_of_Residents.x
)


model_months_plm_random <- plm(form, data = dat_months_stockholm, effect = "individual", model = "random", index = c("Municipality_Name","Date"))
model_months_plm_fixed <- plm(form, data = dat_months_stockholm, model = "within", index = c("Municipality_Name","Date"))

model_months_plm_pooled <- plm(form, data = dat_months_stockholm, model = "pooling", index = c("Municipality_Name", "Date"))

model_months_pglm_random <- pglm(form, data = dat_months_stockholm, effect = "individual", model = "random", family = "poisson", index = c("Municipality_Name","Date"))


summary(model_months_plm_random)
summary(model_months_plm_fixed)
summary(model_months_plm_pooled)


phtest(model_months_plm_random, model_months_plm_fixed)
summary(model_months_pglm_random)





