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
require(forecast)
require(stats)



# Dummy variables estimators, equivalent fixed effects

months_arson <- lm(formula = dat_months_stockholm_arson$Number_of_Fires_Month ~
                     + dat_months_stockholm_arson$Municipality_Name + dat_months_stockholm_arson$Holidays + dat_months_stockholm_arson$Temperature + dat_months_stockholm_arson$`Percentage_of_Unemployed_and_Not_Looking_for_Work_or_Studying_16_64,`
                   + dat_months_stockholm_arson$`Percentage_of_Adults_Claiming_Low-Income_Benefits_for_a_Long_Period_of_Time`
                   + dat_months_stockholm_arson$Percentage_of_16_to_84_Lacking_Trust_in_Others
                   + dat_months_stockholm_arson$Percentage_of_Students_without_the_Grades_to_be_admitted_into_Work_Related_High_School_Programs
                   + dat_months_stockholm_arson$Total_Number_of_Residents + dat_months_stockholm_arson$Percentage_of_Residents_Born_Outside_Sweden)

months_arson_2 <- glm(formula = dat_months_stockholm_arson$Number_of_Fires_Month ~
                        + dat_months_stockholm_arson$Municipality_Name + dat_months_stockholm_arson$Holidays + dat_months_stockholm_arson$Temperature, family = "poisson")

months_arson_3 <- glm.nb(formula = dat_months_stockholm_arson$Number_of_Fires_Month ~
                           + dat_months_stockholm_arson$Municipality_Name + dat_months_stockholm_arson$Holidays + dat_months_stockholm_arson$Temperature)

summary(months_arson)
summary(months_arson_2)
summary(months_arson_3)


# Classic panel models" 

form <- (dat_months_stockholm_arson$Number_of_Fires_Month ~
           + dat_months_stockholm_arson$Holidays + dat_months_stockholm_arson$Temperature + dat_months_stockholm_arson$`Percentage_of_Unemployed_and_Not_Looking_for_Work_or_Studying_16_64,`
         + dat_months_stockholm_arson$`Percentage_of_Adults_Claiming_Low-Income_Benefits_for_a_Long_Period_of_Time`
         + dat_months_stockholm_arson$Percentage_of_16_to_84_Lacking_Trust_in_Others
         + dat_months_stockholm_arson$Percentage_of_Students_without_the_Grades_to_be_admitted_into_Work_Related_High_School_Programs + dat_months_stockholm_arson$Precipitation)

form_2 <- (dat_months_stockholm_arson$Number_of_Fires_Month ~
             + dat_months_stockholm_arson$Temperature)

months_arson_plm_random <- plm(form, data = dat_months_stockholm_arson, model = "random", index = c("Municipality_Name","Date"))
months_arson_plm_fixed <- plm(form, data = dat_months_stockholm_arson, model = "within", index = c("Municipality_Name","Date"))
months_arson_plm_pooled <- plm(form, data = dat_months_stockholm_arson, model = "pooling", index = c("Municipality_Name", "Date"))
months_arson_plm_first_difference <- plm(form, data = dat_months_stockholm_arson, model = "fd", index = c("Municipality_Name", "Date"))

summary(months_arson_plm_random)
summary(months_arson_plm_fixed)
summary(months_arson_plm_pooled)
summary(months_arson_plm_first_difference)


months_arson_pglm_random <- pglm(form, data = dat_months_stockholm_arson, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
months_arson_pglm_fixed <- pglm(form, data = dat_months_stockholm_arson, model = "within", family= "poisson", index = c("Municipality_Name","Date"))
months_arson_pglm_pooled <- pglm(form, data = dat_months_stockholm_arson, model = "pooling", family= "poisson", index = c("Municipality_Name", "Date"))
months_arson_pglm_first_difference <- pglm(form, data = dat_months_stockholm_arson, model = "fd",family = "poisson", index = c("Municipality_Name", "Date"))

summary(months_arson_pglm_random)
summary(months_arson_pglm_fixed)
summary(months_arson_pglm_pooled)
summary(months_arson_pglm_first_difference)


phtest(months_arson_plm_random, months_arson_plm_fixed)


#days

form <- dat_days_stockholm_arson$Number_of_Fires ~
         dat_days_stockholm_arson$Temperature + dat_days_stockholm_arson$Weekday
form_2 <- (dat_days_stockholm_arson$Number_of_Fires ~
             + dat_days_stockholm_arson$Temperature)

days_arson_plm_random <- plm(form, data = dat_days_stockholm_arson, model = "random", index = c("Municipality_Name","Date"))
days_arson_plm_fixed <- plm(form, data = dat_days_stockholm_arson, model = "within", index = c("Municipality_Name","Date"))
days_arson_plm_pooled <- plm(form, data = dat_days_stockholm_arson, model = "pooling", index = c("Municipality_Name", "Date"))
days_arson_plm_first_difference <- plm(form, data = dat_days_stockholm_arson, model = "fd", index = c("Municipality_Name", "Date"))

summary(days_arson_plm_random)
summary(days_arson_plm_fixed)
summary(days_arson_plm_pooled)
summary(days_arson_plm_first_difference)

days_arson_pglm_random <- pglm(form, data = dat_days_stockholm_arson, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
days_arson_pglm_fixed <- plm(form, data = dat_days_stockholm_arson, model = "within", family= "poisson",index = c("Municipality_Name","Date"))
days_arson_pglm_pooled <- plm(form, data = dat_days_stockholm_arson, model = "pooling",family = "poisson",  index = c("Municipality_Name", "Date"))
days_arson_pglm_first_difference <- plm(form, data = dat_days_stockholm_arson, model = "fd",family= "poisson", index = c("Municipality_Name", "Date"))

summary(days_arson_pglm_random)
summary(days_arson_pglm_fixed)
summary(days_arson_pglm_pooled)
summary(days_arson_pglm_first_difference)

days_arson_pglm_random <- pglm(form, data = dat_days_stockholm_arson, model = "random", family = "negbin", index = c("Municipality_Name","Date"))
days_arson_pglm_fixed <- plm(form, data = dat_days_stockholm_arson, model = "within", family= "negbin",index = c("Municipality_Name","Date"))
days_arson_pglm_pooled <- plm(form, data = dat_days_stockholm_arson, model = "pooling",family = "negbin",  index = c("Municipality_Name", "Date"))
days_arson_pglm_first_difference <- plm(form, data = dat_days_stockholm_arson, model = "fd",family= "negbin", index = c("Municipality_Name", "Date"))


phtest(days_arson_plm_random, days_arson_plm_fixed)