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
require(lmtest)
library(leaps)


#remove unsed variables
dat_months_stockholm_lm <- dat_months_stockholm[ ,-c(2,4, 6:16,18,19,22)]

# dat_months_stockholm_lm_impute <- dat_months_stockholm_lm[ , colSums(is.na(dat_months_stockholm_lm)) == 0] - coloumns with na values

full.model <- lm(Number_of_Fires_Month ~.-1, data = dat_months_stockholm_lm)

dat_months_stockholm_lm <- na.omit(dat_months_stockholm_lm)
summary(full.model)


step.model <- stepAIC(full.model, direction = "both", 
                      trace = TRUE)

summary(step.model)

plot(step.model$residuals)


# Dummy variables estimators, equivalent fixed effects
model_months <- lm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                    dat_months_stockholm$Municipality_Name 
                   + dat_months_stockholm$Temperature 
                   + dat_months_stockholm$Month
                   + dat_months_stockholm$Percentage_of_Unemployed_18_64
                   + dat_months_stockholm$Total_Number_of_Residents)



model_months_2 <- glm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                        dat_months_stockholm$Municipality_Name 
                      + dat_months_stockholm$Temperature 
                      + dat_months_stockholm$Month
                      + dat_months_stockholm$Percentage_of_Unemployed_18_64
                      +dat_months_stockholm$Total_Number_of_Residents-1, family = "poisson")

model_months_3 <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                           dat_months_stockholm$Municipality_Name 
                         + dat_months_stockholm$Temperature 
                         + dat_months_stockholm$Month
                         + dat_months_stockholm$Percentage_of_Unemployed_18_64
                         +dat_months_stockholm$Total_Number_of_Residents -1)

summary(model_months)
summary(model_months_2)
summary(model_months_3)

# Classic panel models" 

form <- (dat_months_stockholm$Number_of_Fires_Month ~
           dat_months_stockholm$Temperature
         + dat_months_stockholm$Percentage_of_Unemployed_18_64
         + dat_months_stockholm$Total_Number_of_Residents
         +dat_months_stockholm$Median_Income_20plus)

form_2 <- (dat_months_stockholm$Number_of_Fires_Month ~
             + dat_months_stockholm$Temperature)

model_months_plm_random <- plm(form, data = dat_months_stockholm, model = "random", index = c("Municipality_Name","Date"))
model_months_plm_fixed <- plm(form, data = dat_months_stockholm, model = "within", index = c("Municipality_Name","Date"))
model_months_plm_pooled <- plm(form, data = dat_months_stockholm, model = "pooling", index = c("Municipality_Name", "Date"))
model_months_plm_first_difference <- plm(form, data = dat_months_stockholm, model = "fd", index = c("Municipality_Name", "Date"))

summary(model_months_plm_random)
summary(model_months_plm_fixed)
summary(model_months_plm_pooled)
summary(model_months_plm_first_difference)


model_months_pglm_random <- pglm(form, data = dat_months_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
model_months_pglm_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "poisson", index = c("Municipality_Name","Date"))
model_months_pglm_pooled <- pglm(form, data = dat_months_stockholm, model = "pooling", family= "poisson", index = c("Municipality_Name", "Date"))
model_months_pglm_first_difference <- pglm(form, data = dat_months_stockholm, model = "fd",family = "poisson", index = c("Municipality_Name", "Date"))

summary(model_months_pglm_random)
summary(model_months_pglm_fixed)
summary(model_months_pglm_pooled)
summary(model_months_pglm_first_difference)


phtest(model_months_plm_random, model_months_plm_fixed)


#days

form <- (dat_days_stockholm$Number_of_Fires ~  dat_days_stockholm$Temperature
         + dat_days_stockholm$Holidays)

form_2 <- (dat_days_stockholm$Number_of_Fires ~
             + dat_days_stockholm$Temperature)

model_days_plm_random <- plm(form, data = dat_days_stockholm, model = "random", index = c("Municipality_Name","Date"))
model_days_plm_fixed <- plm(form, data = dat_days_stockholm, model = "within", index = c("Municipality_Name","Date"))
model_days_plm_pooled <- plm(form, data = dat_days_stockholm, model = "pooling", index = c("Municipality_Name", "Date"))
model_days_plm_first_difference <- plm(form, data = dat_days_stockholm, model = "fd", index = c("Municipality_Name", "Date"))

summary(model_days_plm_random)
summary(model_days_plm_fixed)
summary(model_days_plm_pooled)
summary(model_days_plm_first_difference)

model_days_pglm_random <- pglm(form, data = dat_days_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
model_days_pglm_fixed <- plm(form, data = dat_days_stockholm, model = "within", family= "poisson",index = c("Municipality_Name","Date"))
model_days_pglm_pooled <- plm(form, data = dat_days_stockholm, model = "pooling",family = "poisson",  index = c("Municipality_Name", "Date"))
model_days_pglm_first_difference <- plm(form, data = dat_days_stockholm, model = "fd",family= "poisson", index = c("Municipality_Name", "Date"))

summary(model_days_pglm_random)
summary(model_days_pglm_fixed)
summary(model_days_pglm_pooled)
summary(model_days_pglm_first_difference)

model_days_pglm_random <- pglm(form, data = dat_days_stockholm, model = "random", family = "negbin", index = c("Municipality_Name","Date"))
model_days_pglm_fixed <- plm(form, data = dat_days_stockholm, model = "within", family= "negbin",index = c("Municipality_Name","Date"))
model_days_pglm_pooled <- plm(form, data = dat_days_stockholm, model = "pooling",family = "negbin",  index = c("Municipality_Name", "Date"))
model_days_pglm_first_difference <- plm(form, data = dat_days_stockholm, model = "fd",family= "negbin", index = c("Municipality_Name", "Date"))


phtest(model_days_plm_random, model_days_plm_fixed)





                                                