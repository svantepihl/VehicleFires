require(tidyverse)
require(MASS)
require(lmtest)
require(xts)
require(plm)
require(zoo)
require(pglm)
require(forecast)
require(stats)
require(lmtest)
require(car)
require(sandwich)
require(lubridate)




# Dummy variables estimators, equivalent fixed effects

model_months <- lm(formula = dat_months_stockholm$Number_of_Fires_Month ~ 
                   +  dat_months_stockholm$Municipality_Name 
                   + dat_months_stockholm$Temperature
                   + dat_months_stockholm$Percentage_of_Unemployed_18_64
                   + dat_months_stockholm$Total_Number_of_1000_Residents
                   + dat_months_stockholm$Median_Income_20plus)


# Summary with robust errors
lmtest::coeftest(model_months, vcov. = sandwich::vcovHC(model_months, type = 'HC1'))
summary(model_months)

rm(model_months)


model_months_poi <- glm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                        dat_months_stockholm$Municipality_Name 
                      + dat_months_stockholm$Temperature 
                      + dat_months_stockholm$Percentage_of_Unemployed_18_64
                      +dat_months_stockholm$Total_Number_of_Residents
                      + dat_months_stockholm$Median_Income_20plus -1, family = "poisson")

# Summary with robust errors
lmtest::coeftest(model_months_poi, vcov. = sandwich::vcovHC(model_months_poi, type = 'HC1'))

model_months_nb <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                           dat_months_stockholm$Municipality_Name 
                         + dat_months_stockholm$Temperature 
                         + dat_months_stockholm$Percentage_of_Unemployed_18_64
                         +dat_months_stockholm$Total_Number_of_1000_Residents
                         + dat_months_stockholm$Adj_Median_income_20plus -1)

# Summary with robust errors
lmtest::coeftest(model_months_nb, vcov. = sandwich::vcovHC(model_months_nb, type = 'HC1'))

vif(model_months) #remember to take away muncipality names

# Classic panel models" 
form <- (dat_months_stockholm$Number_of_Fires_Month ~
           dat_months_stockholm$Temperature
         + dat_months_stockholm$Percentage_of_Unemployed_18_64
         + dat_months_stockholm$Total_Number_of_Residents
         +dat_months_stockholm$Median_Income_20plus -1)



model_months_plm_random <- plm(form, data = dat_months_stockholm, model = "random", index = c("Municipality_Name","Date"))
model_months_plm_fixed <- plm(form, data = dat_months_stockholm, model = "within", index = c("Municipality_Name","Date"))
model_months_plm_pooled <- plm(form, data = dat_months_stockholm, model = "pooling", index = c("Municipality_Name", "Date"))
model_months_plm_first_difference <- plm(form, data = dat_months_stockholm, model = "fd", index = c("Municipality_Name", "Date"))

summary(model_months_plm_random)
summary(model_months_plm_fixed)
summary(model_months_plm_pooled)
summary(model_months_plm_first_difference)


lmtest::coeftest(model_months_plm_fixed, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))
lmtest::coeftest(model_months_plm_first_difference, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))


model_months_poisson_random <- pglm(form, data = dat_months_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
model_months_poisson_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "poisson", index = c("Municipality_Name","Date"))
model_months_poisson_pooled <- pglm(form, data = dat_months_stockholm, model = "pooling", family= "poisson", index = c("Municipality_Name", "Date"))
model_months_poisson_first_difference <- pglm(form, data = dat_months_stockholm, model = "fd",family = "poisson", index = c("Municipality_Name", "Date"))

summary(model_months_poisson_random)
summary(model_months_poisson_fixed)
summary(model_months_poisson_pooled)
summary(model_months_poisson_first_difference)


lmtest::coeftest(model_months_poisson_fixed, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))
lmtest::coeftest(model_months_poisson_first_difference, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))


model_months_neg_bin_random <- pglm(form, data = dat_months_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"))
model_months_neg_bin_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "negbin", index = c("Municipality_Name","Date"))
warnmodel_months_neg_bin_pooled <- pglm(form, data = dat_months_stockholm, model = "pooling", family= "poisson", index = c("Municipality_Name", "Date"))
model_months_neg_bin_first_difference <- pglm(form, data = dat_months_stockholm, model = "fd",family = "poisson", index = c("Municipality_Name", "Date"))

summary(model_months_neg_bin_random)
summary(model_months_neg_bin_fixed)
summary(model_months_neg_bin_pooled)
summary(model_months_neg_bin_first_difference)

model_months_neg_bin_fixed$maximum

lmtest::coeftest(model_months_neg_bin_fixed, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))
lmtest::coeftest(model_months_neg_bin_first_difference, vcov. = sandwich::vcovHC(model_months_plm_fixed, type = 'HC1'))

phtest(model_months_plm_random, model_months_plm_fixed)

phtest(model_days_plm_random, model_days_plm_fixed)





                                                