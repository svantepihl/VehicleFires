require(pglm)
require(sandwich)
require(lmtest)
require(plm)
require(MASS)



# Define form
form <- (dat_months_stockholm$Number_of_Fires_Month ~
           dat_months_stockholm$Temperature
         + dat_months_stockholm$Percentage_of_Unemployed_18_64
         + dat_months_stockholm$Total_Number_of_1000_Residents
         + dat_months_stockholm$Adj_Median_income_20plus -1)

form_intercept <- (dat_months_stockholm$Number_of_Fires_Month ~
           dat_months_stockholm$Temperature
         + dat_months_stockholm$Percentage_of_Unemployed_18_64
         + dat_months_stockholm$Total_Number_of_1000_Residents
         + dat_months_stockholm$Adj_Median_income_20plus)


#----------------------------------------------------------------------------------------------------------------------------------------------------------#
########### PLM (continous) ##########
##Pooled OLS
model_months_plm_pooled <- plm(form, data = dat_months_stockholm, model="pooling", index = c("Municipality_Name","Date"))
model_months_plm_pooled_alt <- lm(form_intercept)
summary(model_months_plm_pooled)
summary(model_months_plm_pooled_alt)


model_months_plm_pooled_alt$fitted.values
coeftest(model_months_plm_pooled, vcov=vcovHAC(model_months_plm_pooled_alt))



##Random effect
model_months_plm_random <- plm(form, data = dat_months_stockholm, model = "random", index = c("Municipality_Name","Date"))
coeftest(model_months_plm_random, vcov=vcovHC(model_months_plm_random, type="sss", cluster="group"))
summary(model_months_plm_random)

# Calculte F stat (Degrees of freedom 4,4051)
(0.19045/4) / ((1-0.19045)/4051)

ranef(model_months_plm_random) # Get random effects

# BP Larange Multiplier test, H_0
plmtest(model_months_plm_random, type="bp")

## Fixed effect
model_months_plm_fixed <- plm(form, data = dat_months_stockholm, model = "within", index = c("Municipality_Name","Date"))
coeftest(model_months_plm_fixed, vcov=vcovHC(model_months_plm_fixed, type="sss", cluster="group"))
summary(model_months_plm_fixed)

# Fixed effect (LSDV)
model_months_plm_fixed_lsdv <- lm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                   +  dat_months_stockholm$Municipality_Name
                   + dat_months_stockholm$Temperature
                   + dat_months_stockholm$Percentage_of_Unemployed_18_64
                   + dat_months_stockholm$Total_Number_of_1000_Residents
                   + dat_months_stockholm$Adj_Median_income_20plus)


coeftest(model_months_plm_fixed_lsdv, vcov=vcovHAC(model_months_plm_fixed_lsdv, cluster="group"))
coeftest(model_months_plm_fixed_lsdv, vcov=vcovHAC(model_months_plm_fixed_lsdv))
summary(model_months_plm_fixed_lsdv)


## First difference
model_months_plm_first_difference <- plm(form, data = dat_months_stockholm, model = "fd", index = c("Municipality_Name", "Date"))
coeftest(model_months_plm_first_difference, vcov=vcovHC(model_months_plm_first_difference, type="sss", cluster="group"))
summary(model_months_plm_first_difference)

pdwtest(model_months_plm_fixed)
pbnftest(model_months_plm_fixed)


#remove models from previous section
rm(model_months_plm_fixed,model_months_plm_first_difference,model_months_plm_random,model_months_plm_fixed_lsdv, model_months_plm_pooled,model_months_plm_pooled_alt)


#----------------------------------------------------------------------------------------------------------------------------------------------------------#
##### Poission ######


# Pooled 
model_months_poisson_pooled <- pglm(form, data = dat_months_stockholm, model = "pooling", family = "poisson", index = c("Municipality_Name","Date"), print.level = 2)
model_months_poisson_pooled_alt <- glm(form,family = "poisson")
summary(model_months_poisson_pooled_alt)
coeftest(model_months_poisson_pooled_alt, vcov=vcovHAC(model_months_poisson_pooled_alt))
summary(model_months_poisson_pooled)


# Random
model_months_poisson_random <- pglm(form, data = dat_months_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"), print.level = 2, R = 100)
coeftest(model_months_poisson_random)
summary(model_months_poisson_random)


# Fixed
model_months_poisson_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "poisson", index = c("Municipality_Name","Date"))
model_months_poisson_fixed_alt <- glm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                        dat_months_stockholm$Municipality_Name 
                                      + dat_months_stockholm$Temperature 
                                      + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                      +dat_months_stockholm$Total_Number_of_1000_Residents
                                      + dat_months_stockholm$Adj_Median_income_20plus -1, family = "poisson")



coeftest(model_months_poisson_fixed_alt, vcov=vcovHAC(model_months_poisson_fixed_alt, cluster="group"))

summary(model_months_poisson_fixed)
summary(model_months_poisson_fixed_alt)

rm(model_months_poisson_fixed,model_months_poisson_fixed_alt,model_months_poisson_pooled,model_months_poisson_pooled_alt,model_months_poisson_random)


#----------------------------------------------------------------------------------------------------------------------------------------------------------#
### NegBin ####

# Pooled
model_months_negbin_pooled <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                                                + dat_months_stockholm$Municipality_Name
                                                                + dat_months_stockholm$Temperature
                                                                + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                                                + dat_months_stockholm$Total_Number_of_1000_Residents
                                                                + dat_months_stockholm$Adj_Median_income_20plus -1)
summary(model_months_negbin_pooled,dispersion = 3.1507)
coeftest(model_months_negbin_pooled, vcov=vcovHAC(model_months_negbin_pooled))


# Random Effect
model_months_negbin_random <- pglm(form, data = dat_months_stockholm, model = "random", family = "poisson", index = c("Municipality_Name","Date"), print.level = 2)
summary(model_months_negbin_random)



# Fixed effect
model_months_negbin_fixed_dummy <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                    + dat_months_stockholm$Municipality_Name
                                    + dat_months_stockholm$Temperature
                                    + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                    + dat_months_stockholm$Total_Number_of_1000_Residents
                                    + dat_months_stockholm$Adj_Median_income_20plus -1)

model_months_negbin_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "negbin", index = c("Municipality_Name","Date"))
summary(model_months_negbin_fixed, dispersion = 12.7824)


rm(model_months_negbin_fixed,model_months_negbin_pooled,model_months_negbin_random,model_months_negbin_fixed_dummy)


