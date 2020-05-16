require(pglm)
require(sandwich)
require(lmtest)
require(plm)
require(foreign)
require(car)

#ploting
# coplot(Number_of_Fires_Month ~ Year|Municipality_Name, type="l", data=dat_months_stockholm) # Lines
# coplot(Number_of_Fires_Month ~ Year|Municipality_Name, type="b", data=dat_months_stockholm) # Lines# Points and lines
# 
# scatterplot(Number_of_Fires_Month ~ Date|Municipality_Name, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=dat_months_stockholm)


# Define form
form <- (dat_months_stockholm$Number_of_Fires_Month ~
           dat_months_stockholm$Temperature
         + dat_months_stockholm$Percentage_of_Unemployed_18_64
         + dat_months_stockholm$Total_Number_of_1000_Residents
         +dat_months_stockholm$Adj_Median_income_20plus -1)



# PLM (continous)

##Random effect
model_months_plm_random <- plm(form, data = dat_months_stockholm, model = "random", index = c("Municipality_Name","Date"))
coeftest(model_months_plm_random, vcov=vcovHC(model_months_plm_random, type="sss", cluster="group"))
summary(model_months_plm_random)

# Calculte F stat (Degrees of freedom 4,4051)
(0.19045/4) / ((1-0.19045)/40451)

ranef(model_months_plm_random) # Get random effects

# BP Larange Multiplier test, H_0
plmtest(model_months_plm_fixed, type="bp")

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
summary(model_months_plm_fixed_lsdv)


## First difference
model_months_plm_first_difference <- plm(form, data = dat_months_stockholm, model = "fd", index = c("Municipality_Name", "Date"))
coeftest(model_months_plm_first_difference, vcov=vcovHC(model_months_plm_first_difference, type="sss", cluster="group"))

pdwtest(model_months_plm_fixed)
pbnftest(model_months_plm_fixed)


rm(model_months_plm_fixed,model_months_plm_first_difference,model_months_plm_random)


model_months_neg_bin_fixed <- pglm(form, data = dat_months_stockholm, model = "within", family= "negbin", index = c("Municipality_Name","Date"))
