# Dummy variables estimators, equivalent fixed effects
model_months_nor <- lm(formula = dat_months_stockholm$Diff_Number_of_Fires_Month ~ 
                     dat_months_stockholm$Diff_Temperature
                   + dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64
                   + dat_months_stockholm$Diff_Total_Number_of_Residents
                   + dat_months_stockholm$Diff_Median_Income_20plus -1)

# Summary with robust errors
summary(model_months_nor)
lmtest::coeftest(model_months_nor, vcov. = sandwich::vcovHC(model_months_nor, type = 'HC1'))

model_months_poi <- glm(formula = dat_months_stockholm$Diff_Number_of_Fires_Month ~
                      dat_months_stockholm$Diff_Temperature
                    + dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64
                    + dat_months_stockholm$Diff_Total_Number_of_Residents
                    + dat_months_stockholm$Diff_Median_Income_20plus -1, family = "poisson")

# Summary with robust errors
lmtest::coeftest(model_months_poi, vcov. = sandwich::vcovHC(model_months_poi, type = 'HC1'))

model_months_nb <- glm.nb(formula = dat_months_stockholm$Diff_Number_of_Fires_Month ~
                    + dat_months_stockholm$Diff_Temperature
                    + dat_months_stockholm$Diff_Percentage_of_Unemployed_18_64
                    + dat_months_stockholm$Diff_Total_Number_of_Residents
                    + dat_months_stockholm$Diff_Median_Income_20plus -1)

# Summary with robust errors
lmtest::coeftest(model_months_nb, vcov. = sandwich::vcovHC(model_months_nb, type = 'HC1'))

vif(model_months) #remember to take away muncipality names