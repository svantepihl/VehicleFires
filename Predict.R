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

# Create tempory dataframe with all observations 2005-2018 inclusive.
temp <- dat_months_stockholm_all
#----------------------------------------------------------------------------------------------------------------------------------------------------------#
########### PLM (continous) ##########
##Random effect PLM
model_months_plm_random <- plm(form, data = dat_months_stockholm, model = "random", index = c("Municipality_Name","Date"))
summary(model_months_plm_random)

Effects <- cbind(data.frame(municip = names(ranef(model_months_plm_random))),as.data.frame(as.numeric(ranef(model_months_plm_random))))
colnames(Effects) <- c("Municipality_Name", "RandomEffect")
temp <- merge(temp,Effects)
temp$PredictionRandomEffect <- (temp$RandomEffect + model_months_plm_random$coefficients[1] * temp$Temperature 
          + model_months_plm_random$coefficients[2] * temp$Percentage_of_Unemployed_18_64 
          + model_months_plm_random$coefficients[3] * temp$Total_Number_of_1000_Residents
          + model_months_plm_random$coefficients[4] * temp$Adj_Median_income_20plus)



##Pooled OLS
model_months_plm_pooled_alt <- lm(form_intercept)
summary(model_months_plm_pooled_alt)

temp$PredictionPooled <- (model_months_plm_pooled_alt$coefficients[1] 
                         + model_months_plm_pooled_alt$coefficients[2] * temp$Temperature
                         + model_months_plm_pooled_alt$coefficients[3] * temp$Percentage_of_Unemployed_18_64
                         + model_months_plm_pooled_alt$coefficients[4] * temp$Total_Number_of_1000_Residents
                         + model_months_plm_pooled_alt$coefficients[5] * temp$Adj_Median_income_20plus)


#Fixed Effect
model_months_plm_fixed <- plm(form, data = dat_months_stockholm, model = "within", index = c("Municipality_Name","Date"))
summary(model_months_plm_fixed)

Effects <- cbind(data.frame(municip = names(fixef(model_months_plm_fixed))),as.data.frame(as.numeric(fixef(model_months_plm_fixed))))
colnames(Effects) <- c("Municipality_Name", "FixedEffect")
temp <- merge(temp,Effects)

temp$PredictionFixedEffect <- (temp$FixedEffect + model_months_plm_fixed$coefficients[1] * temp$Temperature 
                                + model_months_plm_fixed$coefficients[2] * temp$Percentage_of_Unemployed_18_64 
                                + model_months_plm_fixed$coefficients[3] * temp$Total_Number_of_1000_Residents
                                + model_months_plm_fixed$coefficients[4] * temp$Adj_Median_income_20plus)


rm(model_months_plm_pooled,model_months_plm_pooled_alt,model_months_plm_random,Effects,model_months_plm_fixed,model_months_plm_fixed_lsdv)


#----------------------------------------------------------------------------------------------------------------------------------------------------------#
########### Poisson ##########

# PooledPoisson
model_months_poisson_pooled_alt <- glm(form_intercept,family = "poisson")
summary(model_months_poisson_pooled_alt)

temp$PredictionPoissonPooled <- exp(model_months_poisson_pooled_alt$coefficients[1] 
                         + model_months_poisson_pooled_alt$coefficients[2] * temp$Temperature
                         + model_months_poisson_pooled_alt$coefficients[3] * temp$Percentage_of_Unemployed_18_64
                         + model_months_poisson_pooled_alt$coefficients[4] * temp$Total_Number_of_1000_Residents
                         + model_months_poisson_pooled_alt$coefficients[5] * temp$Adj_Median_income_20plus)


# Random effect / NOT POSSIBLE


# Fixed effect 
model_months_poisson_fixed_alt <- glm(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                        dat_months_stockholm$Municipality_Name 
                                      + dat_months_stockholm$Temperature 
                                      + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                      +dat_months_stockholm$Total_Number_of_1000_Residents
                                      + dat_months_stockholm$Adj_Median_income_20plus -1, family = "poisson")

Effects <- as.data.frame(cbind(names(model_months_poisson_fixed_alt$coefficients[1:26]),as.numeric(model_months_poisson_fixed_alt$coefficients[1:26])))
colnames(Effects) <- c("Municipality_Name", "PoissonFixedEffect")
Effects$Municipality_Name <- (substring(Effects$Municipality_Name, 39))
temp <- merge(temp,Effects)
rm(Effects)

temp$PredictionPoissonFixed <- exp((as.numeric(temp$PoissonFixedEffect) + model_months_poisson_fixed_alt$coefficients[27]*temp$Temperature + (model_months_poisson_fixed_alt$coefficients[28] * temp$Percentage_of_Unemployed_18_64) + (model_months_poisson_fixed_alt$coefficients[29] * temp$Total_Number_of_1000_Residents) + (model_months_poisson_fixed_alt$coefficients[30] * temp$Adj_Median_income_20plus)) )



rm(model_months_poisson_random,model_months_poisson_pooled_alt,model_months_poisson_fixed,model_months_poisson_fixed_alt)

#----------------------------------------------------------------------------------------------------------------------------------------------------------#
########### NegBin ##########

# Pooled
model_months_negbin_pooled <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                     + dat_months_stockholm$Temperature
                                     + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                     + dat_months_stockholm$Total_Number_of_1000_Residents
                                     + dat_months_stockholm$Adj_Median_income_20plus)

summary(model_months_negbin_pooled)

temp$PredictionNegBinPooled <- exp(model_months_negbin_pooled$coefficients[1] 
                                   + model_months_negbin_pooled$coefficients[2] * temp$Temperature
                                   + model_months_negbin_pooled$coefficients[3] * temp$Percentage_of_Unemployed_18_64
                                   + model_months_negbin_pooled$coefficients[4] * temp$Total_Number_of_1000_Residents
                                   + model_months_negbin_pooled$coefficients[5] * temp$Adj_Median_income_20plus)

# Random / NOT POSSIBLE

# Fixed effect
model_months_negbin_fixed <- glm.nb(formula = dat_months_stockholm$Number_of_Fires_Month ~
                                          + dat_months_stockholm$Municipality_Name
                                          + dat_months_stockholm$Temperature
                                          + dat_months_stockholm$Percentage_of_Unemployed_18_64
                                          + dat_months_stockholm$Total_Number_of_1000_Residents
                                          + dat_months_stockholm$Adj_Median_income_20plus -1)

summary(model_months_negbin_fixed)

Effects <- as.data.frame(cbind(names(model_months_negbin_fixed$coefficients[1:26]),as.numeric(model_months_negbin_fixed$coefficients[1:26])))
colnames(Effects) <- c("Municipality_Name", "NegBinFixedEffect")
Effects$Municipality_Name <- (substring(Effects$Municipality_Name, 39))
temp <- merge(temp,Effects)
rm(Effects)


temp$PredictionNegBinFixed <- exp((as.numeric(temp$NegBinFixedEffect) + model_months_negbin_fixed$coefficients[27]*temp$Temperature + (model_months_negbin_fixed$coefficients[28] * temp$Percentage_of_Unemployed_18_64) + (model_months_negbin_fixed$coefficients[29] * temp$Total_Number_of_1000_Residents) + (model_months_negbin_fixed$coefficients[30] * temp$Adj_Median_income_20plus)) )

rm(model_months_negbin_fixed,model_months_negbin_pooled)

predictions <- temp[ ,-c(2,3,4,7,8,9,10,13,16,19)]
write_csv(predictions, "predictions.csv")

rm(temp)
