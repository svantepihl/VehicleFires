require(tidyverse)

# Create table for mean, variance and mean-variance for all municipalitys
MeanVar <- dat_months_stockholm[ ,c(1,3)] %>% group_by(Municipality_Name) %>% summarise(Mean=mean(Number_of_Fires_Month),Var=var(Number_of_Fires_Month), Total=sum(Number_of_Fires_Month))
MeanVar$Ratio = MeanVar$Var/MeanVar$Mean


# Calculate correlation matrix for regressors
temp <- dat_months_stockholm[ ,c(2,5,6,7)]
cor(temp)

rm(temp, MeanVar)