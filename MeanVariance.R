require(tidyverse)

Botkyrka <- subset(dat_months_stockholm, dat_months_stockholm$Municipality_Name=="Stockholm")
mean(Botkyrka$Number_of_Fires_Month)
var(Botkyrka$Number_of_Fires_Month)


MeanVar <- dat_months_stockholm[ ,c(1,3)] %>% group_by(Municipality_Name) %>% summarise(Mean=mean(Number_of_Fires_Month),Var=var(Number_of_Fires_Month), Total=sum(Number_of_Fires_Month))
MeanVar$Ratio = MeanVar$Var/MeanVar$Mean




