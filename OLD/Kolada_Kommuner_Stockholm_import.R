require(tidyverse)
require(reshape2)
require(reshape)
require(MASS)
require(readxl)

Kolada_2005 <- read_excel("Kommundata/Kolada_2005.xlsx", 
                          skip = 1)

Kolada_2005 <- data.frame(Kolada_2005[1:2], stack(Kolada_2005[3:ncol(Kolada_2005)]))
colnames(Kolada_2005) <- c("Variable", "Municipality", "Values", "Year")
Kolada_2005 <- Kolada_2005%>% arrange(Kolada_2005$Municipality)
Kolada_2005 <- reshape(data=Kolada_2005,idvar= c("Municipality", "Year"),
                       v.names = "Values",
                       timevar = "Variable",
                       direction="wide")

colnames(Kolada_2005) <- c("Municipality_Name", "Year", "Percentage_of_Unemployed_18_64", "Total_Number_of_Residents", "Percentage_of_Unemployed_or_in_Unemployemt_Programs_16_64","Median_Income_20plus")

Kolada_2005$Year <- Kolada_2005$Year%>% as.character()%>%as.integer()

Kolada_2005 <- Kolada_2005 [!(Kolada_2005$Municipality_Name == "Stockholms l???ns kommuner (ov???gt medel)"),]  

dat_stockholm_kolada <- filter(Kolada_2005, Kolada_2005$Year < 2019)
rm(Kolada_2005, Kolada_2005_kolada, Dates, dat_msb_for_Kolada, dat_fires_stockholm, Dat_Municipalities)

#Number_of_Fires <- data.frame(dat_msb_for_Kolada %>%
#group_by(year(Date), Municipality_Code, Municipality_Name) %>%
#count())

#colnames(Number_of_Fires) <- c("Year", "Municipality_code", "Municipality_Name", "Number_of_Fires")
#Number_of_Fires$Year <- as.factor(Number_of_Fires$Year)

#Kolada_2005 <- left_join(Kolada_2005, Number_of_Fires, by=c("Municipality_Name", "Year"))

#Kolada_2005$Number_of_Fires [is.na(Kolada_2005$Number_of_Fires)]<- 0
#Kolada_2005$Number_of_Fires_per_Thousand <- (Kolada_2005$Number_of_Fires/Kolada_2005$Total_Number_of_Residents) * 1000

#Kolada_2005$Municipality_code<- as.integer(Kolada_2005$Municipality_code)
#Kolada_2005_kolada <- filter(Kolada_2005, Municipality_code < 200)

#Kommuner_stockholm$Year <- Kommuner_stockholm$Year%>% as.character()%>%as.integer()


#dat_stockholm_kolada <- left_join(Kommuner_stockholm, dat_days_stockholm_kolada)

#rm(Kommuner_stockholm, dat_days_stockholm_kolada, dat_msb_for_Kolada, Kolada_2005)


#dat_stockholm_kolada <- left_join(dat_kolada_3, dat_stockholm_kolada, by = c("Year", "Municipality"))
#rm(dat_kolada_3)

#rm(Number_of_Fires, Dat_Municipalities, Kommuner)

#colnames(dat_stockholm_kolada) [1] <- "Municipality_Name"
#colnames(dat_stockholm_kolada) [30] <-"Number_of_Fires_Year"