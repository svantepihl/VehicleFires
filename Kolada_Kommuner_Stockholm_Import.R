require(tidyverse)
require(reshape2)
require(reshape)
require(MASS)
require(readxl)

Kommuner <- read_excel("Kommundata/Kommuner.xlsx")

Kommuner_stockholm <- data.frame(Kommuner[1:2], stack(Kommuner[3:ncol(Kommuner)]))
Kommuner_stockholm <- Kommuner_stockholm %>% arrange(Kommun)
colnames(Kommuner_stockholm) <- c("Variable", "Municipality", "Values", "Year")
Kommuner_stockholm <- reshape(data=Kommuner_stockholm,idvar= c("Municipality", "Year"),
                          v.names = "Values",
                          timevar = "Variable",
                          direction="wide")
Kommuner_stockholm <- Kommuner_stockholm[,-19]
colnames(Kommuner_stockholm) <- c("Municipality", "Year", "Number of Expert Exployees", "Percentage of Expert Employees", "Regional GDP per capita", "Percentage of Non-Workforce",
                                  "Percentage of Students without the Grades to be admitted into Professional High School Programs", "Percentage of Active Workforce", "Percentage of 0-19 People Living in Poor Households",
                                  "Percentage of Unemployed and Not Looking for Work or Studying 16-64,", "Percentage of 16 to 84 Lacking Trust in Others", "Percentage of Unemployed and Not Looking for Work or Studying 17-24",
                                  "Percentage of 16-64 with Low-Income", "Amount of Benefits claimed", "Percentage of 16-24 in Long Term Unemployment", "Median Income 20+","Percentage of Residents Born Outside Sweden", 
                                  "Percentage of Adults Claiming Low-Income Benefits for a Long Period of Time")  


#Add data about inhabitants
dat_inhabitants <- read_excel("kommuner_invånare.xlsx")

dat_inhabitants <- data.frame(dat_inhabitants[1:1], stack(dat_inhabitants[2:ncol(dat_inhabitants)]))
colnames(dat_inhabitants) <- c("Municipality_Name","Inhabitants", "Year")
dat_inhabitants <- left_join(dat_inhabitants,Dat_Municipalities[ ,c(1,2)])


