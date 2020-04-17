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
dat_inhabitants <- read_excel("alla invanare.xlsx", 
                                               skip = 1)

dat_inhabitants <- data.frame(dat_inhabitants[1:2], stack(dat_inhabitants[3:ncol(dat_inhabitants)]))
colnames(dat_inhabitants) <- c("Variable", "Municipality", "Values", "Year")
dat_inhabitants <- dat_inhabitants%>% arrange(dat_inhabitants$Municipality)
dat_inhabitants <- reshape(data=dat_inhabitants,idvar= c("Municipality", "Year"),
                           v.names = "Values",
                           timevar = "Variable",
                           direction="wide")

dat_inhabitants$antal_20_30 <- rowSums (dat_inhabitants[,6:16])
dat_inhabitants$andel_20_30 <- (dat_inhabitants$antal_20_30/dat_inhabitants$`Values.Invånare  totalt, antal`) *100
dat_inhabitants <- dat_inhabitants [, -c(6:16,18)]
colnames(dat_inhabitants) <- c("Municipality", "Year", "Yearly_Change_in_Number_of_Residents_since_Previous_Year", "Total_Number_of_Residents", "Percentage_of_Residents_under_Twenty", "Percentage_of_residents_over_Sixty_Five", "Percentage_of_Residents_between_Twenty_and_Thirty" )

Number_of_Fires <- data.frame( dat_msb %>%
  group_by(year(Date), Municipality_Code, Municipality_Name) %>%
  count())

colnames(Number_of_Fires) <- c("Year", "Municipality_code", "Municipality", "Number_of_Fires")
Number_of_Fires$Year <- as.factor(Number_of_Fires$Year)

dat_inhabitants <- left_join(dat_inhabitants, Number_of_Fires, by=c("Municipality", "Year"))
dat_inhabitants <- dat_inhabitants[-c(45:66),]
dat_inhabitants$Number_of_Fires [is.na(dat_inhabitants$Number_of_Fires)]<- 0
dat_inhabitants$Number_of_Fires_per_Thousand <- (dat_inhabitants$Number_of_Fires/dat_inhabitants$Total_Number_of_Residents) * 1000

