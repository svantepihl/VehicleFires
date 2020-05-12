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
colnames(Kommuner_stockholm) <- c("Municipality", "Year", "Number_of_Expert_Employees", "Percentage_of_Expert_Employees", "Regional_GDP_per_Capita", "Percentage of Non-Workforce",
                                  "Percentage_of_Students_without_the_Grades_to_be_admitted_into_Work_Related_High_School_Programs", "Percentage_of_Active_Workforce", "Percentage of 0-19 People Living in Poor Households",
                                  "Percentage_of_Unemployed_16_64,", "Percentage_of_16_to_84_Lacking_Trust_in_Others", "Percentage_of_Unemployed_and_Not_Looking_for_Work_or_Studying_17_24",
                                  "Percentage_of_16_64_with_Low_Income", "Amount_of_Benefits_Claimed", "Percentage_of_16_24_in_Long_Term_Unemployment", "Median_Income_20+","Percentage_of_Residents_Born_Outside_Sweden", 
                                  "Percentage_of_Adults_Claiming_Low-Income_Benefits_for_a_Long_Period_of_Time")  


#Add data about inhabitants
dat_inhabitants <- read_excel("Kommundata/alla invanare.xlsx", 
                                               skip = 1)

dat_inhabitants <- data.frame(dat_inhabitants[1:2], stack(dat_inhabitants[3:ncol(dat_inhabitants)]))
colnames(dat_inhabitants) <- c("Variable", "Municipality", "Values", "Year")
dat_inhabitants <- dat_inhabitants%>% arrange(dat_inhabitants$Municipality)
dat_inhabitants <- reshape(data=dat_inhabitants,idvar= c("Municipality", "Year"),
                           v.names = "Values",
                           timevar = "Variable",
                           direction="wide")

dat_inhabitants$antal_20_30 <- rowSums (dat_inhabitants[,6:16])
dat_inhabitants$andel_20_30 <- (dat_inhabitants$`Values.Invånare  totalt, antal`/dat_inhabitants$`Values.Förändring i antal invånare sen föregående år, andel (%)` *100)
dat_inhabitants <- dat_inhabitants [, -c(6:16,18)]
colnames(dat_inhabitants) <- c("Municipality", "Year", "Yearly_Change_in_Number_of_Residents_since_Previous_Year", "Total_Number_of_Residents", "Percentage_of_Residents_under_Twenty", "Percentage_of_residents_over_Sixty_Five", "Percentage_of_Residents_between_Twenty_and_Thirty" )

Number_of_Fires <- data.frame(dat_msb_for_Kolada %>%
  group_by(year(Date), Municipality_Code, Municipality_Name) %>%
  count())

colnames(Number_of_Fires) <- c("Year", "Municipality_code", "Municipality", "Number_of_Fires")
Number_of_Fires$Year <- as.factor(Number_of_Fires$Year)

dat_inhabitants <- left_join(dat_inhabitants, Number_of_Fires, by=c("Municipality", "Year"))
dat_inhabitants <- dat_inhabitants[-c(45:66),]
dat_inhabitants$Number_of_Fires [is.na(dat_inhabitants$Number_of_Fires)]<- 0
dat_inhabitants$Number_of_Fires_per_Thousand <- (dat_inhabitants$Number_of_Fires/dat_inhabitants$Total_Number_of_Residents) * 1000

dat_inhabitants$Municipality_code<- as.integer(dat_inhabitants$Municipality_code)
dat_inhabitants_kolada <- filter(dat_inhabitants, Municipality_code < 200)
dat_inhabitants_kolada$Year <- dat_inhabitants_kolada$Year%>% as.character()%>%as.integer()
dat_days_stockholm_kolada <- filter(dat_inhabitants_kolada, dat_inhabitants_kolada$Year>2011 & dat_inhabitants_kolada$Year < 2019)

Kommuner_stockholm$Year <- Kommuner_stockholm$Year%>% as.character()%>%as.integer()
Kommuner_stockholm <- filter(Kommuner_stockholm, Kommuner_stockholm$Year > 2011 & Kommuner_stockholm$Year < 2019)
rm(dat_inhabitants, dat_inhabitants_kolada)

dat_stockholm_kolada <- left_join(Kommuner_stockholm, dat_days_stockholm_kolada)

rm(Kommuner_stockholm, dat_days_stockholm_kolada, dat_msb_for_Kolada, dat_inhabitants)

#Kolada_3

Kolada_3 <- read_excel("Kommundata/Kolada_3.xlsx", skip = 1)



dat_kolada_3 <- data.frame(Kolada_3[1:2], stack(Kolada_3[3:ncol(Kolada_3)]))
colnames(dat_kolada_3) <- c("Variable", "Municipality", "Values", "Year")
dat_kolada_3 <- dat_kolada_3%>% arrange(dat_kolada_3$Municipality)
dat_kolada_3 <- reshape(data=dat_kolada_3,idvar= c("Municipality", "Year"),
                           v.names = "Values",
                           timevar = "Variable",
                           direction="wide")

colnames(dat_kolada_3) <- c("Municipality", "Year", "Meters_of_Car_Roads_per_Person", "Mil_per_Person_per_Year_by_car", "Perncetage_of_People_Satisfied_with_the_Possibilities_to_Use_Public_transportation", "Percentage_of_Residents_Satisfied_with_Traffic_Safety", "Percentage_of_Residents_Satisified_with_the_Condition_of_the_Streets") 
rm(Kolada_3)

dat_kolada_3$Year <- dat_kolada_3$Year%>% as.character()%>%as.integer()
dat_kolada_3 <- filter(dat_kolada_3, dat_kolada_3$Year > 2011 & dat_kolada_3$Year < 2019)

dat_stockholm_kolada <- left_join(dat_kolada_3, dat_stockholm_kolada, by = c("Year", "Municipality"))
rm(dat_kolada_3)
rm(Dates, Number_of_Fires, Dat_Municipalities, Kommuner)

colnames(dat_stockholm_kolada) [1] <- "Municipality_Name"
colnames(dat_stockholm_kolada) [30] <-"Number_of_Fires_Year"
