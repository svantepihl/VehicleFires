require(tidyverse)
require(lubridate)
require(MASS)
require(lmtest)

colnames(dat_days_stockholm_arson) [12] <- "Number_of_Fires"

dat_days_stockholm_arson$Week <- isoweek(dat_days_stockholm_arson$Date)
dat_days_stockholm_arson$Month <- month(dat_days_stockholm_arson$Date)
dat_days_stockholm_arson$Quarter <- quarter(dat_days_stockholm_arson$Date)
dat_days_stockholm_arson$Year <- year(dat_days_stockholm_arson$Date)

split(dat_days_stockholm_arson, as.factor(dat_days_stockholm_arson$Weekday)) %>%lapply( "[", ,12)%>%lapply(mean)
split(dat_days_stockholm_arson, as.factor(dat_days_stockholm_arson$Month)) %>%lapply( "[", ,12)%>%lapply(mean)
split(dat_days_stockholm_arson, as.factor(dat_days_stockholm_arson$Quarter)) %>%lapply( "[", ,12)%>%lapply(mean)

dat_days_stockholm_arson$first_quarter <- 0
dat_days_stockholm_arson$second_quarter <- 0
dat_days_stockholm_arson$third_quarter <-0
dat_days_stockholm_arson$fourth_quarter <- 0
dat_days_stockholm_arson$weekend <- 0

dat_days_stockholm_arson [dat_days_stockholm_arson$Quarter == 1, "first_quarter"] <- 1
dat_days_stockholm_arson [dat_days_stockholm_arson$Quarter == 2, "second_quarter"] <- 1
dat_days_stockholm_arson [dat_days_stockholm_arson$Quarter == 3, "third_quarter"] <- 1
dat_days_stockholm_arson [dat_days_stockholm_arson$Quarter == 4, "fourth_quarter"] <- 1
dat_days_stockholm_arson [dat_days_stockholm_arson$Weekday == "lördag" | dat_days_stockholm_arson$Weekday == "söndag", "weekend"] <- 1

a <- split(dat_days_stockholm_arson, dat_days_stockholm_arson$Municipality_Name)


Botkyrka <- a$Botkyrka # Neighbours:Nynäshamn, Haninge, Huddinge, Stockholm, Ekerö, Salem, Södertälje
Danderyd <- a$Danderyd # Neighbours:Sollentuna, Täby, Solna, Växholm, Lidingö, Stockholm
Ekerö <- a$Ekerö # Neighbours: Upplands-Bro, Järfälla, Stockholm, Botkyrka, Salem, Södertälje
Haninge <-a$Haninge # Neighbours: Tyresö, Huddinge, Botkykrka, Nynäshämn
Huddinge <- a$Huddinge # Neighbours: Botkyrka, Stockholm, Tyresö, Haninge
Järfälla <-a$Järfälla # Neighbours: Upplands Bro, Upplands Väsby, Sollentuna, Sundbyberg, Stockholm, Ekerö
Lidingö <- a$Lidingö # Neighbours: Vaxholm, Österåker, Täby, Danderyd, Stocholm, Nacka, Värmdö
Nacka <- a$Nacka # Neighbours: Värmdö, Vaxholm, Lidingö, Stockholm, Tyresö
Norrtälje <- a$Norrtälje # Neighbours: Sigtuna, Vallentuna, Österåker
Nykvarn <- a$Nykvarn # Neighbours: Södertälje
Nynäshamn <- a$Nynäshamn # Neighbours: Haninge, Botkyrka, Södertälje
Salem <- a$Salem # Neighbours: Södertälje, Ekerö, Botkyrka
Sigtuna <- a$Sigtuna # Neighbours: Norrtälje, Vallentuna, Upplands Väsby, Upplands Bro
Sollentuna <-a$Sollentuna # Neighbours: Upplands Väsby, Täby, Dandedryd, Solna, Sundbyberg, Stockholm, Järfälla
Solna <- a$Solna # Neighbours: Sundbyberg, Sollentuna, Danderyd, Stockholm
Stockholm <- a$Stockholm # Neighbours: Järfälla, Sollentuna, Sundbyberg, Solna, Danderyd, Lidingö, Nacka, Tyresö, Huddinge, Botkykra, Ekerö
Sundbyberg <- a$Sundbyberg # Neighbours: Stockholm, Järfälla, Sollentuna, Solna
Södertälje <- a$Södertälje # Neighbours: Nykvarn, Salem, Botkyrka, Nynänshamn, Ekerö
Tyresö <- a$Tyresö # Neighbours: Nacka, Stockholm, Huddinge, Haninge
Täby <- a$Täby # Neighbours: Uppalnds Väsby, Vallentuna, Österåker, Vaxholm, Lidingö, Danderyd, Sollentuna
Upplands_Bro <-a$`Upplands-Bro`# Neighbours: Sigtuna, Upplands Väsby, Järfälla, Ekerö
Upplands_Väsby <- a$`Upplands Väsby`# Neighbours: Upplands-Bro, Sigtuna, Vallentuna, Täby, Sollentuna, Järfälla
Vallentuna <- a$Vallentuna # Neighbours: Sigtuna, Norrtälje, Öteråker, Täby, Upplands Väsby 
Vaxholm <- a$Vaxholm # Neighbours: Österåker, Värmdö, Nacka, Lidingö, Danderyd, Täby
Värmdö <- a$Värmdö # Neighbours: Vaxholm, Lidingö, Nacka
Österåker <- a$Österåker # Neighbours: Norrtälje, Vallentuna, Täby, Vaxholm

#Botkyrka

y_yesterday_botkyrka <-  mega_list [[1]] [1:2556, 12]
y_day_before_yesterday_botkyrka <- mega_list [[1]] [1:2555, 12]
Botkyrka$Yesterday_fires <- c(0, y_yesterday_botkyrka)
Botkyrka$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday)

#Danderyd
y_yesterday_Danderyd <-  mega_list [[2]] [1:2556, 12]
y_day_before_yesterday_Danderyd <- mega_list [[2]] [1:2555, 12]
Danderyd$Yesterday_fires <- c(0, y_yesterday_Danderyd)
Danderyd$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Danderyd)

#Ekerö
y_yesterday_Ekerö <-  mega_list [[3]] [1:2556, 12]
y_day_before_yesterday_Ekerö <- mega_list [[3]] [1:2555, 12]
Ekerö$Yesterday_fires <- c(0, y_yesterday_Ekerö)
Ekerö$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Ekerö)

#Haninge

y_yesterday_Haninge <-  mega_list [[4]] [1:2556, 12]
y_day_before_yesterday_Haninge <- mega_list [[4]] [1:2555, 12]
Haninge$Yesterday_fires <- c(0, y_yesterday_Haninge)
Haninge$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Haninge)

#Huddinge

y_yesterday_Huddinge <-  mega_list [[5]] [1:2556, 12]
y_day_before_yesterday_Huddinge <- mega_list [[5]] [1:2555, 12]
Huddinge$Yesterday_fires <- c(0, y_yesterday_Huddinge)
Huddinge$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Huddinge)

#Järfälla

y_yesterday_Järfälla <-  mega_list [[6]] [1:2556, 12]
y_day_before_yesterday_Järfälla <- mega_list [[6]] [1:2555, 12]
Järfälla$Yesterday_fires <- c(0, y_yesterday_Järfälla)
Järfälla$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Järfälla)


#Lidingö

y_yesterday_Lidingö <-  mega_list [[7]] [1:2556, 12]
y_day_before_yesterday_Lidingö <- mega_list [[7]] [1:2555, 12]
Lidingö$Yesterday_fires <- c(0, y_yesterday_Lidingö)
Lidingö$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Lidingö)


#Nacka

y_yesterday_Nacka <-  mega_list [[8]] [1:2556, 12]
y_day_before_yesterday_Nacka <- mega_list [[8]] [1:2555, 12]
Nacka$Yesterday_fires <- c(0, y_yesterday_Nacka)
Nacka$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Nacka)

#Norrtälje

y_yesterday_Norrtälje <-  mega_list [[9]] [1:2556, 12]
y_day_before_yesterday_Norrtälje <- mega_list [[9]] [1:2555, 12]
Norrtälje$Yesterday_fires <- c(0, y_yesterday_Norrtälje)
Norrtälje$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Norrtälje)


#Nykvarn

y_yesterday_Nykvarn <-  mega_list [[10]] [1:2556, 12]
y_day_before_yesterday_Nykvarn <- mega_list [[10]] [1:2555, 12]
Nykvarn$Yesterday_fires <- c(0, y_yesterday_Nykvarn)
Nykvarn$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Nykvarn)


#Nynäshamn

y_yesterday_Nynäshamn <-  mega_list [[11]] [1:2556, 12]
y_day_before_yesterday_Nynäshamn <- mega_list [[11]] [1:2555, 12]
Nynäshamn$Yesterday_fires <- c(0, y_yesterday_Nynäshamn)
Nynäshamn$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Nynäshamn)

# Salem

y_yesterday_Salem <-  mega_list [[12]] [1:2556, 12]
y_day_before_yesterday_Salem <- mega_list [[12]] [1:2555, 12]
Salem$Yesterday_fires <- c(0, y_yesterday_Salem)
Salem$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Salem)


# Sigtuna

y_yesterday_Sigtuna <-  mega_list [[13]] [1:2556, 12]
y_day_before_yesterday_Sigtuna <- mega_list [[13]] [1:2555, 12]
Sigtuna$Yesterday_fires <- c(0, y_yesterday_Sigtuna)
Sigtuna$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Sigtuna)


#Sollentuna

y_yesterday_Sollentuna <-  mega_list [[14]] [1:2556, 12]
y_day_before_yesterday_Sollentuna <- mega_list [[14]] [1:2555, 12]
Sollentuna$Yesterday_fires <- c(0, y_yesterday_Sollentuna)
Sollentuna$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Sollentuna)


#Solna


y_yesterday_Solna <-  mega_list [[15]] [1:2556, 12]
y_day_before_yesterday_Solna <- mega_list [[15]] [1:2555, 12]
Solna$Yesterday_fires <- c(0, y_yesterday_Solna)
Solna$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Solna)


# Stockholm

y_yesterday_Stockholm <-  mega_list [[16]] [1:2556, 12]
y_day_before_yesterday_Stockholm <- mega_list [[16]] [1:2555, 12]
Stockholm$Yesterday_fires <- c(0, y_yesterday_Stockholm)
Stockholm$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Stockholm)


# Sundbyberg 

y_yesterday_Sundbyberg <-  mega_list [[17]] [1:2556, 12]
y_day_before_yesterday_Sundbyberg <- mega_list [[17]] [1:2555, 12]
Sundbyberg$Yesterday_fires <- c(0, y_yesterday_Sundbyberg)
Sundbyberg$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Sundbyberg)


# Södertälje 

y_yesterday_Södertälje <-  mega_list [[18]] [1:2556, 12]
y_day_before_yesterday_Södertälje <- mega_list [[18]] [1:2555, 12]
Södertälje$Yesterday_fires <- c(0, y_yesterday_Södertälje)
Södertälje$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Södertälje)


# Tyresö

y_yesterday_Tyresö <-  mega_list [[19]] [1:2556, 12]
y_day_before_yesterday_Tyresö <- mega_list [[19]] [1:2555, 12]
Tyresö$Yesterday_fires <- c(0, y_yesterday_Tyresö)
Tyresö$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Tyresö)


# Täby

y_yesterday_Täby <-  mega_list [[20]] [1:2556, 12]
y_day_before_yesterday_Täby <- mega_list [[20]] [1:2555, 12]
Täby$Yesterday_fires <- c(0, y_yesterday_Täby)
Täby$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Täby)


# Upplands_Bro

y_yesterday_Upplands_Bro <-  mega_list [[21]] [1:2556, 12]
y_day_before_yesterday_Upplands_Bro <- mega_list [[21]] [1:2555, 12]
Upplands_Bro$Yesterday_fires <- c(0, y_yesterday_Upplands_Bro)
Upplands_Bro$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Upplands_Bro)


# Upplands_Väsby

y_yesterday_Upplands_Väsby <-  mega_list [[22]] [1:2556, 12]
y_day_before_yesterday_Upplands_Väsby <- mega_list [[22]] [1:2555, 12]
Upplands_Väsby$Yesterday_fires <- c(0, y_yesterday_Upplands_Väsby)
Upplands_Väsby$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Upplands_Väsby)


# Vallentuna

y_yesterday_Vallentuna <-  mega_list [[23]] [1:2556, 12]
y_day_before_yesterday_Vallentuna <- mega_list [23] [1:2555, 12]
Vallentuna$Yesterday_fires <- c(0, y_yesterday_Vallentuna)
Vallentuna$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Vallentuna)


# Vaxholm


y_yesterday_Vaxholm <-  mega_list [[24]] [1:2556, 12]
y_day_before_yesterday_Vaxholm <- mega_list [[24]] [1:2555, 12]
Vaxholm$Yesterday_fires <- c(0, y_yesterday_Vaxholm)
Vaxholm$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Vaxholm)


#Värmdö

y_yesterday_Värmdö <-  mega_list [[25]] [1:2556, 12]
y_day_before_yesterday_Värmdö <- mega_list [[25]] [1:2555, 12]
Värmdö$Yesterday_fires <- c(0, y_yesterday_Värmdö)
Värmdö$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Värmdö)


# Österåker

y_yesterday_Österåker <-  mega_list [[26]] [1:2556, 12]
y_day_before_yesterday_Österåker <- mega_list [[26]] [1:2555, 12]
Österåker$Yesterday_fires <- c(0, y_yesterday_Österåker)
Österåker$Day_Before_Yesterday_Fires <- c(0,0, y_day_before_yesterday_Österåker)



######## Botkykrka ######


#Neighbours:Nynäshamn, Haninge, Huddinge, Stockholm, Ekerö, Salem, Södertälje

Botkyrka$Nynäshamn_yesterday <- Nynäshamn$Yesterday_fires
Botkyrka$Nynäshamn_day_before_yesterday <- Nynäshamn$Day_Before_Yesterday_Fires

Botkyrka$Haninge_yesterday <- Haninge$Yesterday_fires
Botkyrka$Haninge_day_before_yesterday <- Haninge$Day_Before_Yesterday_Fires

Botkyrka$Huddinge_yesterday <- Huddinge$Yesterday_fires
Botkyrka$Huddinge_day_before_yesterday <- Huddinge$Day_Before_Yesterday_Fires

Botkyrka$Stockholm_yesterday <- Stockholm$Yesterday_fires
Botkyrka$Stockholm_day_before_yesterday <- Stockholm$Day_Before_Yesterday_Fires

Botkyrka$Ekerö_yesterday <- Ekerö$Yesterday_fires
Botkyrka$Ekerö_day_before_yesterday <- Ekerö$Day_Before_Yesterday_Fires

Botkyrka$Salem_yesterday <- Salem$Yesterday_fires
Botkyrka$Salem_day_before_yesterday <- Salem$Day_Before_Yesterday_Fires

Botkyrka$Södertälje_yesterday <- Södertälje$Yesterday_fires
Botkyrka$Södertälje_day_before_yesterday <- Södertälje$Day_Before_Yesterday_Fires


prot_botkyrka <- lm (formula = Botkyrka$Number_of_Fires ~ Botkyrka$Yesterday_fires
                     + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$weekend + as.factor(Botkyrka$Quarter)
                     + Botkyrka$Nynäshamn_yesterday + Botkyrka$Nynäshamn_day_before_yesterday 
                     + Botkyrka$Haninge_yesterday + Botkyrka$Haninge_day_before_yesterday 
                     + Botkyrka$Huddinge_yesterday + Botkyrka$Huddinge_day_before_yesterday 
                     + Botkyrka$Stockholm_yesterday + Botkyrka$Stockholm_day_before_yesterday 
                     + Botkyrka$Ekerö_yesterday + Botkyrka$Ekerö_day_before_yesterday 
                     + Botkyrka$Salem_yesterday + Botkyrka$Salem_day_before_yesterday 
                     + Botkyrka$Södertälje_yesterday + Botkyrka$Södertälje_day_before_yesterday)

prot_botkyrka_2 <- glm(formula = Botkyrka$Number_of_Fires ~ Botkyrka$Yesterday_fires
                       + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$weekend + as.factor(Botkyrka$Quarter)
                       + Botkyrka$Nynäshamn_yesterday + Botkyrka$Nynäshamn_day_before_yesterday 
                       + Botkyrka$Haninge_yesterday + Botkyrka$Haninge_day_before_yesterday 
                       + Botkyrka$Huddinge_yesterday + Botkyrka$Huddinge_day_before_yesterday 
                       + Botkyrka$Stockholm_yesterday + Botkyrka$Stockholm_day_before_yesterday 
                       + Botkyrka$Ekerö_yesterday + Botkyrka$Ekerö_day_before_yesterday 
                       + Botkyrka$Salem_yesterday + Botkyrka$Salem_day_before_yesterday 
                       + Botkyrka$Södertälje_yesterday + Botkyrka$Södertälje_day_before_yesterday, family = "poisson")

prot_botkyrka_3 <- glm.nb(formula = Botkyrka$Number_of_Fires ~ Botkyrka$Yesterday_fires
                          + Botkyrka$Day_Before_Yesterday_Fires + Botkyrka$weekend + as.factor(Botkyrka$Quarter)
                          + Botkyrka$Nynäshamn_yesterday + Botkyrka$Nynäshamn_day_before_yesterday 
                          + Botkyrka$Haninge_yesterday + Botkyrka$Haninge_day_before_yesterday 
                          + Botkyrka$Huddinge_yesterday + Botkyrka$Huddinge_day_before_yesterday 
                          + Botkyrka$Stockholm_yesterday + Botkyrka$Stockholm_day_before_yesterday 
                          + Botkyrka$Ekerö_yesterday + Botkyrka$Ekerö_day_before_yesterday 
                          + Botkyrka$Salem_yesterday + Botkyrka$Salem_day_before_yesterday 
                          + Botkyrka$Södertälje_yesterday + Botkyrka$Södertälje_day_before_yesterday)

sum(Botkyrka$Number_of_Fires)
summary(prot_botkyrka)
summary(prot_botkyrka_2)
summary(prot_botkyrka_3)
plot(prot_botkyrka_3$residuals)
prot_botkyrka_3$residuals

bgtest(prot_botkyrka)
bgtest(prot_botkyrka_2)
bgtest(prot_botkyrka_3)
preds_botkyrka <- prot_botkyrka_3$fitted.values
# then we (1) need to keep in mind that values above 0.5 will be classified
# as 1, the individual voting for the socialdemocrats and (2) compare with what
# we know in our data. I recommend that you simply copy my code here

# we will store our correct classifications in the total vector
# where the values 1 will represent correct classification
high_risk_botkyrka = numeric(2557)   
# && is 'and'; or is || 
for(i in 1:2557){
  if(preds_botkyrka[i] > 0.45 && Botkyrka$Number_of_Fires[i] > 3){  # && means "and". The statement in if() will only be TRUE if both booleans are true
    # this is if model predicts 1, and the true value is 1
    high_risk_botkyrka[i] = 1
  } 
  else if((preds_botkyrka[i] < 0.45) && (Botkyrka$Number_of_Fires[i] > 3)){
    # this is if the model predicts 0, and the true value is 0
    high_risk_botkyrka[i] = -1
  }
  else if((preds_botkyrka[i] < 0.45) && (Botkyrka$Number_of_Fires[i] == 0)){
    # this is if the model predicts 0, and the true value is 0
    high_risk_botkyrka[i] = 0
  }
}
high_risk_botkyrka
summary(high_risk_botkyrka)
summary(as.factor(high_risk_botkyrka))
mean(preds_botkyrka >0.45)
summary(Botkyrka$Number_of_Fires)
summary(as.factor(Botkyrka$Number_of_Fires))
mean(preds)

# then we (1) need to keep in mind that values above 0.5 will be classified
# as 1, the individual voting for the socialdemocrats and (2) compare with what
# we know in our data. I recommend that you simply copy my code here

# we will store our correct classifications in the total vector
# where the values 1 will represent correct classification
low_risk_botkyrka = numeric(2557)   
# && is 'and'; or is || 
for(i in 1:2557){
  if(preds_botkyrka[i] < 0.2 && Botkyrka$Number_of_Fires[i] == 0){# && means "and". The statement in if() will only be TRUE if both booleans are true
    # this is if model predicts 1, and the true value is 1
    low_risk_botkyrka[i] = 1
  } 
  else if((preds_botkyrka[i] < 0.2) && (Botkyrka$Number_of_Fires[i] > 0)){
    # this is if the model predicts 0, and the true value is 0
    low_risk_botkyrka[i] = -1
  }
}
low_risk_botkyrka
summary(low_risk_botkyrka)
summary(as.factor(low_risk_botkyrka))
mean(preds_botkyrka < 0.2)
summary(Botkyrka$Number_of_Fires)
summary(as.factor(Botkyrka$Number_of_Fires))
