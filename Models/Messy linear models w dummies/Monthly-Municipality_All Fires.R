require(tidyverse)
require(lubridate)
require(MASS)
require(lmtest)
require(xts)


mega_list <- split(dat_months_stockholm_arson, dat_months_stockholm_arson$Municipality_Name)


Botkyrka <- mega_list$Botkyrka # Neighbours:Nynäshamn, Haninge, Huddinge, Stockholm, Ekerö, Salem, Södertälje
Danderyd <- mega_list$Danderyd # Neighbours:Sollentuna, Täby, Solna, Växholm, Lidingö, Stockholm
Ekerö <- mega_list$Ekerö # Neighbours: Upplands-Bro, Järfälla, Stockholm, Botkyrka, Salem, Södertälje
Haninge <-mega_list$Haninge # Neighbours: Tyresö, Huddinge, Botkykrka, Nynäshämn
Huddinge <- mega_list$Huddinge # Neighbours: Botkyrka, Stockholm, Tyresö, Haninge
Järfälla <-mega_list$Järfälla # Neighbours: Upplands Bro, Upplands Väsby, Sollentuna, Sundbyberg, Stockholm, Ekerö
Lidingö <- mega_list$Lidingö # Neighbours: Vaxholm, Österåker, Täby, Danderyd, Stocholm, Nacka, Värmdö
Nacka <- mega_list$Nacka # Neighbours: Värmdö, Vaxholm, Lidingö, Stockholm, Tyresö
Norrtälje <- mega_list$Norrtälje # Neighbours: Sigtuna, Vallentuna, Österåker
Nykvarn <- mega_list$Nykvarn # Neighbours: Södertälje
Nynäshamn <- mega_list$Nynäshamn # Neighbours: Haninge, Botkyrka, Södertälje
Salem <- mega_list$Salem # Neighbours: Södertälje, Ekerö, Botkyrka
Sigtuna <- mega_list$Sigtuna # Neighbours: Norrtälje, Vallentuna, Upplands Väsby, Upplands Bro
Sollentuna <-mega_list$Sollentuna # Neighbours: Upplands Väsby, Täby, Dandedryd, Solna, Sundbyberg, Stockholm, Järfälla
Solna <- mega_list$Solna # Neighbours: Sundbyberg, Sollentuna, Danderyd, Stockholm
Stockholm <- mega_list$Stockholm # Neighbours: Järfälla, Sollentuna, Sundbyberg, Solna, Danderyd, Lidingö, Nacka, Tyresö, Huddinge, Botkykra, Ekerö
Sundbyberg <- mega_list$Sundbyberg # Neighbours: Stockholm, Järfälla, Sollentuna, Solna
Södertälje <- mega_list$Södertälje # Neighbours: Nykvarn, Salem, Botkyrka, Nynänshamn, Ekerö
Tyresö <- mega_list$Tyresö # Neighbours: Nacka, Stockholm, Huddinge, Haninge
Täby <- mega_list$Täby # Neighbours: Uppalnds Väsby, Vallentuna, Österåker, Vaxholm, Lidingö, Danderyd, Sollentuna
Upplands_Bro <-mega_list$`Upplands-Bro`# Neighbours: Sigtuna, Upplands Väsby, Järfälla, Ekerö
Upplands_Väsby <- mega_list$`Upplands Väsby`# Neighbours: Upplands-Bro, Sigtuna, Vallentuna, Täby, Sollentuna, Järfälla
Vallentuna <- mega_list$Vallentuna # Neighbours: Sigtuna, Norrtälje, Öteråker, Täby, Upplands Väsby 
Vaxholm <- mega_list$Vaxholm # Neighbours: Österåker, Värmdö, Nacka, Lidingö, Danderyd, Täby
Värmdö <- mega_list$Värmdö # Neighbours: Vaxholm, Lidingö, Nacka
Österåker <- mega_list$Österåker # Neighbours: Norrtälje, Vallentuna, Täby, Vaxholm

#Botkyrka

y_Past_Month_botkyrka <-  mega_list [[1]] [1:83, 17]
y_Two_Months_ago_botkyrka <- mega_list [[1]] [1:82, 17]
Botkyrka$Past_Month_fires <- c(0, y_Past_Month_botkyrka)
Botkyrka$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_botkyrka)

#Danderyd
y_Past_Month_Danderyd <-  mega_list [[2]] [1:83, 17]
y_Two_Months_ago_Danderyd <- mega_list [[2]] [1:82, 17]
Danderyd$Past_Month_fires <- c(0, y_Past_Month_Danderyd)
Danderyd$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Danderyd)

#Ekerö
y_Past_Month_Ekerö <-  mega_list [[3]] [1:83, 17]
y_Two_Months_ago_Ekerö <- mega_list [[3]] [1:82, 17]
Ekerö$Past_Month_fires <- c(0, y_Past_Month_Ekerö)
Ekerö$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Ekerö)

cor(Botkyrka$Number_of_Fires, Ekerö$Number_of_Fires)

#Haninge

y_Past_Month_Haninge <-  mega_list [[4]] [1:83, 17]
y_Two_Months_ago_Haninge <- mega_list [[4]] [1:82, 17]
Haninge$Past_Month_fires <- c(0, y_Past_Month_Haninge)
Haninge$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Haninge)

#Huddinge

y_Past_Month_Huddinge <-  mega_list [[5]] [1:83, 17]
y_Two_Months_ago_Huddinge <- mega_list [[5]] [1:82, 17]
Huddinge$Past_Month_fires <- c(0, y_Past_Month_Huddinge)
Huddinge$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Huddinge)

#Järfälla

y_Past_Month_Järfälla <-  mega_list [[6]] [1:83, 17]
y_Two_Months_ago_Järfälla <- mega_list [[6]] [1:82, 17]
Järfälla$Past_Month_fires <- c(0, y_Past_Month_Järfälla)
Järfälla$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Järfälla)


#Lidingö

y_Past_Month_Lidingö <-  mega_list [[7]] [1:83, 17]
y_Two_Months_ago_Lidingö <- mega_list [[7]] [1:82, 17]
Lidingö$Past_Month_fires <- c(0, y_Past_Month_Lidingö)
Lidingö$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Lidingö)


#Nacka

y_Past_Month_Nacka <-  mega_list [[8]] [1:83, 17]
y_Two_Months_ago_Nacka <- mega_list [[8]] [1:82, 17]
Nacka$Past_Month_fires <- c(0, y_Past_Month_Nacka)
Nacka$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Nacka)

#Norrtälje

y_Past_Month_Norrtälje <-  mega_list [[9]] [1:83, 17]
y_Two_Months_ago_Norrtälje <- mega_list [[9]] [1:82, 17]
Norrtälje$Past_Month_fires <- c(0, y_Past_Month_Norrtälje)
Norrtälje$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Norrtälje)


#Nykvarn

y_Past_Month_Nykvarn <-  mega_list [[10]] [1:83, 17]
y_Two_Months_ago_Nykvarn <- mega_list [[10]] [1:82, 17]
Nykvarn$Past_Month_fires <- c(0, y_Past_Month_Nykvarn)
Nykvarn$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Nykvarn)


#Nynäshamn

y_Past_Month_Nynäshamn <-  mega_list [[11]] [1:83, 17]
y_Two_Months_ago_Nynäshamn <- mega_list [[11]] [1:82, 17]
Nynäshamn$Past_Month_fires <- c(0, y_Past_Month_Nynäshamn)
Nynäshamn$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Nynäshamn)

# Salem

y_Past_Month_Salem <-  mega_list [[17]] [1:83, 17]
y_Two_Months_ago_Salem <- mega_list [[17]] [1:82, 17]
Salem$Past_Month_fires <- c(0, y_Past_Month_Salem)
Salem$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Salem)


# Sigtuna

y_Past_Month_Sigtuna <-  mega_list [[13]] [1:83, 17]
y_Two_Months_ago_Sigtuna <- mega_list [[13]] [1:82, 17]
Sigtuna$Past_Month_fires <- c(0, y_Past_Month_Sigtuna)
Sigtuna$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Sigtuna)


#Sollentuna

y_Past_Month_Sollentuna <-  mega_list [[14]] [1:83, 17]
y_Two_Months_ago_Sollentuna <- mega_list [[14]] [1:82, 17]
Sollentuna$Past_Month_fires <- c(0, y_Past_Month_Sollentuna)
Sollentuna$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Sollentuna)


#Solna


y_Past_Month_Solna <-  mega_list [[15]] [1:83, 17]
y_Two_Months_ago_Solna <- mega_list [[15]] [1:82, 17]
Solna$Past_Month_fires <- c(0, y_Past_Month_Solna)
Solna$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Solna)


# Stockholm

y_Past_Month_Stockholm <-  mega_list [[16]] [1:83, 17]
y_Two_Months_ago_Stockholm <- mega_list [[16]] [1:82, 17]
Stockholm$Past_Month_fires <- c(0, y_Past_Month_Stockholm)
Stockholm$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Stockholm)


# Sundbyberg 

y_Past_Month_Sundbyberg <-  mega_list [[17]] [1:83, 17]
y_Two_Months_ago_Sundbyberg <- mega_list [[17]] [1:82, 17]
Sundbyberg$Past_Month_fires <- c(0, y_Past_Month_Sundbyberg)
Sundbyberg$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Sundbyberg)


# Södertälje 

y_Past_Month_Södertälje <-  mega_list [[18]] [1:83, 17]
y_Two_Months_ago_Södertälje <- mega_list [[18]] [1:82, 17]
Södertälje$Past_Month_fires <- c(0, y_Past_Month_Södertälje)
Södertälje$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Södertälje)


# Tyresö

y_Past_Month_Tyresö <-  mega_list [[19]] [1:83, 17]
y_Two_Months_ago_Tyresö <- mega_list [[19]] [1:82, 17]
Tyresö$Past_Month_fires <- c(0, y_Past_Month_Tyresö)
Tyresö$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Tyresö)


# Täby

y_Past_Month_Täby <-  mega_list [[20]] [1:83, 17]
y_Two_Months_ago_Täby <- mega_list [[20]] [1:82, 17]
Täby$Past_Month_fires <- c(0, y_Past_Month_Täby)
Täby$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Täby)


# Upplands_Bro

y_Past_Month_Upplands_Bro <-  mega_list [[21]] [1:83, 17]
y_Two_Months_ago_Upplands_Bro <- mega_list [[21]] [1:82, 17]
Upplands_Bro$Past_Month_fires <- c(0, y_Past_Month_Upplands_Bro)
Upplands_Bro$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Upplands_Bro)


# Upplands_Väsby

y_Past_Month_Upplands_Väsby <-  mega_list [[22]] [1:83, 17]
y_Two_Months_ago_Upplands_Väsby <- mega_list [[22]] [1:82, 17]
Upplands_Väsby$Past_Month_fires <- c(0, y_Past_Month_Upplands_Väsby)
Upplands_Väsby$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Upplands_Väsby)


# Vallentuna

y_Past_Month_Vallentuna <-  mega_list [[23]] [1:83, 17]
y_Two_Months_ago_Vallentuna <- mega_list [[23]] [1:82, 17]
Vallentuna$Past_Month_fires <- c(0, y_Past_Month_Vallentuna)
Vallentuna$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Vallentuna)


# Vaxholm


y_Past_Month_Vaxholm <-  mega_list [[24]] [1:83, 17]
y_Two_Months_ago_Vaxholm <- mega_list [[24]] [1:82, 17]
Vaxholm$Past_Month_fires <- c(0, y_Past_Month_Vaxholm)
Vaxholm$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Vaxholm)


#Värmdö

y_Past_Month_Värmdö <-  mega_list [[25]] [1:83, 17]
y_Two_Months_ago_Värmdö <- mega_list [[25]] [1:82, 17]
Värmdö$Past_Month_fires <- c(0, y_Past_Month_Värmdö)
Värmdö$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Värmdö)


# Österåker

y_Past_Month_Österåker <-  mega_list [[26]] [1:83, 17]
y_Two_Months_ago_Österåker <- mega_list [[26]] [1:82, 17]
Österåker$Past_Month_fires <- c(0, y_Past_Month_Österåker)
Österåker$Two_Months_ago_Fires <- c(0,0, y_Two_Months_ago_Österåker)



######## Botkykrka ######


#Neighbours:Nynäshamn, Haninge, Huddinge, Stockholm, Ekerö, Salem, Södertälje


prot_botkyrka <- lm (formula = Botkyrka$Number_of_Fires ~ 
                    + Botkyrka$Number_Weekend_Days 
                    + Botkyrka$Holidays
                     + Nynäshamn$Number_of_Fires 
                     + Haninge$Number_of_Fires 
                     + Huddinge$Number_of_Fires 
                     + Stockholm$Number_of_Fires 
                     + Ekerö$Number_of_Fires 
                     + Salem$Number_of_Fires 
                     + Södertälje$Number_of_Fires )

prot_botkyrka_2 <- glm(formula = Botkyrka$Number_of_Fires ~ 
                         + Botkyrka$Number_Weekend_Days 
                       + Botkyrka$Holidays
                       + Nynäshamn$Number_of_Fires 
                       + Haninge$Number_of_Fires 
                       + Huddinge$Number_of_Fires 
                       + Stockholm$Number_of_Fires 
                       + Ekerö$Number_of_Fires 
                       + Salem$Number_of_Fires 
                       + Södertälje$Number_of_Fires, family = "poisson")

prot_botkyrka_3 <- glm.nb(formula = Botkyrka$Number_of_Fires ~ 
                            + Botkyrka$Number_Weekend_Days 
                          + Botkyrka$Holidays
                          + Nynäshamn$Number_of_Fires 
                          + Haninge$Number_of_Fires 
                          + Huddinge$Number_of_Fires 
                          + Stockholm$Number_of_Fires 
                          + Ekerö$Number_of_Fires 
                          + Salem$Number_of_Fires 
                          + Södertälje$Number_of_Fires)
mean(Botkyrka$Number_of_Fires)
summary(prot_botkyrka)
summary(prot_botkyrka_2)
summary(prot_botkyrka_3)
plot(prot_botkyrka$residuals)
plot(prot_botkyrka_2$residuals)
plot(prot_botkyrka_3$residuals)

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


