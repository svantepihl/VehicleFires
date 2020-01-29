######## Alla Bränder #########

# yearly, comparision betweeen different causes 

#Option 1
ggplot() + 
  geom_freqpoly(data = Arson, aes(x=year, colour ="red"), binwidth = 1) + 
  geom_freqpoly(data = Technical_malfunctioning, aes(x=year, colour = "blue"), binwidth=1) + geom_freqpoly(data = Others, aes(x=year, colour = "green"), binwidth=1) + geom_freqpoly(data = Unknown, aes(x=year, colour = "black"), binwidth=1)+
  theme(legend.position="right")  + scale_colour_identity(guide = "legend", name= "cause",  labels= c("unknown","technical malfunctioning", "other reason","arson" )) + labs(y = "amount") + xlim (1998, 2018) + guides(color = guide_legend(reverse = TRUE))


#Option 2
ggplot(dat_msb, binwidth=1, aes(x=year, colour = Reason)) + geom_freqpoly (binwidth = 1) + xlim (1998, 2018) + labs (y="amount")



#yearly, riket

dat_msb %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

#quarterly, riket
dat_msb %>% 
  ggplot(aes(quarter)) + geom_bar(aes(fill = as.factor(quarter))) + labs(y = "amount") + scale_fill_discrete(name = "Quarters", labels = c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec"))


#monthly, riket

#Create labels and breaks for the months
labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

breaks = seq(1, 12, by=1)

# Option 1
dat_msb %>% 
  ggplot(aes(month))+ geom_freqpoly(binwidth = 1) + scale_x_continuous(limits = c(1, 12), breaks=breaks, labels = labels) + labs(y = "amount")

# Option 2
dat_msb %>% 
  ggplot(aes(month))+ geom_bar(aes(fill = as.factor(month))) + 
  scale_x_continuous(breaks=breaks, labels = labels) + 
  labs(y = "amount") + scale_fill_discrete(name = "Month", labels = labels)



##### Anlagda Bilbränder, riket######

Arson <- filter(dat_msb, dat_msb$Reason == "Arson")

#yearly, riket

Arson %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

#quarterly, riket
Arson %>% 
  ggplot(aes(quarter)) + geom_bar(aes(fill = as.factor(quarter))) + labs(y = "amount") + scale_fill_discrete(name = "Quarters", labels = c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec"))
  

#monthly, riket

#Create labels and breaks for the months
labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

breaks = seq(1, 12, by=1)

# Option 1
Arson %>% 
  ggplot(aes(month))+ geom_freqpoly(binwidth = 1) + scale_x_continuous(limits = c(1, 12), breaks=breaks, labels = labels) + labs(y = "amount")

# Option 2
Arson %>% 
  ggplot(aes(month))+ geom_bar(aes(fill = as.factor(month))) + 
  scale_x_continuous(breaks=breaks, labels = labels) + 
  labs(y = "amount") + scale_fill_discrete(name = "Month", labels = labels)

# yearly, all län at once!!

ggplot(Arson, binwidth=1, aes(x=year, colour = län)) + geom_freqpoly (binwidth = 1) + xlim (1998, 2018) + labs (y="amount")

# yearly, län by län
Blekinge %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Dalarna %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Gotland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Gävleborg %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Halland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Jämtland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Jönköping %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Kalmar %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Kronoberg %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Norrbotten %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Skåne %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Stockholm %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Södermalm %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Uppsala %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Värmland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Västerbotten %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Västernorrland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Västmanland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Västra_Götland %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Örebro %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

Östergöta %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")


