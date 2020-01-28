##### Anlagda Bilbränder, riket######

#Create breaks to apply to x-axe, aesthetic reasons to have more precise labels on the x-axe 


#Create labels for the months
labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# yearly, comparision betweeen different causes 
breaks = seq(1998, 2018, by=1)

ggplot() + 
  geom_freqpoly(data = anlagda, aes(x=year, colour ="red"), binwidth = 1) + 
  geom_freqpoly(data = fel_i_utrustning, aes(x=year, colour = "blue"), binwidth=1) + geom_freqpoly(data = annat, aes(x=year, colour = "green"), binwidth=1) + geom_freqpoly(data = okända, aes(x=year, colour = "black"), binwidth=1)+
  theme(legend.position="right")  + scale_colour_identity(guide = "legend", name= "cause",  labels= c("unknown","technical malfunctioning", "other reason","arson" )) + labs(y = "amount") + xlim (1998, 2018) + guides(color = guide_legend(reverse = TRUE))


#yearly, riket
anlagda %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")

#quarterly, riket
anlagda %>% 
  ggplot(aes(quarter)) + geom_freqpoly(binwidth = 1) + xlim (1,4) + labs(y = "amount")

#monthly, riket
breaks = seq(1, 12, by=1)
anlagda %>% 
  ggplot(aes(month)) + geom_freqpoly(binwidth = 1) + scale_x_continuous(limits = c(1, 12), breaks=breaks, labels = labels) + labs(y = "amount")

min(annat$year)
max(annat$year)

filter(annat, year=="1998")
filter(annat, year=="1999")
filter(annat, year=="2000")

# yearly, län
Blekinge %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")
