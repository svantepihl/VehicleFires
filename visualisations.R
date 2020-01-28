##### Anlagda Bilbränder, riket######

Arson <- filter(dat_msb, dat_msb$Reason == "Arson")

#Create labels for the months
labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# yearly, comparision betweeen different causes 
breaks = seq(1998, 2018, by=1)

#Option 1
ggplot() + 
  geom_freqpoly(data = Arson, aes(x=year, colour ="red"), binwidth = 1) + 
  geom_freqpoly(data = Technical_malfunctioning, aes(x=year, colour = "blue"), binwidth=1) + geom_freqpoly(data = Others, aes(x=year, colour = "green"), binwidth=1) + geom_freqpoly(data = Unknown, aes(x=year, colour = "black"), binwidth=1)+
  theme(legend.position="right")  + scale_colour_identity(guide = "legend", name= "cause",  labels= c("unknown","technical malfunctioning", "other Reason","arson" )) + labs(y = "amount") + xlim (1998, 2018) + guides(color = guide_legend(reverse = TRUE))

#Option 2
ggplot(dat_msb, binwidth=1, aes(x=year, colour = Reason)) + geom_freqpoly (binwidth = 1) + xlim (1998, 2018) + labs (y="amount")


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


# yearly, all län at once!!

ggplot(Arson, binwidth=1, aes(x=year, colour = län)) + geom_freqpoly (binwidth = 1) + xlim (1998, 2018) + labs (y="amount")

# yearly, län by län
Blekinge %>% 
  ggplot(aes(year)) + geom_freqpoly(binwidth = 1) + xlim (1998, 2018) + labs(y = "amount")
# etc.etc.etc.

