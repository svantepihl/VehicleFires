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
labels_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

breaks_month = seq(1, 12, by=1)

# Option 1
dat_msb %>% 
  ggplot(aes(month))+ geom_freqpoly(binwidth = 1) + scale_x_continuous(limits = c(1, 12), breaks=breaks_month, labels = labels_month) + labs(y = "amount")

# Option 2
dat_msb %>% 
  ggplot(aes(month))+ geom_bar(aes(fill = as.factor(month))) + 
  scale_x_continuous(breaks=breaks, labels = labels) + 
  labs(y = "amount") + scale_fill_discrete(name = "Month", labels = labels)

# Day of the month, riket 
#Option 1
dat_msb %>% 
  ggplot(aes(day))+ geom_freqpoly(binwidth = 1) + scale_x_continuous(limits = c(1, 7), breaks=breaks_days, labels = labels_days) + labs(y = "amount")

#Option 2
dat_msb %>% 
  ggplot(aes(day))+ geom_bar(aes(fill = as.factor(day))) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels)

# Weekday, riket

breaks_weekdays = seq(1, 7, by=1)
labels_weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

dat_msb %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(weekday))) +
 scale_x_discrete (breaks=breaks_weekdays, labels = labels_weekdays) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels_weekdays)

# weekdays okända, riket

# Weekday, arson, riket

breaks_weekdays = seq(1, 7, by=1)
labels_weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

Arson %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(weekday))) +
  scale_x_discrete (breaks=breaks_weekdays, labels = labels_weekdays) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels_weekdays)

# weekdays okända, riket

Unknown %>%
  
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(weekday))) +
  scale_x_discrete (breaks=breaks_weekdays, labels = labels_weekdays) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels_weekdays)

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

# Weekdays, Riket

breaks_weekdays = seq(1, 7, by=1)
labels_weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

Arson %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = weekday)) +
  scale_x_discrete (labels = labels_weekdays) + 
  labs(y = "amount") + scale_fill_discrete(name = "Day", labels = labels_weekdays)

# Hours, riket

breaks_hours = seq(1, 24, by=1)
labels_hours = c("00-01", "01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24")


Arson %>% 
  ggplot(aes(hour)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Arson %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

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

# Weekday and time, län by län

Blekinge %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Dalarna %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Gotland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour")

Gävleborg %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Halland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Jämtland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Jönköping %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Kalmar %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Kronoberg %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Norrbotten %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Skåne %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Stockholm %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Södermalm %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Uppsala %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Värmland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Västerbotten %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Västernorrland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Västmanland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Västra_Götland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Örebro %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Östergöta %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Västernorrland %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

# Year by Year Visulaisations
  
Ninety_eight %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Ninety_nine %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours)

Eighteen %>% 
  ggplot(aes(weekday)) + geom_bar(aes(fill = as.factor(hour))) +
  scale_x_discrete (breaks=breaks_hours, labels = labels_hours) + 
  labs(y = "amount") + scale_fill_discrete(name = "Hour", labels = labels_hours) 


