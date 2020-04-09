require(tidyverse)
require(ggplot2)
require(survival)
require(lubridate)

dat_poisson <- subset(dat_days_stockholm)

dat_poisson <- dat_days_stockholm
dat_poisson$Year <- year(dat_poisson$Date)
dat_poisson$Weekend <- F
dat_poisson$Weekend[dat_poisson$Weekday == "Saturday" | dat_poisson$Weekday == "Sunday"] <- T 
dat_poisson$Type_of_Municipality_Code <- as.factor(dat_poisson$Type_of_Municipality_Code)

# Check for equal mean and variance
var(dat_days_stockholm$Count)
mean(dat_days_stockholm$Count)

model <- (glm(formula = Count ~ Type_of_Municipality_Code+Weekend+Temperature+Holidays+Year,data = dat_poisson, family = poisson))
summary(model)
model$fitted.values
summary(dat_poisson$Type_of_Municipality_Code)
