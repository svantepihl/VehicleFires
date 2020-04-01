require(survival)
require(ranger)
require(ggfortify)
require(lubridate)
require(dplyr)
require(anytime)



# differences between holiday and non-holiday days
x <- filter(dat_stockholm, dat_stockholm$Year == 2017| dat_stockholm$Year == 2018 | dat_stockholm$Year == 2019 )
x<- filter(x, x$Reason == "Arson")
x_1 <- filter(x, x$Sport_Holidays == TRUE)
x_2 <- filter( x, x$Week == 12)


str(dat_stockholm$Date)

surv_sto <- numeric(dat_stockholm$Date)


surv_sto <- Surv(dat_stockholm$Date)

N <- length(dat_stockholm$Date)

for (i in vector N) {difftime(i+1 - i)
  
}

difftime(dat_stockholm$Date, dat_stockholm$Date[i+1])

differences <- difftime(dat_stockholm$Time [-1], dat_stockholm$Time[-nrow(dat_stockholm$Date), units = "hours"])

diff <- c(0, difftime(dat_stockholm$Time [-1], dat_stockholm$Time[-length(dat_stockholm$Date), units ="mins"]))                         
  
length(dat_stockholm$Time)
dat_stockholm$Time <- na.omit(dat_stockholm$Time)

dat_stockholm$time_correct <-strftime(dat_stockholm$Time, "%H:%M:%S", tz = "CET")

