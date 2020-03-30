


x <- filter(dat_stockholm, dat_stockholm$Year == 2017| dat_stockholm$Year == 2018 | dat_stockholm$Year == 2019 )
x<- filter(x, x$Reason == "Arson")
x_1 <- filter(x, x$Holidays == TRUE)
x_2 <- filter( x, x$Holidays == FALSE)
