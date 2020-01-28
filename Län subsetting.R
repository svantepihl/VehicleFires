
# change data-type kommun nummer to integer to make it easier to work with  
str(dat_msb$kommun)
dat_msb$kommun<- as.integer(dat_msb$kommun)
str(dat_msb$kommun)

str(Arson$kommun)
Arson$kommun<- as.integer(Arson$kommun)
str(Arson$kommun)

#Organisera enligt län

# 01 Stockholm

Stockholm <- filter(Arson, Arson$kommun < 200  )

# 03 Uppsala

Uppsala <- filter(Arson, Arson$kommun > 299 & Arson$kommun < 400 )

# 04 Södermalmland

Södermalm <- filter(Arson, Arson$kommun > 399 & Arson$kommun < 500 )
 
# 05 Östergötland

Östergöta  <- filter(Arson, Arson$kommun > 499 & Arson$kommun < 600 )

# 06 Jönköping

Jönköping <- filter(Arson, Arson$kommun > 599 & Arson$kommun < 700 )

# 07 Kronoberg

Kronoberg <- filter(Arson, Arson$kommun > 699 & Arson$kommun < 800 )

# 08 Kalmar

Kalmar <- filter(Arson, Arson$kommun > 799 & Arson$kommun < 900 )

# 09 Gotland

Gotland <- filter(Arson, Arson$kommun > 899 & Arson$kommun < 1000 )

# 10 Blekinge

Blekinge <- filter(Arson, Arson$kommun > 999 & Arson$kommun < 1100 )

# 12 Skåne

Skåne <- filter(Arson, Arson$kommun > 1199 & Arson$kommun < 1300)

# 13 Halland

Halland <- filter(Arson, Arson$kommun > 1299 & Arson$kommun < 1400)

# 14 Västra Götland

Västra_Götland <- filter(Arson, Arson$kommun > 1399 & Arson$kommun < 1500)

# 17 Värmland

Värmland  <- filter(Arson, Arson$kommun > 1699 & Arson$kommun < 1800)

# 18 Örebro

Örebro <- filter(Arson, Arson$kommun > 1799 & Arson$kommun < 1900)

# 19 Västmanland

Västmanland <- filter(Arson, Arson$kommun > 1899 & Arson$kommun < 2000)

# 20 Dalarna

Dalarna <- filter(Arson, Arson$kommun > 1999 & Arson$kommun < 2100)

# 21 Gävleborg

Gävleborg <- filter(Arson, Arson$kommun > 2099 & Arson$kommun < 2200)

# 22 Västernorlland

Västernorrland <- filter(Arson, Arson$kommun > 2199 & Arson$kommun < 2300)

# 23 Jämtland

Jämtland <- filter(Arson, Arson$kommun > 2299 & Arson$kommun < 2400)
# 24 Västerbotten

Västerbotten <- filter(Arson, Arson$kommun > 2399 & Arson$kommun < 2500)

# 25 Norrbotten

Norrbotten <- filter(Arson, Arson$kommun > 2499 & Arson$kommun < 2600)




####### We add the Län to the data-frame ######

dat_msb$län <- dat_msb$kommun

str(dat_msb$län)
dat_msb$län <- as.integer(dat_msb$län)
str(dat_msb$län)


# 01 Stockholm

dat_msb$län [dat_msb$län < 200] <- "Stockholm"

# 03 Uppsala

dat_msb$län [dat_msb$län > 299 & dat_msb$län < 400] <- "Uppsala"

# 04 Södermalmland

dat_msb$län [dat_msb$län > 399 & dat_msb$län < 500] <- "Södermalm"

# 05 Östergötland

dat_msb$län [dat_msb$län > 499 & dat_msb$län < 600] <- "Östergöta"

# 06 Jönköping

dat_msb$län [dat_msb$län > 599 & dat_msb$län < 700] <- "Jönköping"

# 07 Kronoberg

dat_msb$län [dat_msb$län > 699 & dat_msb$län < 800] <- "Kronoberg"

# 08 Kalmar

dat_msb$län [dat_msb$län > 799 & dat_msb$län < 900] <- "Kalmar"

# 09 Gotland (seems to have a weird bug, that´s why I rewrote it to another digit than above)

dat_msb$län [dat_msb$län == 980] <- "Gotland"

# 10 Blekinge(seems to have a weird bug, that´s why I rewrote it other digits than above)

dat_msb$län [dat_msb$län > 1050 & dat_msb$län < 1095] <- "Blekinge"

# 12 Skåne

dat_msb$län [dat_msb$län > 1199 & dat_msb$län < 1300] <- "Skåne"

# 13 Halland

dat_msb$län [dat_msb$län > 1299 & dat_msb$län < 1400] <- "Halland"

# 14 Västra Götland

dat_msb$län [dat_msb$län > 1399 & dat_msb$län < 1500] <- "Västra_Götland"

# 17 Värmland
 
dat_msb$län [dat_msb$län > 1699 & dat_msb$län < 1800] <- "Värmland" 

# 18 Örebro

dat_msb$län [dat_msb$län > 1799 & dat_msb$län < 1900] <- "Örebro"

# 19 Västmanland

dat_msb$län [dat_msb$län > 1899 & dat_msb$län < 2000] <- "Västmanland"

# 20 Dalarna

dat_msb$län [dat_msb$län > 1999 & dat_msb$län < 2100] <- "Dalarna"

# 21 Gävleborg

dat_msb$län [dat_msb$län > 2099 & dat_msb$län < 2200] <- "Gävleborg"

# 22 Västernorlland

dat_msb$län [dat_msb$län > 2199 & dat_msb$län < 2300] <- "Västernorrland"

# 23 Jämtland

dat_msb$län [dat_msb$län > 2299 & dat_msb$län < 2400] <- "Jämtland"

# 24 Västerbotten

dat_msb$län [dat_msb$län > 2399 & dat_msb$län < 2500] <- "Västerbotten"

# 25 Norrbotten

dat_msb$län [dat_msb$län > 2499 & dat_msb$län < 2600] <- "Norrbotten"




