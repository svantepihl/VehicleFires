
# change data-type kommun_code nummer to integer to make it easier to work with  
str(dat_2018$kommun_code)
dat_2018$kommun_code<- as.integer(dat_2018$kommun_code)
str(dat_2018$kommun_code)

str(Arson_2018$kommun_code)
Arson_2018$kommun_code<- as.integer(Arson_2018$kommun_code)
str(Arson_2018$kommun_code)



#Organisera enligt län

# 01 Stockholm

Stockholm <- filter(Arson_2018, Arson_2018$kommun_code < 200  )

# 03 Uppsala

Uppsala <- filter(Arson_2018, Arson_2018$kommun_code > 299 & Arson_2018$kommun_code < 400 )

# 04 Södermalmland

Södermalm <- filter(Arson_2018, Arson_2018$kommun_code > 399 & Arson_2018$kommun_code < 500 )

# 05 Östergötland

Östergöta  <- filter(Arson_2018, Arson_2018$kommun_code > 499 & Arson_2018$kommun_code < 600 )

# 06 Jönköping

Jönköping <- filter(Arson_2018, Arson_2018$kommun_code > 599 & Arson_2018$kommun_code < 700 )

# 07 Kronoberg

Kronoberg <- filter(Arson_2018, Arson_2018$kommun_code > 699 & Arson_2018$kommun_code < 800 )

# 08 Kalmar

Kalmar <- filter(Arson_2018, Arson_2018$kommun_code > 799 & Arson_2018$kommun_code < 900 )

# 09 Gotland

Gotland <- filter(Arson_2018, Arson_2018$kommun_code > 899 & Arson_2018$kommun_code < 1000 )

# 10 Blekinge

Blekinge <- filter(Arson_2018, Arson_2018$kommun_code > 999 & Arson_2018$kommun_code < 1100 )

# 12 Skåne

Skåne <- filter(Arson_2018, Arson_2018$kommun_code > 1199 & Arson_2018$kommun_code < 1300)

# 13 Halland

Halland <- filter(Arson_2018, Arson_2018$kommun_code > 1299 & Arson_2018$kommun_code < 1400)

# 14 Västra Götland

Västra_Götland <- filter(Arson_2018, Arson_2018$kommun_code > 1399 & Arson_2018$kommun_code < 1500)

# 17 Värmland

Värmland  <- filter(Arson_2018, Arson_2018$kommun_code > 1699 & Arson_2018$kommun_code < 1800)

# 18 Örebro

Örebro <- filter(Arson_2018, Arson_2018$kommun_code > 1799 & Arson_2018$kommun_code < 1900)

# 19 Västmanland

Västmanland <- filter(Arson_2018, Arson_2018$kommun_code > 1899 & Arson_2018$kommun_code < 2000)

# 20 Dalarna

Dalarna <- filter(Arson_2018, Arson_2018$kommun_code > 1999 & Arson_2018$kommun_code < 2100)

# 21 Gävleborg

Gävleborg <- filter(Arson_2018, Arson_2018$kommun_code > 2099 & Arson_2018$kommun_code < 2200)

# 22 Västernorlland

Västernorrland <- filter(Arson_2018, Arson_2018$kommun_code > 2199 & Arson_2018$kommun_code < 2300)

# 23 Jämtland

Jämtland <- filter(Arson_2018, Arson_2018$kommun_code > 2299 & Arson_2018$kommun_code < 2400)
# 24 Västerbotten

Västerbotten <- filter(Arson_2018, Arson_2018$kommun_code > 2399 & Arson_2018$kommun_code < 2500)

# 25 Norrbotten

Norrbotten <- filter(Arson_2018, Arson_2018$kommun_code > 2499 & Arson_2018$kommun_code < 2600)




####### We add the Län to the data-frame ######

dat_2018$län <- dat_2018$kommun_code

str(dat_2018$län)
dat_2018$län <- as.integer(dat_2018$län)
str(dat_2018$län)


# 01 Stockholm

dat_2018$län [dat_2018$län < 200] <- "Stockholm"

# 03 Uppsala

dat_2018$län [dat_2018$län > 299 & dat_2018$län < 400] <- "Uppsala"

# 04 Södermalmland

dat_2018$län [dat_2018$län > 399 & dat_2018$län < 500] <- "Södermalm"

# 05 Östergötland

dat_2018$län [dat_2018$län > 499 & dat_2018$län < 600] <- "Östergöta"

# 06 Jönköping

dat_2018$län [dat_2018$län > 599 & dat_2018$län < 700] <- "Jönköping"

# 07 Kronoberg

dat_2018$län [dat_2018$län > 699 & dat_2018$län < 800] <- "Kronoberg"

# 08 Kalmar

dat_2018$län [dat_2018$län > 799 & dat_2018$län < 900] <- "Kalmar"

# 09 Gotland (seems to have a weird bug, that´s why I rewrote it to another digit than above)

dat_2018$län [dat_2018$län == 980] <- "Gotland"

# 10 Blekinge(seems to have a weird bug, that´s why I rewrote it other digits than above)

dat_2018$län [dat_2018$län > 1050 & dat_2018$län < 1095] <- "Blekinge"

# 12 Skåne

dat_2018$län [dat_2018$län > 1199 & dat_2018$län < 1300] <- "Skåne"

# 13 Halland

dat_2018$län [dat_2018$län > 1299 & dat_2018$län < 1400] <- "Halland"

# 14 Västra Götland

dat_2018$län [dat_2018$län > 1399 & dat_2018$län < 1500] <- "Västra_Götland"

# 17 Värmland

dat_2018$län [dat_2018$län > 1699 & dat_2018$län < 1800] <- "Värmland" 

# 18 Örebro

dat_2018$län [dat_2018$län > 1799 & dat_2018$län < 1900] <- "Örebro"

# 19 Västmanland

dat_2018$län [dat_2018$län > 1899 & dat_2018$län < 2000] <- "Västmanland"

# 20 Dalarna

dat_2018$län [dat_2018$län > 1999 & dat_2018$län < 2100] <- "Dalarna"

# 21 Gävleborg

dat_2018$län [dat_2018$län > 2099 & dat_2018$län < 2200] <- "Gävleborg"

# 22 Västernorlland

dat_2018$län [dat_2018$län > 2199 & dat_2018$län < 2300] <- "Västernorrland"

# 23 Jämtland

dat_2018$län [dat_2018$län > 2299 & dat_2018$län < 2400] <- "Jämtland"

# 24 Västerbotten

dat_2018$län [dat_2018$län > 2399 & dat_2018$län < 2500] <- "Västerbotten"

# 25 Norrbotten

dat_2018$län [dat_2018$län > 2499 & dat_2018$län < 2600] <- "Norrbotten"



