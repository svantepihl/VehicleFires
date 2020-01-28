
######### Anlagda Bilbränder  ############

#Organisera enligt län

# 01 Stockholm

Stockholm <- filter(anlagda, anlagda$kommun < 200  )

# 03 Uppsala

Uppsala <- filter(anlagda, anlagda$kommun > 299, anlagda$kommun < 400 )

# 04 Södermalmland

Södermalm <- filter(anlagda, anlagda$kommun > 399, anlagda$kommun < 500 )

# 05 Östergötland

Östergöta  <- filter(anlagda, anlagda$kommun > 499, anlagda$kommun < 600 )

# 06 Jönköping

Jönköping <- filter(anlagda, anlagda$kommun > 599, anlagda$kommun < 700 )

# 07 Kronoberg

Kronoberg <- filter(anlagda, anlagda$kommun > 699, anlagda$kommun < 800 )

# 08 Kalmar

Kalmar <- filter(anlagda, anlagda$kommun > 799, anlagda$kommun < 900 )

# 09 Gotland

Gotland <- filter(anlagda, anlagda$kommun > 899, anlagda$kommun < 1000 )

# 10 Blekinge

Blekinge <- filter(anlagda, anlagda$kommun > 999, anlagda$kommun < 1100 )

# 12 Skåne

Skåne <- filter(anlagda, anlagda$kommun > 1199, anlagda$kommun < 1300)

# 13 Halland

Halland <- filter(anlagda, anlagda$kommun > 1299, anlagda$kommun < 1400)

# 14 Västra Götland

Västra_Götland <- filter(anlagda, anlagda$kommun > 1399, anlagda$kommun < 1500)

# 17 Värmland

Värmland  <- filter(anlagda, anlagda$kommun > 1699, anlagda$kommun < 1800)

# 18 Örebro

Örebro <- filter(anlagda, anlagda$kommun > 1799, anlagda$kommun < 1900)

# 19 Västmanland

Västmanland <- filter(anlagda, anlagda$kommun > 1899, anlagda$kommun < 2000)

# 20 Dalarna

Dalarna <- filter(anlagda, anlagda$kommun > 1999, anlagda$kommun < 2100)

# 21 Gävleborg

Gävleborg <- filter(anlagda, anlagda$kommun > 2099, anlagda$kommun < 2200)

# 22 Västernorlland

Västernorrland <- filter(anlagda, anlagda$kommun > 2199, anlagda$kommun < 2300)

# 23 Jämtland

Jämtland <- filter(anlagda, anlagda$kommun > 2299, anlagda$kommun < 2400)
# 24 Västerbotten

Västerbotten <- filter(anlagda, anlagda$kommun > 2399, anlagda$kommun < 2500)

# 25 Norrbotten

Norrbotten <- filter(anlagda, anlagda$kommun > 2499, anlagda$kommun < 2600)
