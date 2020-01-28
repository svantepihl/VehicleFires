require(tidyverse)
require(readxl)
require(lubridate)
dat_msb <- read_excel("msb.xlsx", 
                      col_types = c("date", "date", "text", 
                                    "text", "text", "numeric", "text", 
                                    "text"))

# change data-type kommun nummer to integer to make it easier to work with  
str(dat_msb$kommun)
dat_msb$kommun<- as.integer(dat_msb$kommun)
str(dat_msb$kommun)


#Data cleaning- create separate year, quarter, month, day, hour and minute variables. 
dat_msb$year <- year(dat_msb$datum)
dat_msb$quarter <- quarter(dat_msb$datum)
dat_msb$month <- month(dat_msb$datum)
dat_msb$day <- day(dat_msb$datum)
dat_msb$hour <- hour(dat_msb$tid)
dat_msb$minute <- minute(dat_msb$tid)

# subset: anlagd, fel i utrustning, okänd, annat
anlagda <- filter(dat_msb, dat_msb$BEJBbrandorsakText == "Avsiktlig brand")
fel_i_utrustning <- filter(dat_msb, dat_msb$BEJBbrandorsakText == "Fel i utrustning")
okända <- filter(dat_msb, dat_msb$BEJBbrandorsakText == "Okänd")
annat <- filter(dat_msb, dat_msb$BEJBbrandorsakText != "Fel i utrustning", dat_msb$BEJBbrandorsakText != "Avsiktlig brand", dat_msb$BEJBbrandorsakText != "Okänd")


dat_msb$BEJBbrandorsakText [dat_msb$BEJBbrandorsakText != "Fel i utrustning"& dat_msb$BEJBbrandorsakText != "Avsiktlig brand"& dat_msb$BEJBbrandorsakText != "Okänd"] <-"Annat"
  
dat_msb$BEJBbrandorsakText <- factor(dat_msb$BEJBbrandorsakText, levels = c("unkown", "other reason", "techincal malfunctioning", "arson"))


######### Anlagda Bilbränder  ############


