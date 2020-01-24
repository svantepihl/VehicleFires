require(tidyverse)
require(readxl)
dat_msb <- read_excel("Räddningsinsatser till fordonsbränder utomhus 1998-2018.xlsx", 
                      col_types = c("date", "date", "text", 
                                    "text", "text", "numeric", "text", 
                                    "text"))




