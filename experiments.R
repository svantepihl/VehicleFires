#Create dataset with only years 2012-2019
dat_subset_12_19 <- filter(dat_stockholm, dat_stockholm$Year >=2012 )

# Select only fires with suspected arson
dat_subset_12_19 <- filter(dat_subset_12_19, dat_subset_12_19$Reason == "Arson")
summary(dat_subset_12_19$Holidays)


