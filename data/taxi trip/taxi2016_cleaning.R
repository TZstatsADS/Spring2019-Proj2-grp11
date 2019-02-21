setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/data")
library(data.table)
library(tibble)
library(dplyr)
library(bit64)
library(tidyverse)
library(stringr)
library(stringi)
library("ggmap")

mykey = "AIzaSyDj6W8F2IcxkcEnW-aKGmcJovBpMAhIAt4"
register_google(key = mykey)

#### Read in data
taxi2016 = fread("../data/taxi trip/taxi 2016.csv")
taxi_zone = fread("../data/taxi trip/taxi_zone_lookup.csv")

names(taxi2016)
names(taxi_zone)

#### Match pickup and drop-off zones
colnames(taxi_zone)[1] = "PULocationID"
taxi16 = inner_join(taxi2016, taxi_zone, by = "PULocationID")
names(taxi16)
colnames(taxi16)[9] = "PU.Borough"
colnames(taxi16)[10] = "PU.zone"

colnames(taxi_zone)[1] = "DOLocationID"
taxi16 = inner_join(taxi16, taxi_zone, by = "DOLocationID")
colnames(taxi16)[11] = "DO.Borough"
colnames(taxi16)[12] = "DO.zone"

### Delete useless info
View(taxi16)
taxi_2016 = taxi16[,-c(5,6,7)]
View(taxi_2016)

### Clean date and time
pu = as.character(taxi_2016$tpep_pickup_datetime)
do = as.character(taxi_2016$tpep_dropoff_datetime)

# Extract hours
time.express = "[0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{2}"
pu.time = unlist(regmatches(pu, gregexpr(pu, pattern = time.express)))
do.time = unlist(regmatches(do, gregexpr(do, pattern = time.express)))

pu.hour = stri_extract_first_regex(pu.time, "[0-9]{2}")
do.hour = stri_extract_first_regex(do.time, "[0-9]{2}")

# Update data
taxi_2016$PU.hour = pu.hour
taxi_2016$DO.hour = do.hour

# Extract date only
date.express = "[0-9]{2}/[0-9]{2}/[0-9]{4}"
pu.date = unlist(regmatches(pu, gregexpr(pu, pattern = date.express)))
do.date = unlist(regmatches(do, gregexpr(do, pattern = date.express)))

# Update data
taxi_2016[,1] = pu.date
taxi_2016[,2] = do.date
colnames(taxi_2016)[1] = "PU.date"
colnames(taxi_2016)[2] = "DO.date"

write_csv(taxi_2016, path = "../output/taxi2016_TimeClean.csv")

taxi_2016 = fread("../output/taxi2016_TimeClean.csv")


### Get longitude and latitude

# Count Pickup zones
count.pick = taxi_2016 %>%
  group_by(`PU.zone`) %>%
  summarise(
    value = n()
  )
count.pick = count.pick[order(-count.pick$value),]
View(count.pick)

# Count Drop-off zones
count.drop = taxi_2016 %>%
  group_by(`DO.zone`) %>%
  summarise(
    value = n()
  )
count.drop = count.drop[order(-count.drop$value),]
View(count.drop)

# get geographical info:
pu.geo =count.pick %>%
  mutate(Add = paste(`PU.zone`, "New York, NY", sep = ","))%>%
  mutate_geocode(Add)

do.geo =count.drop %>%
  mutate(Add = paste(`DO.zone`, "New York, NY", sep = ","))%>%
  mutate_geocode(Add)

View(pu.geo)

PU.count = pu.geo[,2:5]
DO.count = do.geo[,2:5]

write_csv(PU.count, path = "../output/taxi2016_PUcount.csv")
write_csv(DO.count, path = "../output/taxi2016_DOcount.csv")

PU.count = read_csv("../output/taxi2016_PUcount.csv")
DO.count = read_csv("../output/taxi2016_DOcount.csv")

# Match geo


taxi.geo = inner_join(taxi_2016, pu.geo[,c(1,4,5)], by = "PU.zone")  # pickup
colnames(taxi.geo)[12] = "PU.lon"
colnames(taxi.geo)[13] = "PU.lat"
  
taxi.geo = inner_join(taxi.geo, do.geo[,c(1,4,5)], by = "DO.zone")  # drop-off
colnames(taxi.geo)[14] = "DO.lon"
colnames(taxi.geo)[15] = "DO.lat"
View(taxi.geo)

# Delete useless info
#taxi2016_clean = taxi.geo[,-c(6,7)]
#View(taxi2016_clean)
#write_csv(taxi2016_clean, path = "../output/taxi2016_Clean.csv")
write_csv(taxi.geo, path = "../output/taxi2016_Clean.csv")





#####################   For adding Borough
taxi_2016 = fread("../output/taxi2016_TimeClean.csv")
taxi2016_Clean = read_csv("../output/taxi2016_Clean.csv")
View(taxi2016_Clean)
taxi.geo = data.frame(taxi_2016, taxi2016_Clean[,10:13])
