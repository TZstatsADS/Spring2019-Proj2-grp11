setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/data")
library(data.table)
library(tibble)
library(dplyr)
library(bit64)
library(tidyverse)
library(stringr)
library(stringi)
library("ggmap")
# mykey = as.character(read.csv(file = "ggmap key.csv"), header = F)
register_google(key = mykey)


#### Read in data
taxi2016 = fread("../data/taxi trip/taxi 2016.csv")
taxi_zone = fread("../data/taxi trip/taxi_zone_lookup.csv")
taxi2016 = taxi2016[,-8]
names(taxi2016)
names(taxi_zone)

#############################################################################

#### Match pickup and drop-off zones
colnames(taxi_zone)[1] = "PULocationID"
taxi16 = inner_join(taxi2016, taxi_zone, by = "PULocationID")
names(taxi16)
colnames(taxi16)[8] = "PU.Borough"
colnames(taxi16)[9] = "PU.zone"

colnames(taxi_zone)[1] = "DOLocationID"
taxi16 = inner_join(taxi16, taxi_zone, by = "DOLocationID")
colnames(taxi16)[10] = "DO.Borough"
colnames(taxi16)[11] = "DO.zone"

### Delete useless info
View(taxi16)
taxi_2016 = taxi16[,-5]
View(taxi_2016)

#############################################################################
### Clean date and time
pu = as.character(taxi_2016$tpep_pickup_datetime)
do = as.character(taxi_2016$tpep_dropoff_datetime)

# Time
time.express = "[0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{2}"
pu.time = unlist(regmatches(pu, gregexpr(pu, pattern = time.express)))
do.time = unlist(regmatches(do, gregexpr(do, pattern = time.express)))

pu.hour = format(strptime(pu.time, "%I:%M:%S %p"), format="%H:%M:%S")
pu.hour = stri_extract_first_regex(pu.hour, "[0-9]{2}")
pu.hour = paste(pu.hour,":00:00", sep = "", collapse = " ")
pu.hour = unlist(strsplit(pu.hour, split = " "))

do.hour = format(strptime(do.time, "%I:%M:%S %p"), format="%H:%M:%S")
do.hour = stri_extract_first_regex(do.hour, "[0-9]{2}")
do.hour = paste(do.hour,":00:00", sep = "", collapse = " ")
do.hour = unlist(strsplit(do.hour, split = " "))


# Extract date only
date.express = "[0-9]{2}/[0-9]{2}"
year.express = "/[0-9]{4}"
pu.year = unlist(regmatches(pu, gregexpr(pu, pattern = year.express)))
pu.year = as.character(gsub("/", "", pu.year))
pu.date = unlist(regmatches(pu, gregexpr(pu, pattern = date.express)))
pu.date = as.character(gsub("/","-",pu.date))

do.year = unlist(regmatches(do, gregexpr(do, pattern = year.express)))
do.year = as.character(gsub("/", "", do.year))
do.date = unlist(regmatches(do, gregexpr(do, pattern = date.express)))
do.date = as.character(gsub("/","-",do.date))


pu.time = paste(pu.year, pu.date, sep = "-", collapse = " ")
pu.time = unlist(strsplit(pu.time, split = " "))
pu.date.time = paste(pu.time, pu.hour, sep = " ", collapse = ";")
pu.date.time = unlist(strsplit(pu.date.time, split = ";"))

do.time = paste(do.year, do.date, sep = "-", collapse = " ")
do.time = unlist(strsplit(do.time, split = " "))
do.date.time = paste(do.time, do.hour, sep = " ", collapse = ";")
do.date.time = unlist(strsplit(do.date.time, split = ";"))

# Update data
taxi_2016[,1] = pu.date.time
taxi_2016[,2] = do.date.time
colnames(taxi_2016)[1] = "PU.date"
colnames(taxi_2016)[2] = "DO.date"

taxi_2016$PU.hour = stri_extract_first_regex(pu.hour, "[0-9]{2}")
taxi_2016$DO.hour = stri_extract_first_regex(do.hour, "[0-9]{2}")

##################################################################################
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


write_csv(pu.geo, path = "../output/taxi2016_PUgeo.csv")
write_csv(do.geo, path = "../output/taxi2016_DOgeo.csv")

PU.geo = read_csv("../output/taxi2016_PUgeo.csv")
DO.geo = read_csv("../output/taxi2016_DOgeo.csv")

# Match geo


taxi.geo = inner_join(taxi_2016, pu.geo[,c(1,4,5)], by = "PU.zone")  # pickup
colnames(taxi.geo)[13] = "PU.lon"
colnames(taxi.geo)[14] = "PU.lat"
  
taxi.geo = inner_join(taxi.geo, do.geo[,c(1,4,5)], by = "DO.zone")  # drop-off
colnames(taxi.geo)[15] = "DO.lon"
colnames(taxi.geo)[16] = "DO.lat"
View(taxi.geo)

taxi2016_Clean = taxi.geo[,c(1,5,7,8,13,14,11,2,6,9,10,15,16,12,3,4)]
names(taxi2016_Clean)

##### Separate data set
row = nrow(taxi2016_Clean)
sp = round(nrow(taxi2016_Clean)/2)
write_csv(taxi2016_Clean[1:sp,], path = "../output/taxi2016_1.csv")
write_csv(taxi2016_Clean[(sp+1):row,], path = "../output/taxi2016_2.csv")
