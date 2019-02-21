setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output")
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

taxi2016 = fread("../data/taxi trip/taxi 2016.csv")
taxi2016_Clean = fread("taxi2016_Clean.csv")
taxi_zone = fread("../data/taxi trip/taxi_zone_lookup.csv")

View(taxi2016)
View(taxi2016_Clean)
View(taxi_zone)

taxi2016_PU = taxi2016[,c(1,6)]
View(taxi2016_PU)


#### Match Pickup ID with pickup location
colnames(taxi_zone)[1] = "PULocationID"
taxi2016_PU = inner_join(taxi2016_PU, taxi_zone, by = "PULocationID")

taxi2016_PU = taxi2016_PU[,-3]
View(taxi2016_PU)
PU.geo = taxi2016_Clean[,c(7,12,13)]
colnames(PU.geo)[1] = "Zone"
View(PU.geo)
count.PU.geo = PU.geo %>%
  group_by(`Zone`) %>%
  summarise(
    value = n()
  )
count.PU.geo = count.PU.geo[order(-count.PU.geo$value),]
View(count.PU.geo)

pu.geo =count.PU.geo %>%
  mutate(Add = paste(`Zone`, "New York, NY", sep = ","))%>%
  mutate_geocode(Add)
View(pu.geo)

pu.geo = read_csv("PickUp_Clean.csv")
taxi16_PU_geo = inner_join(taxi2016_PU, pu.geo[,c(1,4,5)], by = "Zone")
colnames(taxi16_PU_geo)[4] = "mean_long"
colnames(taxi16_PU_geo)[5] = "mean_lat"
View(taxi16_PU_geo)


#### Time and datas
pu = as.character(taxi16_PU_geo$tpep_pickup_datetime)

# Extract hours
time.express = "[0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{2}"
pu.time = unlist(regmatches(pu, gregexpr(pu, pattern = time.express)))
# sample = head(pu.time)
# format(strptime(sample, "%I:%M:%S %p"), format="%H:%M:%S")
pu.hour = format(strptime(pu.time, "%I:%M:%S %p"), format="%H:%M:%S")
pu.hour = stri_extract_first_regex(pu.hour, "[0-9]{2}")
pu.hour = paste(pu.hour,":00:00", sep = "", collapse = " ")
pu.hour = unlist(strsplit(pu.hour, split = " "))

# Extract date only
date.express = "[0-9]{2}/[0-9]{2}"
year.express = "/[0-9]{4}"
pu.year = unlist(regmatches(pu, gregexpr(pu, pattern = year.express)))
pu.year = as.character(gsub("/", "", pu.year))
pu.date = unlist(regmatches(pu, gregexpr(pu, pattern = date.express)))
pu.date = as.character(gsub("/","-",pu.date))

pu.time = paste(pu.year, pu.date, sep = "-", collapse = " ")
pu.time = unlist(strsplit(pu.time, split = " "))

pu.date.time = paste(pu.time, pu.hour, sep = " ", collapse = ";")
pu.date.time = unlist(strsplit(pu.date.time, split = ";"))


  # Update data
taxi16_PU_geo[,1] = pu.date.time
colnames(taxi16_PU_geo)[1] = "groups_byhour"

PU.16 = taxi16_PU_geo[,-3]
View(PU.16)
count.PU = PU.16 %>%
  group_by(`groups_byhour`) %>%
  summarise(
    value = n()
  )
View(count.PU)

PU.16 = inner_join(PU.16,count.PU, by = "groups_byhour")
PU.16 = PU.16[,c(5,2,1,4,3)]
colnames(PU.16) = c("Count_byhour", "locationID", "groups_byhour","mean_lat","mean_long")
write_csv(PU.16, path = "../output/PickUp2016_Clean.csv")

################################################################################ 2015

taxi2015 = fread("../data/taxi trip/taxi 2015.csv")
dim(taxi2015)
View(taxi2015)
taxi2015_PU = taxi2015[,1:4]
View(taxi2015_PU)

#### Time and datas
pu15 = as.character(taxi2015_PU$pickup_date)

# Extract hours
pu.hour15 = paste(taxi2015_PU$pickup_hour,":00:00", sep = "", collapse = " ")
pu.hour15 = unlist(strsplit(pu.hour15, split = " "))

# Extract date only
date.express = "[0-9]+/[0-9]{1,2}"
year.express = "/[0-9]{4}"
pu.year15 = unlist(regmatches(pu15, gregexpr(pu15, pattern = year.express)))
pu.year15 = as.character(gsub("/", "", pu.year15))
pu.date15 = unlist(regmatches(pu15, gregexpr(pu15, pattern = date.express)))
pu.date15 = as.character(gsub("/","-",pu.date15))

pu.day15 = paste(pu.year15, pu.date15, sep = "-", collapse = " ")
pu.day15 = unlist(strsplit(pu.day15, split = " "))

pu.date.time15 = paste(pu.day15, pu.hour15, sep = " ", collapse = ";")
pu.date.time15 = unlist(strsplit(pu.date.time15, split = ";"))

# Update data
taxi2015_PU[,4] = pu.date.time15
colnames(taxi2015_PU)[4] = "groups_byhour"


count.PU15 = taxi2015_PU %>%
  group_by(`groups_byhour`) %>%
  summarise(
    value = n()
  )
View(count.PU15)

PU.15 = inner_join(taxi2015_PU,count.PU15, by = "groups_byhour")
PU.15 = PU.15[,-3]
PU.15 = PU.15[,c(4,3,2,1)]
colnames(PU.15) = c("Count_byhour", "groups_byhour","mean_lat","mean_long")
write_csv(PU.15, path = "../output/PickUp2015_Clean.csv")