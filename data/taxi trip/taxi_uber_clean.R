setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output")

library(data.table)
library(tibble)
library(dplyr)
library(bit64)
library(tidyverse)
library(stringr)
library(stringi)

PU.16=fread("PickUp2016_Clean.csv")
PU.15=fread("PickUp2015_Clean.csv")

View(PU.15)
pu15 = as.character(PU.15$groups_byhour)
pu16 = as.character(PU.16$groups_byhour)

# Time

# time format about 2015
time.express = "[0-9]+:00:00"
pu15.time = unlist(regmatches(pu15, gregexpr(pu15, pattern = time.express)))
pu16.time = unlist(regmatches(pu16, gregexpr(pu16, pattern = time.express)))

pu.hour15 = format(strptime(pu15.time, "%H:%M:%S"), format="%H:%M:%S")  # let hour be 2 digits
pu.hour16 = format(strptime(pu16.time, "%H:%M:%S"), format="%H:%M:%S")  # let hour be 2 digits

# Extract date only
date.express = "[0-9]{4}-[0-9]+-[0-9]+"
pu15.date = unlist(regmatches(pu15, gregexpr(pu15, pattern = date.express)))
pu16.date = unlist(regmatches(pu16, gregexpr(pu16, pattern = date.express)))

pu.date15 = as.Date(pu15.date)
pu.date16 = as.Date(pu16.date)

pu16.date.time = paste(pu.date16, pu.hour16, sep = " ", collapse = ";")
pu16.date.time = unlist(strsplit(pu16.date.time, split = ";"))

pu15.date.time = paste(pu.date15, pu.hour15, sep = " ", collapse = ";")
pu15.date.time = unlist(strsplit(pu15.date.time, split = ";"))

# Update data
PU.15[,2] =pu15.date.time
PU.16[,3] =pu16.date.time
PU.16 = PU.16[,-2]
write_csv(PU.15, path = "../output/PU15_Update.csv")
write_csv(PU.16, path = "../output/PU16_Update.csv")

####################################################################   Clean test.Uber

UberSample=fread("../data/taxi trip/test_uber.csv")
View(UberSample)
uber = UberSample[,-c(1,2,4)]
View(uber)

time.express = "[0-9]+:00:00"
uber.time = unlist(regmatches(uber$groups_byhour, 
                              gregexpr(uber$groups_byhour, pattern = time.express)))
uber.hour = format(strptime(uber.time, "%H:%M:%S"), format="%H:%M:%S")  # let hour be 2 digits


date.express = "[0-9]{4}-[0-9]+-[0-9]+"
uber.date = unlist(regmatches(uber$groups_byhour, 
                              gregexpr(uber$groups_byhour, pattern = date.express)))

uber.date.time = paste(uber.date, uber.hour, sep = " ", collapse = ";")
uber.date.time = unlist(strsplit(uber.date.time, split = ";"))

uber[,2] = uber.date.time
write_csv(uber, path = "../output/testUber_Update.csv")

###### ###### ####### Below is not related to this data processing 
time = "1"
format(strptime(time, "%H"), format="%H:%M:%S")

date = as.Date("2015-1-1")
time = format(strptime("1","%H"), format="%H:%M:%S")
paste(date, time, sep = " ")

##################################################################### Clean ubercount_byhour_id

All.Uber=fread("../data/taxi trip/ubercount_byhour_id.csv")
View(All.Uber)
uber = All.Uber[,-c(1,3)]
View(uber)

time.express = "[0-9]+:00:00"
uber.time = unlist(regmatches(uber$groups_byhour, 
                              gregexpr(uber$groups_byhour, pattern = time.express)))
uber.hour = format(strptime(uber.time, "%H:%M:%S"), format="%H:%M:%S")  # let hour be 2 digits


date.express = "[0-9]{4}-[0-9]+-[0-9]+"
uber.date = unlist(regmatches(uber$groups_byhour, 
                              gregexpr(uber$groups_byhour, pattern = date.express)))

uber.date.time = paste(uber.date, uber.hour, sep = " ", collapse = ";")
uber.date.time = unlist(strsplit(uber.date.time, split = ";"))

uber[,2] = uber.date.time
write_csv(uber, path = "../output/ubercount_byhour_id.csv")
