setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/data")
library(data.table)
library(tibble)
library(dplyr)
library(tidyverse)
library("ggmap")
mykey = "AIzaSyDj6W8F2IcxkcEnW-aKGmcJovBpMAhIAt4"
register_google(key = mykey)

taxi_zone = fread("../data/taxi trip/taxi_zone_lookup.csv")
View(taxi_zone)

taxigeo =taxi_zone %>%
  mutate(Add = paste(`Zone`, "New York, NY", sep = ","))%>%
  mutate_geocode(Add)
View(taxigeo)

colnames(taxigeo)[5] = "mean_long"
colnames(taxigeo)[6] = "mean_lat"

write_csv(taxigeo, path = "../output/TaxiGeo.csv")
