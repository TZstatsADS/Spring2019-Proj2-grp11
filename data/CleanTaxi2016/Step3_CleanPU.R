setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output")
library(data.table)
library(bit64)
library(tibble)
library(dplyr)
library(tidyverse)

map.e = fread("Map_e.csv")
map.f = fread("Map_f.csv")
map.g = fread("Map_g.csv")
taxigeo = fread("TaxiGeo.csv")
geo = taxigeo[,c(1,5,6)]

count.date.geo = function(map = map.e){
  
  count = map %>%
    group_by(`groups_byhour`,`LocationID`) %>%
    summarise(
      value = n()
    )
  count = count[order(-count$value),]
  count = inner_join(count, geo, by = "LocationID")
  
  #count = inner_join(join.geo, map[,c(1,2)], by = "LocationID")
  count = as.data.frame(count)
  count = count[,c(3,1,5,4)]
  colnames(count) = c("Count_byhour","groups_byhour","mean_lat","mean_long")
  
  #### Counting numbers are way too larger than uber data counting numbers, need to shrink them
  #count$Count_byhour = count$Count_byhour/100

  return(count)

}

count_1 = count.date.geo(map = map.e)
count_2 = count.date.geo(map = map.f)
count_3 = count.date.geo(map = map.g)

load("count2016_1.RData")  # dataframe name: count_1
load("count2016_2.RData")  # dataframe name: count_2
load("count2016_3.RData")  # dataframe name: count_3

PU16_Update = bind_rows(count_1, count_2, count_3)

save(PU16_Update, file = '../output/PU16_Update.RData')
load("PU16_Update.RData")  # dataframe name: count_3

head(count_1)
head(PU16_Update)

