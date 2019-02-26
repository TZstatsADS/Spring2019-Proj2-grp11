setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output")
library(data.table)
library(bit64)
library(tibble)
library(dplyr)
library(tidyverse)
#xaa = fread("xaa.csv")
#xab = fread("xab.csv")
#xac = fread("xac.csv")
#xad = fread("xad.csv")
#xae = fread("xae.csv")
#xaf = fread("xaf.csv")
#xag = fread("xag.csv")
#xah = fread("xah.csv")
#xai = fread("xai.csv")

date.time.format = function(xaa = data.frame()){
  
  pu.DateTime = xaa$tpep_pickup_datetime
  do.DateTime = xaa$tpep_dropoff_datetime
  
  xaa[,1] = format(strptime(pu.DateTime, format='%Y-%m-%d %H:%M:%S'),
                   format="%Y-%m-%d")
  xaa[,2] = format(strptime(do.DateTime, format='%Y-%m-%d %H:%M:%S'),
                   format="%Y-%m-%d")
  xaa$pickup_hour = format(strptime(pu.DateTime, format='%Y-%m-%d %H:%M:%S'),
                           format="%H")
  xaa$dropoff_hour = format(strptime(do.DateTime, format='%Y-%m-%d %H:%M:%S'),
                            format="%H")
  
  colnames(xaa) = c("pickup_date", "dropoff_date","trip_distance",
                    "pickup_longitude","pickup_latitude","dropoff_longitude",
                    "dropoff_latitude","fare_amount","pickup_hour","dropoff_hour")
  return(xaa)
  
}

#taxi2016_1 = date.time.format(xaa = xaa) #
#taxi2016_2 = date.time.format(xaa = xab) #
#taxi2016_3 = date.time.format(xaa = xac) #
#taxi2016_4 = date.time.format(xaa = xad) #
#taxi2016_5 = date.time.format(xaa = xae) # no memory 
#taxi2016_6 = date.time.format(xaa = xaf)
#taxi2016_7 = date.time.format(xaa = xag)
#taxi2016_8 = date.time.format(xaa = xah)
#taxi2016_9 = date.time.format(xaa = xai) #


#save(taxi2016_1, file = '../output/taxi2016_1.RData') #
#save(taxi2016_2, file = '../output/taxi2016_2.RData') #
#save(taxi2016_3, file = '../output/taxi2016_3.RData') #
#save(taxi2016_4, file = '../output/taxi2016_4.RData') #
#save(taxi2016_5, file = '../output/taxi2016_5.RData') # no memory 
#save(taxi2016_6, file = '../output/taxi2016_6.RData')
#save(taxi2016_7, file = '../output/taxi2016_7.RData')
#save(taxi2016_8, file = '../output/taxi2016_8.RData')
#save(taxi2016_9, file = '../output/taxi2016_9.RData') #

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

