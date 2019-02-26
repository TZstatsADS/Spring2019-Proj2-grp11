setwd("~/Downloads")
library(data.table)
library(tibble)
library(dplyr)
library(bit64)
library(tidyverse)
library(stringr)
library(stringi)

header = c("tpep_pickup_datetime","tpep_dropoff_datetime","trip_distance","pickup_longitude","pickup_latitude",
           "dropoff_longitude","dropoff_latitude","fare_amount","PULocationID","DOLocationID")
#xaa = fread("xaa.csv")
#xaa = xaa[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xaa) = header

#xab = fread("xab.csv")
#xab = xab[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xab) = header

#xac = fread("xac.csv")
#xac = xac[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xac) = header

#xad = fread("xad.csv")
#xad = xad[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xad) = header

#xae = fread("xae.csv")
#xae = xae[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xae) = header

#xaf = fread("xaf.csv")
#xaf = xaf[,c(2,3,5,6,7,10,11,13,20,21)]
#colnames(xaf) = header

#xag = fread("xag.csv")
#xag = xag[,c(2,3,5,6,7,10,11,13,20,21)]
colnames(xag) = header

xah = fread("xah.csv")
xah = xah[,c(2,3,5,6,7,10,11,13,20,21)]
colnames(xah) = header

xai = fread("xai.csv")
xai = xai[,c(2,3,5,6,7,10,11,13,20,21)]
colnames(xai) = header


write_csv(xai, path = "~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/data/xai.csv")

