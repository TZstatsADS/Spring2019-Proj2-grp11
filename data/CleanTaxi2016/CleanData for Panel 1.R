setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output")
library(data.table)
library(bit64)
library(tibble)
library(sp)
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


load('~/Desktop/GR5243 Applied Data Science/Project 2/for map2/taxi2016_1.RData')
load('nyc_nbhd.RData')
weather<-read.csv('weather_small.csv',as.is=TRUE)

############### Start function ###############

nbhd<-function(ll){
  colnames(ll)=c('lon','lat')
  ll_spdf<-ll
  coordinates(ll_spdf)= ~lon + lat
  proj4string(ll_spdf) <- proj4string(nyc_nbhd)
  match<-over(ll_spdf, nyc_nbhd)
  return(match[,c(1,3)])
}

clean.panel.1 = function(dataset = taxi2016_1){

  a_raw<-dim(dataset)

  filtaxi <- dataset%>%
    mutate(fpd=fare_amount/trip_distance)%>%
    filter(trip_distance<80 & trip_distance>0)%>%
    filter(fare_amount<200 & fare_amount>0)%>%
    filter(fpd<10 & fpd>0.5)
  b_filter<-dim(filtaxi)

  punbhd<-filtaxi%>%
    select(pickup_longitude,pickup_latitude)%>%
   nbhd()
  colnames(punbhd)<-c('PUnbhd','PUnbhdBorough')


  donbhd<-filtaxi%>%
   select(pickup_longitude,pickup_latitude)%>%
   nbhd()
  colnames(donbhd)<-c('DOnbhd','DOnbhdBorough')

  
  taxitemp<-cbind(filtaxi,punbhd,donbhd)
  taxitemp1<- taxitemp%>%na.omit()
  c_nbhd_naomit<-dim(taxitemp1)
  
  taxitemp2<-taxitemp1%>%mutate(pu_wl=if_else(PUnbhdBorough %in% c('Manhattan', 'Bronx', 'Unknown')
                                            , 'NY CITY CENTRAL PARK, NY US',
                                            if_else(PUnbhdBorough %in% c('Brooklyn','Queens')
                                                    , 'LA GUARDIA AIRPORT, NY US'
                                                    ,'WOODBRIDGE TWP 1.1 ESE, NJ US')))
  taxitemp3<-taxitemp2%>%mutate(do_wl=if_else(DOnbhdBorough %in% c('Manhattan', 'Bronx', 'Unknown')
                                            , 'NY CITY CENTRAL PARK, NY US',
                                            if_else(DOnbhdBorough %in% c('Brooklyn','Queens')
                                                    , 'LA GUARDIA AIRPORT, NY US'
                                                    ,'WOODBRIDGE TWP 1.1 ESE, NJ US')))

  pu_weather<-taxitemp3%>%left_join(weather,by=c('pickup_date'='DATE','pu_wl'='NAME'))%>%select(BAD_WEATHER)
  colnames(pu_weather)<-'pu_bad_weather'
  do_weather<-taxitemp3%>%left_join(weather,by=c('dropoff_date'='DATE','do_wl'='NAME'))%>%select(BAD_WEATHER)
  colnames(do_weather)<-'do_bad_weather'
  taxitemp4<-cbind(taxitemp1,pu_weather,do_weather)
  d_final<-dim(taxitemp4%>%na.omit())


  PU<- taxitemp4%>%
    group_by(pickup_date, pickup_hour, PUnbhd, PUnbhdBorough, pu_bad_weather)%>%
    summarize(totaldist=sum(trip_distance),
              totalfare=sum(fare_amount),
              count=n())%>%
    select(pickup_date, pickup_hour, PUnbhd, PUnbhdBorough, pu_bad_weather, totaldist, totalfare, count)

  DO<- taxitemp4%>%
    group_by(dropoff_date, dropoff_hour, DOnbhd, DOnbhdBorough, do_bad_weather)%>%
    summarize(totaldist=sum(trip_distance),
              totalfare=sum(fare_amount),
              count=n())%>%
    select(dropoff_date, dropoff_hour, DOnbhd, DOnbhdBorough, do_bad_weather, totaldist, totalfare, count)


  dim<-rbind(a_raw,b_filter,c_nbhd_naomit,d_final)
  
  PU_9 = PU #
  DO_9 = DO #
  dim_9 = dim #
  return(save(PU_9, #
              DO_9, #
              dim_9, #
              file = "test_taxi9example.RData")) #
}   
############### End function ###############

clean.panel.1(dataset = taxi2016_9)

load("test_taxi1example.RData")
load("test_taxi2example.RData")
load("test_taxi3example.RData")
load("test_taxi4example.RData")
load("test_taxi5example.RData")
load("test_taxi6example.RData")
load("test_taxi7example.RData")
load("test_taxi8example.RData")
load("test_taxi9example.RData")

PU =bind_rows(PU_1,PU_2,PU_3,PU_4,PU_5,PU_6,PU_7,PU_8,PU_9)
DO = bind_rows(DO_1,DO_2,DO_3,DO_4,DO_5,DO_6,DO_7,DO_8,DO_9)

#save(PU,DO, file = "taxi2016_FinalClean.RData")
dim(PU)
dim(DO)
dim = bind_cols(dim_1,dim_2,dim_3,dim_4,dim_5,dim_6,dim_7,dim_8,dim_9)
dim_1 = as.data.frame(dim_1)
dim_2 = as.data.frame(dim_2)
dim_3 = as.data.frame(dim_3)
dim_4 = as.data.frame(dim_4)
dim_5 = as.data.frame(dim_5)
dim_6 = as.data.frame(dim_6)
dim_7 = as.data.frame(dim_7)
dim_8 = as.data.frame(dim_8)
dim_9 = as.data.frame(dim_9)
colnames(dim_1) = c("V1.1","V1.2")
colnames(dim_2) = c("V2.1","V2.2")
colnames(dim_3) = c("V3.1","V3.2")
colnames(dim_4) = c("V4.1","V4.2")
colnames(dim_5) = c("V5.1","V5.2")
colnames(dim_6) = c("V6.1","V6.2")
colnames(dim_7) = c("V7.1","V7.2")
colnames(dim_8) = c("V8.1","V8.2")
colnames(dim_9) = c("V9.1","V9.2")
dim
save(dim, file = "taxi2016_FinalClean_dim.RData")
