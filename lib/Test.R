
library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)


setwd("/Users/nelsonlin/Documents/GitHub/Spring2019-Proj2-grp11")

load('output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))


# load airport traffic data
load('output/RSummarized/JFK_count_seperated.RData')
load('output/RSummarized/JFK_FPD_seperated.RData')
load('output/RSummarized/NWK_count_seperated.RData')
load('output/RSummarized/NWK_FPD_seperated.RData')
load('output/RSummarized/LGA_count_seperated.RData')
load('output/RSummarized/LGA_FPD_seperated.RData')

# load interactive data
load('output/count_seperated.RData')
load('output/FPD_seperated.RData')

# names of each borough. subdat is transformed shape1 file
rownames(count_result) = subdat@data$NTACode

count_result1 <- JFK_count_result
FPD_result1 <- JFK_count_result

# subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
# subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

#create a data fram for longitude and latitude of click location
 
dattest = data.frame(Longitude = -73.83499, Latitude = 40.88029)
coordinates(dattest) <- ~ Longitude + Latitude
proj4string(dattest) <- CRS("+proj=longlat")
dattest <- spTransform(dattest, proj4string(myShape1))

#rtest is to see which borough click falls in
rtest = over(dattest, myShape1)
count_resultNTA = count_result1[which(rownames(count_result) == rtest$NTACode),,]
print(count_resultNTA)
count_resultNTA = apply(count_resultNTA, 1, sum)
print(count_resultNTA)
index <- c(0:23)
dfcount_resultNTA <- data.frame(index, count_resultNTA)
ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
  labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)

