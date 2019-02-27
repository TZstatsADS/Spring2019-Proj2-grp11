library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)

taxi2016count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PickUp2016_Clean.csv")
taxi2015count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PickUp2015_Clean.csv")
taxicount_byhour_id<-as.data.frame(bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id[,-2]))

t <- filter(taxicount_byhour_id, groups_byhour=="2015-1-8 20:00:00")

leaflet(t)%>% addTiles() %>%
    setView(-73.86, 40.72, zoom=10)%>%
    addProviderTiles("Stamen.Watercolor") %>%
    addCircles(lng=~mean_long,lat=~mean_lat ,weight = 5,radius = ~Count_byhour^(1/4)*200,
               color = "yellow",stroke = TRUE,fillOpacity = 1)
