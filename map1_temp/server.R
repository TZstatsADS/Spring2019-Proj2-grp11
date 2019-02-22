library(shiny)
library(plyr)
library(ggplot2)
library(shiny)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(readr)




#color = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
#             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
#             color3 = c("#F7FCF5","#74C476", "#005A32"))
#bin = list(bin1 = c(0,100,1000,10000,100000,1000000,10000000), bin2 = c(0,1,2,3,4,5,6,7))
#bin = list(bin1 = c(0,10,100,500,1000,100000,1000000), bin2 = c(0,1,2,3,4,5,6,7))
#pal = colorBin(color[[1]], bins = bin[[1]])




setwd("/Users/yeyejiang/Documents/GitHub/Spring2019-Proj2-grp11/output")
load('nyc_nbhd.RData')
load('taxidata.RData')
taxi<-taxidata%>%na.omit()


shinyServer(function(input, output) {
  
  output$map1 <- renderLeaflet({
    
    timegpdata<- taxi%>%group_by(PU.hour,PUnbhd)%>%tally()
    mmmmmmmm<-timegpdata%>%filter(PU.hour==input$hour)
    map_data <- geo_join(nyc_nbhd, mmmmmmmm, "neighborhood", "PUnbhd")
    
    
    pal2 <- colorNumeric(palette = "RdBu",domain = range(map_data@data$n, na.rm=T))
    
    leaflet(map_data) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal2(n), popup = ~neighborhood,weight=1) %>% 
      #addMarkers(~lng, ~lat, popup = ~neighborhood, data = points) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.98, 40.75, zoom = 12)
    

    
  })
})