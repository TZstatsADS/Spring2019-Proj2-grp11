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




color = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
            color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'))
#bin = list(bin1 = c(0,100,1000,10000,100000,1000000,10000000), bin2 = c(0,1,2,3,4,5,6,7))
#bin = list(bin1 = c(0,10,100,500,1000,100000,1000000), bin2 = c(0,1,2,3,4,5,6,7))
#pal = colorBin(color[[1]], bins = bin[[1]])
label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6-7","7+"))
title = list(t1 = "Pick Up Numbers", t2 = "Fair Per Distance")



setwd("../output")
load('nyc_nbhd.RData')
load('taxidata.RData')
taxi<-taxi%>%na.omit()


shinyServer(function(input, output) {
  
  output$map1 <- renderLeaflet({
    
    timegpdata<- taxi%>%group_by(PU.hour,PUnbhd)%>%tally()
    mmmmmmmm<-timegpdata%>%filter(PU.hour==input$hour)
    map_data <- geo_join(nyc_nbhd, mmmmmmmm, "neighborhood", "PUnbhd")
    
    
    pal2 <- colorNumeric(palette = "RdBu",domain = range(map_data@data$n, na.rm=T))
    popup1 = paste0('<strong>Neighborhood: </strong><br>', map_data@data$neighborhood, 
                    '<br><strong>Count of pick-ups: </strong><br>', map_data@data$n)
    
    
    # leaflet(map_data) %>%
    #   addTiles() %>% 
    #   addPolygons(fillColor = ~pal2(n), popup = ~neighborhood,weight=1) %>% 
    #   #addMarkers(~lng, ~lat, popup = ~neighborhood, data = points) %>%
    #   addProviderTiles("CartoDB.Positron") %>%
    #   setView(-73.98, 40.75, zoom = 12)
    
    FPD_result <- taxi %>% group_by(FPD,FPD_level, PUnbhd)%>% tally()
    
    names(FPD_result) <- c("FPD", "FPD_level", "PUnbhd","total")
    
    FPD_map_data <-  geo_join(nyc_nbhd, FPD_result, "neighborhood", "PUnbhd")
    
    #pal3 <- colorQuantile("Blues", FPD_map_data@data$FPD_level, n = 8)
    pal3 <- colorNumeric(palette = "Blues",domain = range(FPD_map_data@data$FPD, na.rm=T),reverse = T)
    #pal3 <- colorBin(color[[2]], bins=c(0,1,2,3,4,5,6,7),reverse = T)
    #pal3 <- colorBin("Blues",FPD_map_data@data$FPD_level,8,pretty = F)
    
    popup2 = paste0('<strong>Neighborhood: </strong><br>', FPD_map_data@data$neighborhood, 
                    '<br><strong>Fair Per Distance: </strong><br>', FPD_map_data@data$FPD)
    
    pic1 <- leaflet(map_data) %>%
      setView(-73.98, 40.75, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron")
    
    picFPD <- leaflet(FPD_map_data) %>%
      setView(-73.98, 40.75, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron")
    
    if (input$CF == "count"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal2(n), color = 'grey', weight = 1,
                    popup = popup1) #%>%
        # addLegend(position = "bottomright",
        #           colors = color[[1]],
        #           labels = label[[1]],
        #           opacity = 0.6,
        #           title = title[[1]])
    }

    else if (input$CF == "FPD"){
      picFPD<-picFPD %>%
        addPolygons(fillColor = ~pal3(FPD), color = 'grey', weight = 1,
                    popup = popup2) #%>%
        # addLegend(position = 'bottomright',
        #           colors = color[[2]],
        #           labels = label[[2]], ## legend labels (only min and max)
        #           opacity = 0.6,      ##transparency again
        #           title = title[[2]])
    }
    
    
  })
})