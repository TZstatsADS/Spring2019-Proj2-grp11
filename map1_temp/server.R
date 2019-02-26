library(shiny)
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


load('taxi9example.RData')
setwd("../output")
load('nyc_nbhd.RData')
# load('taxidata.RData')
# load('FPD_map.RData')
# load('Sunny_FPD_map.RData')
# load('Bad_FPD_map.RData')
# taxi<-taxi%>%na.omit()

PUfpd <- na.omit(PUfpd %>% group_by(PUnbhd,pickup_hour,pu_bad_weather) %>%
                   mutate(FPD=sum(totalfare)/sum(totaldist)))

add_level <- function(df){
  A <- df %>% mutate(FPD_level = ifelse(FPD>=0 & FPD < 1, 0,
                                        ifelse(FPD>=1 & FPD < 2,1,
                                               ifelse(FPD>=2 & FPD < 3,2,
                                                      ifelse(FPD>=3 & FPD < 4,3,
                                                             ifelse(FPD>=4 & FPD < 5,4,
                                                                    ifelse(FPD>=5 & FPD < 6,5,
                                                                           ifelse(FPD>=6 & FPD < 7,6,7))))))))
  return(A)
}

PUfpd <- add_level(PUfpd)

shinyServer(function(input, output) {
  
  
  
  output$map1 <- renderLeaflet({
    
    if (input$weather == "All Days"){
      timegpdata<- PUcount
      timeFPD<- PUfpd %>% filter(as.integer(pickup_hour)==input$hour)
    }
    else if(input$weather == "Sunny Days"){
      timegpdata<- PUcount %>% filter(pu_bad_weather==0)
      timeFPD<- PUfpd %>% filter(pu_bad_weather==0) %>% filter(as.integer(pickup_hour)==input$hour)
    }
    else{
      timegpdata<- PUcount %>% filter(pu_bad_weather==1)
      timeFPD<- PUfpd %>% filter(pu_bad_weather==1) %>% filter(as.integer(pickup_hour)==input$hour)
    }

    
    mmmmmmmm<-timegpdata%>%filter(as.integer(pickup_hour)==input$hour)
    map_data <- geo_join(nyc_nbhd, mmmmmmmm, "neighborhood", "PUnbhd")
    
    
    pal2 <- colorBin(color[[1]], bins=c(0,100,1000,2000,4000,5000,10000))
    #pal2 <- colorNumeric(palette = "RdBu",domain = range(map_data@data$n, na.rm=T))
    popup1 = paste0('<strong>Neighborhood: </strong><br>', map_data@data$neighborhood, 
                    '<br><strong>Count of pick-ups: </strong><br>', map_data@data$n)
    
    
    
    FPD_map_data <- geo_join(nyc_nbhd, timeFPD, "neighborhood", "PUnbhd")
   
    
    
    #pal3 <- colorNumeric(palette = "Blues",domain = range(FPD_map@data$FPD_level, na.rm=T))
    pal3 <- colorBin(color[[2]], bins=c(0,1,2,3,4,5,6,7))

    
    popup2 = paste0('<strong>Neighborhood: </strong><br>', FPD_map_data@data$neighborhood, 
                    '<br><strong>Fair Per Distance: </strong><br>', FPD_map_data@data$FPD)
    
    pic1 <- leaflet(map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    picFPD <- leaflet(FPD_map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    if (input$CF == "count"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal2(n), color = 'grey', weight = 1,
                    popup = popup1,fillOpacity = .6) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }

    else if (input$CF == "FPD"){
      picFPD<-picFPD %>%
        addPolygons(fillColor = ~pal3(FPD_level), color = 'grey', weight = 1,
                    popup = popup2,fillOpacity = .6) %>%
        addLegend(position = 'bottomright',
                  colors = color[[2]],
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    
    
    

    
  })
  
  
})