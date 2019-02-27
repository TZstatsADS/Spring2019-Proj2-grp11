library(tibble)
library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)

setwd("~/Desktop/map 2/output")

ubercount_byhour_id <- fread("ubercount_byhour_id.csv")
load("count2016_1.RData")  # dataframe name: count_1
load("count2016_2.RData")  # dataframe name: count_2
load("count2016_3.RData")  # dataframe name: count_3
taxi2016count_byhour_id<-bind_rows(count_1,count_2,count_3)
taxi2015count_byhour_id<-fread("PU15_Update.csv")
taxicount_byhour_id<-bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id)
both_byhour_id<-bind_rows(taxicount_byhour_id,ubercount_byhour_id)


shinyServer(function(input, output) { 
  
  output$map2<-renderLeaflet({
    
    # Formalize the date and time
    date = as.Date(input$date)
    time = format(strptime(input$time, "%H"), format="%H:%M:%S")
    date_time = paste(date, time, sep = " ")
    
    if (input$car == 'Uber'){
     t <- filter(ubercount_byhour_id, groups_byhour==date_time)
      map2<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
                   color = "grey",stroke = TRUE,fillOpacity = 0.5)
      return (map2)
    }
    
    else if(input$car == 'Taxi'){
       t <- filter(taxicount_byhour_id, groups_byhour==date_time)
       map2<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/8)*100,
                   color = "yellow",stroke = TRUE,fillOpacity = 0.5)
      return (map2)
    }
    
    else {
      t <- filter(both_byhour_id, groups_byhour==date_time)
      map2<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
                   color = "blue",stroke = TRUE,fillOpacity = 0.5)
      return (map2)
      
      }


         })

  })
