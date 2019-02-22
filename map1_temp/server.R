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


setwd("../")
load('output/shape.RData')

shinyServer(function(input, output) {
  
  output$map2 <- renderLeaflet({
    leaflet(nyc_neighborhoods) %>%
      addTiles() %>% 
      addPolygons(popup = ~neighborhood,weight = 0.5,color = 'grey') %>%
      addProviderTiles("CartoDB.Positron")
  })
})