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


setwd("/Users/yeyejiang/Documents/GitHub/Spring2019-Proj2-grp11/output")
load('nyc_nbhd.RData')
load('taxidata.RData')

shinyServer(function(input, output) {
  
  output$map2 <- renderLeaflet({
    leaflet(nyc_nbhd) %>%
      addTiles() %>% 
      addPolygons(popup = ~neighborhood,weight = 0.5,color = 'grey') %>%
      addProviderTiles("CartoDB.Positron")
  })
})