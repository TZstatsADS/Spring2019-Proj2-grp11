packages.used=c("rgeos", "sp", "rgdal", 
                "leaflet", "htmlwidgets", "shiny",
                "ggplot2", "dplyr", "data.table","DT")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)


#read the uber data
#urlfile<-'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/master/data/test_uber.csv?token=AsziG_KERfkKFblH2rcJSOWCdxheu7CNks5ceU4VwA%3D%3D'
#ubercount_byhour_id<-read.csv(urlfile)
#taxicount_byhour_id<-...
#both_byhour_id<-...



shinyServer(function(input, output) { 
  
  #get input from ui and select the data we need
  #we have three inputs here(car,time,date)
  drawvalue <- reactive({
      
      #match time and date to our data
      #date_time<-...input$time,input$date
      
      if (input$car == 'Uber'){
        t <- filter(ubercount_byhour_id, groups_byhour=date_time)
        return(t)
      }
    #  elif(input$car == 'Taxi'){
    #    t <- filter(taxicount_byhour_id, groups_byhour=date_time)
    #    return(t)
    #  }
    #  else{t <- filter(both_byhour_id, groups_byhour=date_time)}
    #    return(t)
    })
  
  

  #draw output by leaflet
  output$map<-renderLeaflet({
     #if (input=="Uber"){
        #map<-leaflet(drawvalue)%>% addTiles() %>%
        leaflet(ubercount_byhour_id[1:10,])%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,color = "black",stroke = TRUE,fillOpacity = 0.5)
        #}
     #elif(input=="Taxi"){
     #   map<-leaflet(drawvalue)%>% addTiles() %>%
     #   setView(-73.86, 40.72, zoom=10)%>%
     #   addProviderTiles("Stamen.Watercolor") %>%
     #   addCircles(lng=~mean_long,lat=~mean_lat ,weight = 5,radius = ~Count_byhour^(1/4)*200,color = "yellow",stroke = TRUE,fillOpacity = 1)
     #   return (map)}
     #else{
     # map<-leaflet(drawvalue)%>% addTiles() %>%
     #    setView(-73.86, 40.72, zoom=10)%>%
     #   addProviderTiles("Stamen.Watercolor") %>%
     #    addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,color = "yellow",stroke = TRUE,fillOpacity = 1)
     #   return (map)}
     })
  
  

  })


