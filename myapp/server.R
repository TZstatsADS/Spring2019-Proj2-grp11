
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
#ubercount_byhour_id<-fread("~/Columbia University/cu_semester2/Applied Data Science/project2/ubercount_byhour_id.csv")
taxi2016count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PickUp2016_Clean.csv")
taxi2015count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PickUp2015_Clean.csv")
taxicount_byhour_id<-bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id[,-2])
#both_byhour_id<-

shinyServer(function(input, output, session) { 
  
  #get input from ui and select the data we need
  #we have three inputs here(car,time,date)
  drawvalue <- reactive({
    
    #match time and date to our data
    
    date.time <- paste(input$date, paste(input$time,":00:00", sep = ""), sep = " ")
    
    if (input$car == '2015'){
      t <- filter(taxi2015count_byhour_id, groups_byhour==date.time)
      return(t)
    }
    
    else if (input$car == '2016'){
      t <- filter(taxi2016count_byhour_id, groups_byhour==date.time)
      return(t)
    }
    
    else{t <- filter(taxicount_byhour_id, groups_byhour==date.time)}
    return(t)
  })
  
  
  
  #draw output by leaflet
  output$map<-renderLeaflet({
    if (input=="2015"){
      map<-leaflet(drawvalue)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,popup=taxi2015count_byhour_id$Count_byhour,
                   weight = 5,radius = ~Count_byhour^(1/4)*200,color = "black",stroke = TRUE,fillOpacity = 0.5)}
    
    else if (input=="2016"){
      map<-leaflet(drawvalue)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,popup = taxi2016count_byhour_id$Count_byhour,
                   weight = 5,radius = ~Count_byhour^(1/4)*200,color = "yellow",stroke = TRUE,fillOpacity = 1)}
    
    else{}
  })
  
  
  
})


