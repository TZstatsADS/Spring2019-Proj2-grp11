
########## edit version

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
### Uber
urlfile<-'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/master/data/test_uber.csv?token=AsziG_KERfkKFblH2rcJSOWCdxheu7CNks5ceU4VwA%3D%3D'
ubercount_byhour_id<-read.csv(urlfile)[,-c(1,2)]

### Taxi
#urlfile <- 'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/Xinyi-Hu-Taylor/output/PickUp2015_Clean.csv?token=Aszf_0Mq2SiQptdygH8slOD3Sq1h6DK3ks5ceW8TwA%3D%3D'
#taxi2015count_byhour_id <- read.csv(urlfile)
#urlfile <-'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/Xinyi-Hu-Taylor/output/PickUp2016_Clean.csv?token=Aszf_6MS3W-I4Xuwfv2l2F6v0qgighA3ks5ceW96wA%3D%3D'
#taxi2016count_byhour_id <- read.csv(urlfile)

ubercount_byhour_id <- fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/testUber_Update.csv")
taxi2016count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PU16_Update.csv")
taxi2015count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PU15_Update.csv")
taxicount_byhour_id<-bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id)
both_byhour_id<-bind_rows(taxicount_byhour_id,ubercount_byhour_id)

View(taxicount_byhour_id)
View(ubercount_byhour_id)

shinyServer(function(input, output) { 
  
  #observe({
      
  output$map<-renderLeaflet({
    
    date = as.Date(input$date)
    time = format(strptime(input$time, "%H"), format="%H:%M:%S")
    date_time = paste(date, time, sep = " ")
    
    if (input$car == 'Uber'){
     t <- filter(ubercount_byhour_id, groups_byhour==date_time)
      map<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
                   color = "black",stroke = TRUE,fillOpacity = 0.5)
      return (map)
    }
    
    else if(input$car == 'Taxi'){
       t <- filter(taxicount_byhour_id, groups_byhour==date_time)
       map<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
                   color = "yellow",stroke = TRUE,fillOpacity = 0.5)
      return (map)
    }
    
    else {
      t <- filter(both_byhour_id, groups_byhour==date_time)
      map<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
                   color = "blue",stroke = TRUE,fillOpacity = 0.5)
      return (map)
      
      }


         })

  })
