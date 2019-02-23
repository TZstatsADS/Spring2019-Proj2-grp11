
########## edit version

packages.used=c("tibble","rgeos", "sp", "rgdal", 
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


################ Read Uber data
urlfile<-'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/Xinyi-Hu-Taylor/output/ubercount_byhour_id.csv?token=Aszf_-jlp1f0_087w7JKP7v8ir3xkvQWks5ceqqKwA%3D%3D'
ubercount_byhour_id<-read_csv(urlfile)

################ Read taxi data
urlfile <- 'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/Xinyi-Hu-Taylor/output/PU15_Update.csv?token=Aszf_8cArM9UTIC_oLz7o_m0_UY1UdEIks5ceqolwA%3D%3D'
taxi2015count_byhour_id <- read_csv(urlfile)
urlfile <-'https://raw.githubusercontent.com/TZstatsADS/Spring2019-Proj2-grp11/Xinyi-Hu-Taylor/output/PU16_Update.csv?token=Aszf_1MTcNIIYyKFoFvVa1eII4egrSQxks5ceqpTwA%3D%3D'
taxi2016count_byhour_id <- read_csv(urlfile)

################ Read Uber data
#ubercount_byhour_id <- fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/ubercount_byhour_id.csv")
################ Read taxi data
#taxi2016count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PU16_Update.csv")
#taxi2015count_byhour_id<-fread("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/output/PU15_Update.csv")
#taxicount_byhour_id<-bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id)
#both_byhour_id<-bind_rows(taxicount_byhour_id,ubercount_byhour_id)

#View(taxicount_byhour_id)
#View(ubercount_byhour_id)

shinyServer(function(input, output) { 
  
  output$map<-renderLeaflet({
    
    # Formalize the date and time
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
