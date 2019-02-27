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
label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6-7","7+"))
title = list(t1 = "Pick Up Numbers", t2 = "Fair Per Distance")


load('PU_data.RData')
load('DO_data.RData')
setwd("../output")
load('nyc_nbhd.RData')

shinyServer(function(input, output,session) {
  
  
  
  output$map1 <- renderLeaflet({
    
    if(input$dropoff == TRUE){
      if (input$weather == "All Days"){
        timegpdata <- Allcount2 
        timeFPD<- AllFPD2 %>% filter(as.integer(dropoff_hour)==input$hour)
      }
      else if(input$weather == "Sunny Days"){
        timegpdata<- Sunnycount2 
        timeFPD<- SunnyFPD2 %>% filter(as.integer(dropoff_hour)==input$hour)
      }
      else{
        timegpdata<- Badcount2 
        timeFPD<- BadFPD2 %>% filter(as.integer(dropoff_hour)==input$hour)
      }
      mmmmmmmm<-timegpdata%>%filter(as.integer(dropoff_hour)==input$hour)
      map_data <- geo_join(nyc_nbhd, timegpdata, "neighborhood", "DOnbhd")
      FPD_map_data <- geo_join(nyc_nbhd, timeFPD, "neighborhood", "DOnbhd")
    }
    else{
      if (input$weather == "All Days"){
        timegpdata <- Allcount
        timeFPD<- AllFPD %>% filter(as.integer(pickup_hour)==input$hour)
      }
      else if(input$weather == "Sunny Days"){
        timegpdata<- Sunnycount 
        timeFPD<- SunnyFPD %>% filter(as.integer(pickup_hour)==input$hour)
      }
      else{
        timegpdata<- Badcount
        timeFPD<- BadFPD %>% filter(as.integer(pickup_hour)==input$hour)
      }
      mmmmmmmm<-timegpdata%>%filter(as.integer(pickup_hour)==input$hour)
      map_data <- geo_join(nyc_nbhd, timegpdata, "neighborhood", "PUnbhd")
      FPD_map_data <- geo_join(nyc_nbhd, timeFPD, "neighborhood", "PUnbhd")
    }
    
    
    
    
    
    pal2 <- colorBin(color[[1]], bins=c(0,100,1000,10000,100000,1000000,10000000))
    popup1 = paste0('<strong>Neighborhood: </strong><br>', map_data@data$neighborhood, 
                    '<br><strong>Count: </strong><br>', map_data@data$totalcount)
    
    
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
        addPolygons(fillColor = ~pal2(totalcount), color = 'grey', weight = 1,
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
  
  observeEvent(input$map1_shape_click, {
    click <- input$map1_shape_click
    posi <<- reactive({input$map1_shape_click})
  }
  )
  
  observeEvent(input$details,{
    if(input$details){
      updateTabsetPanel(session, "map1", selected = "panel4")
      
      dattest = data.frame(Longitude = posi()$lng, Latitude = posi()$lat)
      coordinates(dattest) <- ~ Longitude + Latitude
      proj4string(dattest) <- proj4string(nyc_nbhd)
      match1<-over(dattest, nyc_nbhd)
      match1 <- match1[,c(1,3)]
      colnames(match1) <- c('nbhd','boro')
    }
    
    output$plot1 <- renderPlot({
      if (nrow(match1) == 0) {
        return(NULL)
      }
      
      dfcount_resultSun <- Sunnycount2[Sunnycount2$DOnbhd==match1$nbhd,]
      dfcount_resultSun$FPD <- unlist(SunnyFPD2[SunnyFPD2$DOnbhd==match1$nbhd,]$FPD)
      ggplot(data=dfcount_resultSun, aes(x=as.numeric(dropoff_hour), y=totalcount/sum(totalcount),fill=-FPD)) +
        geom_bar(stat="identity",alpha=0.7)+
        geom_smooth(col="#F28123") + xlab("Hours") + ylab("Total DropOff Count")+
        ggtitle("Drop Off Flow Trend in Sunny Days")+ 
        ylim(0,0.1)
      # +
      #   scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) 
      
    })
    
    output$plot2 <- renderPlot({
      if (nrow(match1) == 0) {
        return(NULL)
      }
      
      dfcount_resultBad <- Badcount2[Badcount2$DOnbhd==match1$nbhd,]
      dfcount_resultBad$FPD <- unlist(BadFPD2[BadFPD2$DOnbhd==match1$nbhd,]$FPD)
      ggplot(data=dfcount_resultBad, aes(x=as.numeric(dropoff_hour), y=totalcount/sum(totalcount),fill=-FPD)) +
        geom_bar(stat="identity",alpha=0.7)+
        geom_smooth(col="#F28123") + xlab("Hours") + ylab("Total DropOff Count")+
        ggtitle("Drop Off Flow Trend in Bad Weather Days")+ 
        ylim(0,0.1)
      # +
      #   scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) 
      
    })

 })


})