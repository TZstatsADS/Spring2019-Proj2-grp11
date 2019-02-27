packages.used=c("rgeos", "sp", "rgdal", 
                "leaflet", "htmlwidgets", "shiny",
                "ggplot2", "dplyr", "data.table","DT", "tidyverse")

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
library(readr)


#setwd("Documents/GitHub/Spring2019-Proj2-grp11/")
#setwd("../")

# set group based on radio selection
group1 = "<span style='color: #7f0000; font-size: 11pt'><strong>count</strong></span>"
group2 = "<span style='color: #7f0000; font-size: 11pt'><strong>FPD</strong></span>"
group3 = "<span style='color: #7f0000; font-size: 11pt'><strong>Percentage Cash Paying:</strong></span>"
# set color scheme
color = list(color1 = c('#F2D7D5', '#CD6155', '#C0392B','#641E16'),
             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
             color3 = c("#F7FCF5","#74C476", "#005A32"))
# set bin sizes and make legend in the bottom right
bin = list(bin1 = c(0,100,1000,10000,100000), bin2 = c(0,1,2,3,4,5))
label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6","7+"),
             label3 = c("<0.4","0.4~0.6",">0.6"))
title = list(t1 = "Pick Up Numbers", t2 = "Fair Per Distance",t3  = "PercentagePayingCash")

# load shape data 
load('output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))

# load dynamic data
dynamicdata = fread("data/pickupDropoff date_hour.csv", header = TRUE, stringsAsFactors=F)

# load airport traffic data
load('output/RSummarized/JFK_count_seperated.RData')
load('output/RSummarized/JFK_FPD_seperated.RData')
load('output/RSummarized/NWK_count_seperated.RData')
load('output/RSummarized/NWK_FPD_seperated.RData')
load('output/RSummarized/LGA_count_seperated.RData')
load('output/RSummarized/LGA_FPD_seperated.RData')

# load interactive data
load('output/count_seperated.RData')
load('output/FPD_seperated.RData')

# names of each borough. subdat is transformed shape1 file
rownames(count_result) = subdat@data$NTACode

# summarized data 
payper = read.csv("data/Data_frame_of_summary.csv")

#head(subdat@data)
shinyServer(function(input, output,session) { 
  
    # THIS IS LEAFLET FOR DYNAMIC MAP
    output$map2 <- renderLeaflet({
      leaflet() %>%
        setView(lat=40.7128, lng=-74.0059, zoom=11) %>%
        addProviderTiles('CartoDB.Positron') 
    })
    
    drawvalue <- reactive({
      if (input$pd == 'pick up'){
        t <- filter(dynamicdata, pickup_hour == input$hours, pickup_date == "1/1/2015")
        return(t)
      }
      else{
        t <- filter(dynamicdata, dropoff_hour == input$hours, dropoff_date == "1/1/2015")
        return(t)
      }
    })
  
    observe({
      radius <-  100
    })
    
    
    #OUTPUT CALLED MAP
    output$map <- renderLeaflet({
      
      if (input$airport == "JFK"){
        count_result1 <- JFK_count_result
        FPD_result1 <- JFK_FPD_result
      }
      else if  (input$airport == "NWK"){
        count_result1 <- NWK_count_result
        FPD_result1 <- NWK_FPD_result
      }
      else{
        count_result1 <- LGA_count_result
        FPD_result1 <- LGA_FPD_result
      }
        
      
      # THESE ARE BACK END FOR DATA TO LOAD ON INTERACTIVE MAP
      if (input$days == "All day"){
        count_intermediate = count_result1 %>% apply(c(1,2), sum)
        FPD_intermediate = FPD_result1 %>% apply(c(1,2), mean, na.rm = T)
      }else{
        count_intermediate = count_result1[ , , (input$days == "Not Business Day") + 1]
        FPD_intermediate = FPD_result1[ , , (input$days == "Not Business Day") + 1]
      }
      
      # THIS IS BACK END FOR HOURLY INFORMATION MAP
      if (!input$showhr){
        subdat@data$count = count_intermediate %>% apply(1, sum)
        subdat@data$FPD = FPD_intermediate %>% apply(1, mean, na.rm = T)
      }else{
        subdat@data$count = count_intermediate[, input$hr_adjust+1]
        subdat@data$FPD = FPD_intermediate[, input$hr_adjust+1]
      }
      
      ######
      
      blocks_coord = data.frame(center_lng = rep(NA, 195), center_lat = rep(NA, 195)) # Combine borough coord for future marking purpose
      for (i in 1:195){ blocks_coord[i,] = subdat@polygons[[i]]@labpt }    # One more update: add long/lat permanently into myShape@data as
      
      
      #subdat@data$count = count_result[, input$hr_adjust+1,  1]
      
      subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
      subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
      
      # print leaflet
      pal = colorBin(color[[1]], bins = bin[[1]])
      pal_FPD = colorBin(color[[2]], bins = bin[[2]])
      pal2 = colorBin(c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"), 1:10)
      pal3 = colorBin(c("#005A32", "#74C476", "#F7FCF5"), 0:0.125:1)

      popup1 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                      '<br><strong>Count of pick-ups: </strong><br>', subdat_data$count)
      popup2 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                      '<br><strong>Fair Per Distance: </strong><br>', subdat_data$FPD)
      popup3 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName)
      popup4 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                      '<br><strong>Percentage Paying Cash: </strong><br>', payper$PercentagePaying)
      
      

      pic1<-leaflet(subdat) %>%
        setView(lat=40.7128, lng=-74.0059, zoom=10) %>%
        addProviderTiles('CartoDB.Positron') 
      
      # IF VALUE SELECTED WAS COUNT
      if (input$CF == "count"){
        pic1<-pic1 %>%
          addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
                      popup = popup1, fillOpacity = .6, group = group1) %>%
          addLegend(position = "bottomright",
                    colors = color[[1]],
                    labels = label[[1]],
                    opacity = 0.6,
                    title = title[[1]])
      }
      
      # IF VALUE SELECTED WAS FPD
      else if (input$CF == "FPD"){
        pic1<-pic1 %>%
          addPolygons(fillColor = ~pal_FPD(FPD), color = 'grey', weight = 1, 
                      popup = popup2, fillOpacity = .6, group = group2) %>%
          addLegend(position = 'bottomright',
                    colors = color[[2]],
                    labels = label[[2]], ## legend labels (only min and max)
                    opacity = 0.6,      ##transparency again
                    title = title[[2]])
      }
    
      
      else if (input$CF == "cash"){
        pic1<-pic1 %>%
          addPolygons(fillColor =  ~pal3(payper$PercentagePayingCash), color = 'grey', weight = 1, 
                      popup = popup4, fillOpacity = .6, group = group3) %>%
          addLegend(position = 'bottomright',
                    colors = color[[3]],
                    labels = label[[3]], ## legend labels (only min and max)
                    opacity = 0.6,      ##transparency again
                    title = title[[3]])
      }
      
      
    })
    
    
    observe({
      #whenever map item is clicked, becomes event
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      #print("works")
      
      #create a data fram for longitude and latitude of click location
      dattest = data.frame(Longitude = event$lng, Latitude = event$lat)
      print(dattest)
      coordinates(dattest) <- ~ Longitude + Latitude
      proj4string(dattest) <- CRS("+proj=longlat")
      dattest <- spTransform(dattest, proj4string(myShape1))
      
      #rtest is to see which borough click falls in
      rtest = over(dattest, myShape1)

      #create plot based borough selected
      output$air_districttimeplot <- renderPlot({
        #returns null if no borough selected
        if (nrow(rtest) == 0) {
          return(NULL)
        }
        
        # returns histogram plot
        if (input$days == "All Day"){
          count_resultNTA = count_result1[which(rownames(count_result) == rtest$NTACode),,]
          print(count_resultNTA)
          count_resultNTA = apply(count_resultNTA, 1, sum)
          print(count_resultNTA)
          index <- c(0:23)
          dfcount_resultNTA <- data.frame(index, count_resultNTA)
          ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
            labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
        }
        else if (input$days == "Business Day"){
          count_resultNTA = count_result1[which(rownames(count_result) == rtest$NTACode),,1]
          index <- c(0:23)
          dfcount_resultNTA <- data.frame(index, count_resultNTA)
          ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
            labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
        }
        else if (input$days == "Not Business Day") {
          count_resultNTA = count_result1[which(rownames(count_result) == rtest$NTACode),,2]
          index <- c(0:23)
          dfcount_resultNTA <- data.frame(index, count_resultNTA)
          ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
            labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
        }
        
      })
    })
  
})


