setwd("../output")

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
library(ggpubr)
library(data.table)



######### Map 1 Data ########
color.1 = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
             color3 = c("#F9E79F", "#C0392B", "#A9CCE3", "#34495E", "#1E8449"))
label.1 = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6-7","7+"),
             label3 = c("Else","Entertainment","Residential Area","Work Area","Attractions"))
title.1 = list(t1 = "Pick Up Numbers", t2 = "Fair Per Distance",t3="Neighborhood Cluster")

load('PU_data.RData')
load('DO_data.RData')
load('nyc_nbhd.RData')
load('cluster1.RData')

######### Map 2 Data ########

ubercount_byhour_id <- fread("ubercount_byhour_id.csv")
load("count2016.RData")  # dataframe name: count_3
taxi2016count_byhour_id<-count2016
taxi2015count_byhour_id<-fread("PU15_Update.csv")
taxicount_byhour_id<-bind_rows(taxi2015count_byhour_id, taxi2016count_byhour_id)
both_byhour_id<-bind_rows(taxicount_byhour_id,ubercount_byhour_id)

######### Map 3 Data ########

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
load('myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))

# load dynamic data
dynamicdata = fread("pickupDropoff date_hour.csv", header = TRUE, stringsAsFactors=F)

# load airport traffic data
load('RSummarized/JFK_count_seperated.RData')
load('RSummarized/JFK_FPD_seperated.RData')
JFK_count_result = count_result
JFK_FPD_result = FPD_result

load('RSummarized/NWK_count_seperated.RData')
load('RSummarized/NWK_FPD_seperated.RData')
NWK_count_result = count_result
NWK_FPD_result = FPD_result

load('RSummarized/LGA_count_seperated.RData')
load('RSummarized/LGA_FPD_seperated.RData')
LGA_count_result = count_result
LGA_FPD_result = FPD_result

# load interactive data
load('count_seperated.RData')
load('FPD_seperated.RData')
# names of each borough. subdat is transformed shape1 file
rownames(count_result) = subdat@data$NTACode

# summarized data 
payper = read.csv("Data_frame_of_summary.csv")

#initialize data
count_result1 <- JFK_count_result
FPD_result1 <- JFK_FPD_result

######### Shiny App ########

shinyServer(function(input, output, session) {
  
  ######### Map 1 done########
  
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
    
    
    cluster_map_data <- geo_join(nyc_nbhd, tempt, "neighborhood", "nbhd")
    
    
    pal2 <- colorBin(color[[1]], bins=c(0,100,1000,10000,100000,1000000,10000000))
    popup1 = paste0('<strong>Neighborhood: </strong><br>', map_data@data$neighborhood, 
                    '<br><strong>Count: </strong><br>', map_data@data$totalcount)
    
    
    pal3 <- colorBin(color[[2]], bins=c(0,1,2,3,4,5,6,7))
    popup2 = paste0('<strong>Neighborhood: </strong><br>', FPD_map_data@data$neighborhood, 
                    '<br><strong>Fair Per Distance: </strong><br>', FPD_map_data@data$FPD)
    
    palc = colorFactor(c("#B0A8B9","#C34A36","#D7EBE5","#FF8066","#845EC2"), 1:5)
    
    popupc = paste0('<strong>Neighborhood: </strong><br>', cluster_map_data@data$neighborhood, 
                    '<br><strong>Category: </strong><br>', cluster_map_data@data$xx)
    
    
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
    
    else if(input$CF=="Cluster"){
      picC <- leaflet(cluster_map_data) %>%
        addTiles() %>% 
        addPolygons(fillColor = ~palc(index), popup = popupc, color = 'grey', weight = 1,fillOpacity = .7) %>% 
        addLegend(position = "bottomright",
                  colors = c("#B0A8B9","#C34A36","#D7EBE5","#FF8066","#845EC2"),
                  labels = c("Else","Entertaiment","Residential Area","Work Area","Attraction"),
                  opacity = 0.6,
                  title = "Category") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-73.98, 40.75, zoom = 10)
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
      plot11<- ggplot(data=dfcount_resultSun, aes(x=as.numeric(dropoff_hour), y=totalcount/sum(totalcount),fill=FPD)) +
        geom_bar(stat="identity",alpha=0.85)+
        geom_smooth(col="#F28123") + xlab("Hours") + ylab("Total DropOff Count Percentage")+
        ggtitle("Drop Off Flow Trend in Sunny Days")+ 
        ylim(0,0.1)+ 
        scale_fill_gradient(low="#56B1F7", high="#132B43") 
      
      dfcount_resultBad <- Badcount2[Badcount2$DOnbhd==match1$nbhd,]
      dfcount_resultBad$FPD <- unlist(BadFPD2[BadFPD2$DOnbhd==match1$nbhd,]$FPD)
      plot22<- ggplot(data=dfcount_resultBad, aes(x=as.numeric(dropoff_hour), y=totalcount/sum(totalcount),fill=FPD)) +
        geom_bar(stat="identity",alpha=0.85)+
        geom_smooth(col="#F28123") + xlab("Hours") + ylab("Total DropOff Count Percentage")+
        ggtitle("Drop Off Flow Trend in Bad Weather Days")+ 
        ylim(0,0.1)+
        scale_fill_gradient(low="#56B1F7", high="#132B43") 
      
      ggarrange(plot11, plot22,
                common.legend = TRUE, legend = "bottom"
      )
      
      
    })
    table_1 <- Sunnycount2%>%filter(DOnbhd==match1$nbhd) %>% group_by(DOnbhd) %>%
      mutate(TotalCount=sum(totalcount),Cluster=tempt$xx[as.character(tempt$nbhd)==match1$nbhd])
    names(table_1) <- c("Name","Hour","count","TotalCount","Category")
    output$table1 <- renderTable(
      unique(table_1[,c(1,4,5)])
    )
    
    table_2 <- Badcount2%>%filter(DOnbhd==match1$nbhd) %>% group_by(DOnbhd) %>%
      mutate(TotalCount=sum(totalcount),Cluster=tempt$xx[as.character(tempt$nbhd)==match1$nbhd])
    names(table_2) <- c("Name","Hour","count","TotalCount","Category")
    output$table2 <- renderTable(
      unique(table_2[,c(1,4,5)])
    )
    
  })
  
  ######### Map 2 done ########
  
  output$map.2<-renderLeaflet({
    
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
                   color = "gray",stroke = TRUE,fillOpacity = 0.5)
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
      #t <- filter(both_byhour_id, groups_byhour==date_time)
      t1 <- filter(ubercount_byhour_id, groups_byhour==date_time)
      t2 <- filter(taxicount_byhour_id, groups_byhour==date_time)
      map<-leaflet(t)%>% addTiles() %>%
        setView(-73.86, 40.72, zoom=10)%>%
        addProviderTiles("Stamen.Watercolor") %>%
        #addCircles(lng=~mean_long,lat=~mean_lat,weight = 5,radius = ~Count_byhour^(1/4)*200,
        #           color = "blue",stroke = TRUE,fillOpacity = 0.5)
        addCircles(lng=t1$mean_long,lat=t1$mean_lat,weight = 5,radius = t1$Count_byhour^(1/4)*200,
                   color = "gray",stroke = TRUE,fillOpacity = 0.5) %>%
        addCircles(lng=t2$mean_long,lat=t2$mean_lat,weight = 5,radius = t2$Count_byhour^(1/4)*200,
                   color = "yellow",stroke = TRUE,fillOpacity = 0.5)
      return (map)
      
    }
    
    
  })
  
  ######### Map 3 done ########
  
  # THIS IS LEAFLET FOR DYNAMIC MAP
  
  output$map2 <- renderLeaflet({
    picAirport <- leaflet() %>%
      setView(lat=40.75, lng=-73.98, zoom=11) %>%
      addProviderTiles('Stamen.Toner') 
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
    else if(input$airport == "NWK"){
      count_result1 <- NWK_count_result
      FPD_result1 <- NWK_FPD_result
    }
    else if(input$airport == "NWK"){
      count_result1 <- LGA_count_result
      FPD_result1 <- LGA_FPD_result
    }
    else{
      print("no airport")
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
    
    
    blocks_coord = data.frame(center_lng = rep(NA, 195), center_lat = rep(NA, 195)) # Combine borough coord for future marking purpose
    for (i in 1:195){ blocks_coord[i,] = subdat@polygons[[i]]@labpt }    # One more update: add long/lat permanently into myShape@data as
    
    
    #subdat@data$count = count_result[, input$hr_adjust+1,  1]
    
    subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # print leaflet
    pal = colorBin(color[[1]], bins = bin[[1]])
    pal_FPD = colorBin(color[[2]], bins = bin[[2]])
    pal2 = colorBin(c("#C2EEAA","#A2D78F","#83C074","#64A959","#44923E","#257B23","#066508"), 1:7)
    pal3 = colorBin(c("#005A32", "#74C476", "#F7FCF5"), 0:0.125:1)
    
    popup1 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Count of pick-ups: </strong><br>', subdat_data$count)
    popup2 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Fair Per Distance: </strong><br>', subdat_data$FPD)
    popup3 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName)
    popup4 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Percentage Paying Cash: </strong><br>', payper$PercentagePaying)
    
    
    picAirport<-leaflet(subdat) %>%
      setView(lat=40.75, lng=-73.98, zoom=10) %>%
      addProviderTiles('Stamen.Toner') 
    
    # IF VALUE SELECTED WAS COUNT
    if (input$CF.2 == "count.2"){
      picAirport<-picAirport %>%
        addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
                    popup = popup1, fillOpacity = .9, group = group1) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    
    # IF VALUE SELECTED WAS FPD
    else if (input$CF.2 == "FPD.2"){
      picAirport<-picAirport %>%
        addPolygons(fillColor = ~pal2(FPD), color = 'grey', weight = 1, 
                    popup = popup2, fillOpacity = .9, group = group2) %>%
        addLegend(position = 'bottomright',
                  colors = c("#C2EEAA","#A2D78F","#83C074","#64A959","#44923E","#257B23","#066508"),
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    
    
    else if (input$CF.2 == "cash"){
      picAirport<-picAirport %>%
        addPolygons(fillColor =  ~pal3(payper$PercentagePayingCash), color = 'grey', weight = 1, 
                    popup = popup4, fillOpacity = .9, group = group3) %>%
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
