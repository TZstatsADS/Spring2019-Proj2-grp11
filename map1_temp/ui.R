library(ggplot2)
library(leaflet)
library(shiny)
a <- navbarPage("My Application",
                tabPanel("Component 1",
                         
                         div(class="outer",
                                tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                leafletOutput("map1", width = "100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = 0, right = 40, bottom = "auto",
                                           width = 330, height = "auto",
                                           
                                           h3("Panel"),
                                           
                                           selectInput("weather", label = h3("Weather Condition"), 
                                                       choices = list("All Days","Sunny Days", "Bad Weather Days"), selected = "All Days"),
                                           
                                           sliderInput("hour", "Hours of Day:", label = "Choose the time of the day:",
                                                                   min = 0, max = 23, value = 8, step=1),
                                           
                                           checkboxInput("pickup", "Pick-ups", FALSE),
                                           checkboxInput("dropoff", "Drop-offs", FALSE)
                               

                             ),
                             
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                           width = 160, height = 120,
                                           
                                           radioButtons("CF", label = "Layers",
                                                        choices = list("Count" = "count", "Fare Per Distance" = "FPD","Cluster" = "Cluster"), 
                                                        selected = "count")
                                           
                                           
                             )
                             
                             
                             )
                         ),
                
                
                tabPanel("Component 2"),
                
                
                tabPanel("Component 3")
)     