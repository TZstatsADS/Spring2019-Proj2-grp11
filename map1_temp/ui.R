library(ggplot2)
library(leaflet)
library(shiny)
a <- navbarPage("My Application",
                tabPanel("Component 1",
                         
                         div(class="outer",
                                tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                leafletOutput("map1", width = "100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                           width = 330, height = "auto",
                                           
                                           h3("Panel"),
                                           
                                           selectInput("select", label = h3("Weather Condition"), 
                                                       choices = list("Sunny Days", "Bad Weather Days"), selected = 1),
                                           
                                           sliderInput("hour", "Hours of Day:",  
                                                                   min = 0, max = 23, value = 8, step=1)
                               

                             )
                             
                             
                             )
                         ),
                
                
                tabPanel("Component 2"),
                
                
                tabPanel("Component 3")
)     