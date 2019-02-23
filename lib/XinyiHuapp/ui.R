library(shiny)
library(leaflet)

navbarPage("NYC TRAFFIC", id="nav", 
           
           tabPanel("Time Flow Map",
      
                    leafletOutput("map", width = "150%", height = 700),
                
                    #set panel in the left side
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                  width = 330, height = "auto",
                               
                                  h2("Taxi and Uber hourly flow change"),
                                      
                                  selectInput("car","Car type",c("Taxi","Uber","Both"),selected = "Uber"),
                                  
                                  textInput("year", "Choose year", "2015"),
                                  
                                  textInput("month", "Choose month", "1"),
                                  
                                  textInput("day", "Choose day", "1"),
                                  
                                  sliderInput("time", "Hours of Day:", 
                                                  min = 0, max = 24, value = 0, step = 1,
                                                  animate=animationOptions(interval = 500)),
                                  helpText("Click play button to see dynamic flow data")
                    )
                    
                         
                    )
           


)













