library(shiny)


  
navbarPage("NYC TRAFFIC", id="nav", 
           
           tabPanel("Time Flow Map",
      
                    leafletOutput("map", width = "150%", height = 700),
                
                    #set panel in the left side
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                  width = 330, height = "auto",
                               
                                  h2("Taxi and Uber hourly flow change"),
                                      
                                  selectInput("car","car type",c("Taxi","Uber","Both"),selected = "Taxi"),
                                      
                                  textInput("date", "Choose date", "1/1/2015"),
                                      
                                  sliderInput("time", "Hours of Day:", 
                                                  min = 0, max = 24, value = 0, step = 1,
                                                  animate=animationOptions(interval = 500)),
                                  helpText("Click play button to see dynamic flow data")
                    )
                    
                         
                    )
           


)













