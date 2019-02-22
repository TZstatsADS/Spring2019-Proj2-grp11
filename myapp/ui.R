setwd("~/Desktop/GR5243 Applied Data Science/Project 2/Spring2019-Proj2-grp11/myapp")
library(shiny)

navbarPage("NYC TRAFFIC", id="nav", 
           
           tabPanel("Time Flow Map",
                    
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        h2("Taxi and Uber hourly flow change"),
                        
                        selectInput("car","car type",c("Taxi","Uber","Both"),selected = "Taxi"),
                        
                        textInput("date", "Choose date", "2015-1-8"),
                        
                        sliderInput("time", "Hours of Day:", 
                                    min = 0, max = 24, value = 0, step = 1,
                                    animate=animationOptions(interval = 500)),
                        helpText("Click play button to see dynamic flow data")
                    )
                    
                    
           )
           
           
           
)
