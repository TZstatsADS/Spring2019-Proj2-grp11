library(ggplot2)
library(leaflet)
library(shiny)

setwd("../output")

vars <- c(
  "Business Day" = 1,
  "Not Business Day" = 2
)


navbarPage("NYC Traffic",id='map1',
           #################### 1st panel done ####################
           
           tabPanel("Taxi Traffic Overview",
                    
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
                                      
                                      checkboxInput("pickup", "Pick-ups", TRUE),
                                      checkboxInput("dropoff", "Drop-offs", FALSE),
                                      
                                      actionButton("details", "More Details")
                                      
                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 160, height = 120,
                                      
                                      radioButtons("CF", label = "Layers",
                                                   choices = list("Count" = "count", "Fare Per Distance" = "FPD","Category" = "Cluster"), 
                                                   selected = "count")
                                   
                        )
                   
                    )
           ),
           
           tabPanel("Neighborhood Details", value = "panel4",
                    h3(strong("Neighborhood Details"),align = "center"),
                    br(),
                    column(12,
                           plotOutput('plot1')
                    ),
                    
                    fluidPage(
                      fluidRow(
                        br(),
                        br(),
                        column(6,
                               tableOutput('table1')
                        ),
                        
                        column(6,
                               tableOutput('table2')
                        )
                      )
                    )
           ),
         
           #################### 2nd panel done ####################
           
                tabPanel("Time Flow Map(Taxi and Uber)",

                         leafletOutput("map.2", width = "150%", height = 700),
                         
                         #set panel in the left side
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                       width = 330, height = "auto",
                                       
                                       h2("Taxi and Uber hourly flow change"),
                                       
                                       selectInput("car","Car type",c("Taxi","Uber","Both"),selected = "Uber"),
                                       
                                       textInput("date", "Choose date", "2015-1-1"),
                                       
                                       sliderInput("time", "Hours of Day:", 
                                                   min = 0, max = 24, value = 0, step = 1,
                                                   animate=animationOptions(interval = 100)),
                                       helpText("Click play button to see dynamic flow data")
                                       )
                         
                         ), 
           
           #################### 3rd panel ####################
           
           tabPanel("Airport Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 160, height = 180,
                                      
                                      radioButtons("CF.2", label = "Layers",
                                                   choices = list("Fare Per Distance" = "FPD.2", "Count Number" = "count.2", "Cash Paying Percentage" = "cash"), 
                                                   selected = "FPD.2")
                                      
                                      
                        ),
                        
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h3("Panel"),
                                      
                                      
                                      selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),
                                      
                                      
                                      checkboxInput(inputId = "showhr",
                                                    label = strong("Show hours"),
                                                    value = FALSE),
                                      
                                      conditionalPanel(condition = "input.showhr == false"
                                                       
                                      ),
                                      
                                      
                                      conditionalPanel(condition = "input.showhr == true",
                                                       sliderInput(inputId = "hr_adjust",
                                                                   label = "Choose the time of the day:",
                                                                   min = 0, max = 23, value = NULL, step = 1)
                                      ),
                                      
                                      
                                      
                                      selectInput("airport", "Airport", c("JFK", "LGA", "NWK"),selected = "JFK"),
                                      
                                      
                                      plotOutput("air_districttimeplot", height = 280),
                                      helpText(   a("Analysis",
                                                    href="https://github.com/TZstatsADS/Spr2017-proj2-grp2/blob/master/doc/analysis.html")
                                      )
                        )
                        
                        # absolutePanel(id="graphstuff",class = "panel panel-default", fixed=TRUE,
                        #               draggable = TRUE, top=55, left="auto", right= 5, bottom="auto",width=300,
                        #               height=100, style="opacity:0.65",
                        #               
                        #               
                        #               h4("hourly flow change", align = "center"),
                        #               plotOutput("districttimeplot",height = 200))
                        
                    )
           )
           
           
           
           
           
           
           
           
)
