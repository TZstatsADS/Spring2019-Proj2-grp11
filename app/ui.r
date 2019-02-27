library(leaflet)

#Choices for drop-downs
vars <- c(
  "Business Day" = 1,
  "Not Business Day" = 2

)
#

navbarPage("NYC TAXI", id="nav", 
           #title = 'taxi menu',
           
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
                                      
                                      radioButtons("CF", label = "Layers",
                                                   choices = list("Count Number" = "count", "Fare Per Distance" = "FPD", "Cash Paying Percentage" = "cash"), 
                                                   selected = "count")
                                      
                                      
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
