library(ggplot2)
library(leaflet)
a <- navbarPage("My Application",
                tabPanel("Component 1",
                         
                         div(class="outer",
                             tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                             leafletOutput("map2", width = "100%", height = "100%")
                             )
                         ),
                
                
                tabPanel("Component 2"),
                
                
                tabPanel("Component 3")
)     


