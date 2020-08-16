library(shiny)
library(leaflet)
library(lubridate)
library(fst)
library(dplyr)
data <- read_fst("pre_post.fst") 
dates <- unique(data$date)

pal <- colorNumeric(
    palette = "RdYlGn",
    domain = c(.2,1.6))



ui <- fluidPage(
    titlePanel("Traffic as a share of Feb median for that day of the week"),
    leafletOutput("mymap",height=800),
    sliderInput("date",
                "Date:",
                min   = min(data$date),
                max   = max(data$date),
                value = max(data$date)),

)

server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        data %>% 
        filter(date == input$date) %>%
        leaflet() %>% 
            addTiles() %>%
            addCircleMarkers(
                color = ~pal(change_bin),
                stroke = FALSE, fillOpacity = 0.5
            )  %>% 
            addLegend(pal = pal, values = ~change_bin, opacity = .2)
    })
}

shinyApp(ui, server)
