library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(RColorBrewer)
library(magrittr)
library(tidyverse)

## import data

property <- read.csv("department-of-neighborhood-development-property-inventory.csv")

## tidy data and get lat and long data

property$Location <-  gsub("\\(","",as.character(property$Location))
property$Location <-  gsub("\\)","",as.character(property$Location))
property %<>% mutate(Potential.Use=ifelse(Potential.Use=="", "Not Specified", Potential.Use))
property %<>% mutate(Neighborhood=ifelse(Neighborhood=="", "Not Specified", Neighborhood))
property %<>% 
  separate(Location, sep=",",into = c("Lat", "Long"), convert = T) 

## build ui for shiny app

ui <- 
  navbarPage("City Owned Property Map", collapsible = TRUE, inverse = TRUE,
             tabPanel("Boston Map", leafletOutput("bbmap", height=500)), 
             tabPanel("ZipCode Table", 
                      fluidPage(
                        fluidRow(
                          column(6,
                                 selectInput("code", "ZipCode", choices = property$ZipCode,  width = "100%")
                          ),
                          column(2, selectInput("y", "Y axis", c("count")))
                        ),
                        fluidRow(
                          column(4, tableOutput("Neighborhood")),
                          column(4, tableOutput("StreetName")),
                          column(4, tableOutput("StreetSuffix"))
                        ),
                        fluidRow(
                          column(12, plotOutput("neighbor"))
                        )
                        
                      )
             ),
             tabPanel("Map by Neighborhoods and Streets", 
                      fluidPage(
                        selectInput("neighbor", "Specify the neighborhood of the property", property$Neighborhood),
                        leafletOutput("nbmap", height=500),
                        selectInput("street", "Specify the street of the property", property$StreetName),
                        leafletOutput("ssmap",height=500)
                      )
             )
  )

server <- function(input, output) {
  
  # new column for the popup label
  
  bb_data <- mutate(property, cntnt=paste0('<br><strong>Address:</strong> ',Address,
                                           '<br><strong>Neighorhood:</strong> ',Neighborhood,
                                           '<br><strong>ZipCode:</strong> ', ZipCode,
                                           '<br><strong>Potential Use:</strong> ',Potential.Use,
                                           '<br><strong>Longitude:</strong> ', Long,
                                           '<br><strong>Latitude:</strong> ', Lat))
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = colorRampPalette(brewer.pal(8, "Set2"))(14), 
                     domain = c("Roslindale", "Mattapan", "Roxbury", "Dorchester", "South Boston", "Central", "Jamaica Plain", "West Roxbury", "South End", "Hyde Park", "East Boston", "Charlestown", "Allston/Brighton", "Back Bay/Beacon Hill"))
  
  # create the first leaflet map  
  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data, lat =  ~Lat, lng =~Long, 
                       radius = 3, 
                       popup = ~as.character(cntnt), 
                       color = ~pal(Neighborhood),
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      addLegend(pal=pal, values=bb_data$Neighborhood,opacity=1, na.label = "Not Available", title = "Neighborhood") %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) 
  })
  
  # create the third leaflet map
  
  selected1 <- reactive(bb_data %>% filter(Neighborhood == input$neighbor))
  output$nbmap <- renderLeaflet({
    leaflet(selected1(), options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
      }") %>%
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = selected1(), lat =  ~Lat, lng =~Long, 
                       radius = 3, 
                       popup = ~as.character(cntnt), 
                       color = 'blue',
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      )
  })
  
  selected2 <- reactive(bb_data %>% filter(bb_data$StreetName== input$street))  
  output$ssmap <- renderLeaflet({
    leaflet(selected2(), options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
      }") %>%
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = selected2(), lat =~Lat, lng =~Long, 
          radius = 3, 
          popup = ~as.character(cntnt), 
          color = "blue",
          stroke = FALSE, 
          fillOpacity = 0.8)
    })

  # table and graphs of second tab
  
  selected <- reactive(property %>% filter(ZipCode == input$code))
  
  count_top <- function(df, var, n = 5) {
    df %>%
      mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = 5)) %>%
      group_by({{ var }}) %>%
      summarise(n = n())
  }
  
  output$Neighborhood <- renderTable(count_top(selected(), Neighborhood), width = "100%")
  output$StreetName <- renderTable(count_top(selected(), StreetName), width = "100%")
  output$StreetSuffix <- renderTable(count_top(selected(), StreetSuffix), width = "100%")
  
  summary <- reactive({
    selected() %>%
      group_by(Neighborhood) 
  })
  
  output$neighbor <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(Neighborhood)) +
        geom_bar(width=0.2) +
        labs(y = "Estimated number of properties") +
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position="none")
    } 
  }, res = 96)
  
}

shinyApp(ui,server)



