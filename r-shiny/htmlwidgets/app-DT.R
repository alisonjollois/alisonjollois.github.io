library(shiny)
library(shinydashboard)
library(leaflet)
library(geojsonio)

resume = txhousing %>%
  group_by(city) %>%
  summarise(volume = sum(volume, na.rm = TRUE))

txgeo = geojson_read("texas-city.geojson", what = "sp")
txgeo = subset(txgeo, sub(", TX", "", name) %in% unique(txhousing$city))

txgeo@data$city = sub(", TX", "", txgeo@data$name)
txgeo@data = dplyr::left_join(txgeo@data, resume, all.x = TRUE)

pal = colorNumeric("viridis", NULL)
map = leaflet(txgeo) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(volume), 
              fillOpacity = .5, 
              color = "red", weight = 1,
              label = ~paste0(city, ": ", formatC(volume, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~volume, opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(x)))

shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Test plotly"
    ),
    dashboardSidebar(),
    dashboardBody(
      leafletOutput("carte")
    ),
    title = "Test plotly",
    skin = "yellow"
  ),
  server = function(input, output) {
    output$carte <- renderLeaflet({
      map
    })
  }
)