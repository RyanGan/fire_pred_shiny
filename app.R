# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan 
# Date Created: 2017-06-17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)

# read shape
grid_poly <- readOGR(dsn = 'grid_poly/', layer = 'grid_poly')
summary(grid_poly$fire_risk)

# set crs for poly
grs80 <- paste0("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
# transform
spTransform(grid_poly, CRS(grs80))
# set risk bins
bin <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)

pal <- colorBin(c("green", "red"), domain = c(0,0.5), bins = bin,
                na.color = "transparent")

# fire locations ----
# read in fire locations ----
fire_locations <- read.csv("./fire_locations.csv")
# type indicates either wildfire (WF) or prescription burn (RX)
# set color of WF to red and RX to green
pal_fire <- colorFactor(
  palette = c("red", "green"),
  levels = c("WF", "RX")
)
  

# ui section ----
head <- dashboardHeader(title = "Wildfire Risk (beta)",
                        titleWidth = 300)

# side bar
side <- dashboardSidebar(
  # reactive sidebar
  #radioButtons("date_smoke", label = h2("Date to Forecast"), 
  #             choices = date_list, selected = "layer_1")
) # end side bar

# body
body <- dashboardBody(
  # set tag style
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")
)# end dashboard body


# ui function with dashboard header, side, and body
ui <- dashboardPage(head,side,body, skin = "black") 
# server section ---- 
# consider adding a session function if I want to know statistics
server <- (function(input, output){
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-123.925,lng2=-74.425, lat1=48.225, lat2=33.975) %>% 
      # add fire locaiton icons
      addCircleMarkers(data = fire_locations, lat = fire_locations$latitude, 
                        lng = fire_locations$longitude, color = ~pal_fire(type),
                        radius = ~sqrt(area/100), fill=F, weight = 0.5) %>% 
      # add legend for smoke values
      addLegend(pal=pal, values=c(0, 0.5),
                title = htmltools::HTML("Fire Probability"),
                position = "bottomleft") 
    
  })# end base leaflet
  
  fire_risk <- sprintf(paste0(
    "<strong>Monthly Probability for Fire: %g</strong>",
    "<br> Predicted Size: %g Acres"), 
    # number for smoke concentration
    round(grid_poly$fire_risk,2),
    # number for relative risk respiratory
    round(grid_poly$fire_size,0)) %>% 
    lapply(htmltools::HTML)
  
  leafletProxy(mapId="map") %>%
    clearShapes() %>% 
    # set a box that defines the dimensions of bluesky forecast
    addRectangles(lng1=-130.0,lng2= -59.95, lat1=22.5, lat2=52.5,
                  fillColor = "transparent", color = "black", weight = 2) %>%
    # add smoke polygon 
    addPolygons(data = grid_poly, group = "Fire Risk", color = "transparent", 
                fillColor = ~pal(grid_poly$fire_risk), weight=1, smoothFactor=1, 
                fillOpacity=0.5, 
                # add highlight option
                highlight = highlightOptions(weight = 5, color = "blue", 
                                             bringToFront = T, 
                                             fillOpacity = 0.85),
                # add smoke pm values
                label = fire_risk,
                labelOptions = labelOptions(style = list("font-weight" = "normal", 
                  padding = "3px 8px"), textsize = "12px", direction = "auto")) 
  
})  
  # launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)
