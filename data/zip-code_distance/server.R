library(dplyr)
library(gmapsdistance)
library(ggmap)
library("httr")
library(leaflet)
library(tidyverse)
library(shiny)
library(stringr)


hospital.df <- read.csv("hospital-lat-long-address.csv", stringsAsFactors = FALSE)
zip.api.key <-'PzFgQ1YvdpWmTwKZleDGkFUyJZ66j02QRnDL0lzKU4TH8fxU83RFDxN0zWBKZm0f'

server <- function(input, output) {
  
  
  # Creates and Renders map using subsetted hospital dataframe
  output$mymap <- renderLeaflet({

    # Validates user input
    validate(
        need(input$zipcode != "", "Please enter a zipcode"),
        need(grep("[0-9]{5}", input$zipcode), "Please enter a valid 5 digit zipcode"))
    
    # Reads in nearby zipcodes within a user defined origin and radius
    p.zip <-      input$zipcode
    p.dist <-     input$distance
    p.unit <-     input$unit            #or 'km'
    zip.url <-    paste0('https://www.zipcodeapi.com/rest/', zip.api.key, '/radius.csv/', p.zip, '/', p.dist, '/', p.unit)
    
    
    response <- GET(zip.url)
    p.zip.df <- read.csv(textConnection(content(response, 'text')))
    
    
    # Selects a subset of the hospitals dataframe whose zipcodes match those near the user
    p.hospital.df <- subset(hospital.df, ZIP.Code %in% p.zip.df$zip_code)
    
    
    # Joins information about distance from API's df to hospital subset df
    p.hospital.df <- merge(x = p.hospital.df, y = p.zip.df, by.x="ZIP.Code", by.y="zip_code") %>% arrange(distance)
    
    leafy <- leaflet() %>% addTiles() %>%  addMarkers(data = p.hospital.df, lat = ~ lat, lng = ~ lon, label = ~Hospital.Name)
    
    
    return(leafy)
    })
}

# Used sources:
# https://info201-s17.github.io/book/shiny.html
# https://info201.github.io/shiny.html
# https://shiny.rstudio.com/reference/shiny/latest/sliderInput.html
# https://blog.rstudio.com/2015/06/24/leaflet-interactive-web-maps-with-r/
# https://rstudio.github.io/leaflet/map_widget.html
# (not helpful) https://rstudio-pubs-static.s3.amazonaws.com/200007_6ff29e42dad3476a99fab364bf2f7d62.html#/11

# Need to read input from ui.R file; usually in the format- input$inputId
# Wrap everything that reads user information inside the output because it is reactive
# Return the final output with a return statement
# Don't have pipe operators on different lines
# Validate strings