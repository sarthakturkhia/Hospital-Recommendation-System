library(dplyr)
library(gmapsdistance)
library(ggmap)
library("httr")
library(leaflet)
library(tidyverse)


hospital.df <- read.csv("hospital-lat-long-address.csv", stringsAsFactors = FALSE)

# Reads in nearby zipcodes within a user defined origin and radius
p.zip <-      98105
p.dist <-     10
p.unit <-     'mile'            #or 'km'
zip.api.key <-'FdS0Yzj1pSdRXjMAH8Ablj6zZ26XHZrgm6JzuCCyDAh4xTeXL1BgxGFP9PXwKKHw'
zip.url <-    paste0('https://www.zipcodeapi.com/rest/', zip.api.key, '/radius.csv/', p.zip, '/', p.dist, '/', p.unit)


response <- GET(zip.url)
p.zip.df <- read.csv(textConnection(content(response, 'text')))


# Selects a subset of the hospitals dataframe whose zipcodes match those near the user
p.hospital.df <- subset(hospital.df, ZIP.Code %in% p.zip.df$zip_code)


# Joins information about distance from API's df to hospital subset df
p.hospital.df <- merge(x = p.hospital.df, y = p.zip.df, by.x="ZIP.Code", by.y="zip_code") %>% arrange(distance)


# Creates and Renders map using subsetted hospital dataframe
leaflet() %>% addTiles() %>%  addMarkers(data = p.hospital.df, lat = ~ lat, lng = ~ lon, label = ~Hospital.Name)

# Used sources:
# https://blog.rstudio.com/2015/06/24/leaflet-interactive-web-maps-with-r/
# https://rstudio.github.io/leaflet/map_widget.html
# (not helpful) https://rstudio-pubs-static.s3.amazonaws.com/200007_6ff29e42dad3476a99fab364bf2f7d62.html#/11
