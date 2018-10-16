#ui.R interacts with server.R; it passes along what manufacturer and nutrition the user
#wants to see and plots the resulting graph created by server.R
library(leaflet)
library(shiny)
shinyUI(navbarPage('Hospitals Near You',
                   sidebarLayout(
                     sidebarPanel(
                       

                       textInput(inputId = "zipcode", 
                                 label = h3("Your zipcode"), 
                                 value = "98105", 
                                 placeholder = 'e.g.- 98105'),

                       sliderInput(inputId = "distance",
                                   "Radius of Search:",
                                   min = 1,
                                   max = 50,
                                   value = 10),
                       
                       selectInput(inputId = 'unit',
                                   'Distance Unit',
                                   c('Kilometres' = 'km',
                                     'Miles' = 'mile')),
                       
                       submitButton("Submit"),
                       
                       hr(),
                       fluidRow(column(3, verbatimTextOutput("value")))
                       
                     ),
                     
                     mainPanel(
                       leafletOutput("mymap")
                     )
                   )
))

