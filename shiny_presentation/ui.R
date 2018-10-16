# loading all the libraries required for the project
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)


# Define a UI using the 'fluidPage()` layout
ui <- fluidPage(
  
  # sets the dashboard for the layout
  dashboardPage(skin = "green", 
                
                # the title for the dashboard
                dashboardHeader(title = "Hospital Finder"),
                
                # sets the side panel for the dashboard
                dashboardSidebar(
                  
                  # sets the different side bar options for the dashboard
                  sidebarMenu(
                    
                    # sets the second tab, the data table and the map page
                    menuItem("Search", tabName = "datatable", icon = icon("search")),
                    
                    # sets the first tab, the introduction page
                    menuItem("About", tabName = "about", icon = icon("users")),
                    
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
                    
                    selectInput("reason.for.visit", "Reason for Visit", c("Stroke", "Heart Failure", "Heart Attack",
                                                                          "Hip or Knee Replacement", "Coronary Bypass Artery Surgery", 
                                                                          "Tobacco Use", "Alcohol Use", "Other Surgery", "Mental Illness",
                                                                          "Other")),
                    div(style="display:inline-block;width:98%;text-align: center;",submitButton("Submit"))
                    
                  )
                ),
                
                # organizes the main body of the dashboard
                dashboardBody(
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                  ),
                  
                  # sets the primary box for the main body 
                  box(width = 20, status = "success",
                      tabItems(
                        
                        tabItem(tabName = "about",
                              #  h1("Insert introduction here"),
                              #  htmlOutput("introduction"), 
                                                            includeHTML("../about2.html"),
                              #                              includeHTML("../Technical_Description.html"),
                              br()
                        ),
                        
                        # adds the main data set tab 
                        tabItem(tabName = "datatable",
                                box(title = "Map", 
                                    id = "tabset1", 
                                    # height = "470px", 
                                    width = 12, 
                                    status = "success", 
                                    collapsible = TRUE,
                                    solidHeader = TRUE,
                                    leafletOutput("mymap")
                                    
                                ),
                                
                                box(
                                  title = "Hospitals Rankings", status = "success", solidHeader = TRUE,
                                  width = 12, 
                                  collapsible = TRUE,
                                  #height = "230px",
                                  DT::dataTableOutput('recommendedHospitals'))

                        )


                      )
                  )
                )
               )
             )

shinyUI(ui)