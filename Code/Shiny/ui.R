library(data.table)
library(leaflet)
library(dplyr)
library(ggplot2)

library(shiny)
library(shinythemes)

ui <- navbarPage("YELP", id="nav",
                 #1. Map               
                 tabPanel("Map",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                                #includeScript("gomap.js")
                              ),
                              
                              leafletOutput("yelp", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("YELP"),
                                            helpText("This app can give suggestion on your business and 
                                                     help you improve rating on Yelp."),   
                                            br(),
                                            
                                            selectInput("cityname", "City", city_name ),
                                            
                                            
                                            helpText("Contact us: tsai45@wisc.edu")
                                            )
                          )
                          
                 ),
                 
                 #2. Plot
                 tabPanel(
                   "Plot", plotOutput('plot1')
                 ),
                 
                 #3. Plot
                 tabPanel(
                   "Table", tableOutput('table1')
                 )
                 
)