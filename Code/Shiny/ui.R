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
                                            
                                            # plotOutput('plot2'),
                                            
                                            
                                            helpText("Contact us: tsai45@wisc.edu")
                                            )
                          )
                          
                 ),
                 
                 #2. Plot
                 tabPanel(
                   "Stars Distribution", plotOutput('plot1')
                 ),
                 
                 #3. Customer Suggestion
                 
                 tabPanel(
                   "Customer Suggestion", tableOutput('text1')
                   #"Suggestion", htmlOutput('text1')
                 ),
                 
                 # 4. Business Suggestion
                 tabPanel(
                   "Business Suggestion", tableOutput('text2')
                   #"Suggestion", htmlOutput('text1')
                 )
                 
)

