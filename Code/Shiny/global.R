library(data.table)
library(leaflet)
library(dplyr)
library(ggplot2)

library(shiny)
library(shinythemes)


#need to put these variables in global.R
#choices for drop-downs
city_name <- c("Madison", "Las Vegas", "Phoenix", "Charlotte", "Pittsburgh")
business <- fread("business_sentiment.csv")
review <-  fread("review_sub.csv")
