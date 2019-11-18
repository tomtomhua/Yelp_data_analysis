
library(data.table)
library(leaflet)
library(dplyr)
library(ggplot2)

library(shiny)
library(shinythemes)


##https://bookdown.org/paulcbauer/idv2/13-4-maps-with-leaflet-shiny.html
server <- function(input, output) {
  
  business_city <- reactive({
    business[city == input$cityname]
  })
  
  review_city <- reactive({
    review[city == input$cityname]
  })
  
  
  output$yelp  <- renderLeaflet({ 
    
    # show description when you click the point on the map
    description <- paste("Name: ", business_city()$name, "<br>",
                         "stars: ", business_city()$stars, "<br>")
    
    # define the color palette for the sentiment
    col_pal <- colorFactor(c("red", "blue", "green"), business_city()$sentiment)
    
    #build the map
    bar_locations <- leaflet(business_city()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = description,
                       color = ~col_pal(sentiment), stroke = FALSE, fillOpacity = 0.5,
                       layerId = ~business_id) %>%
      addLegend("bottomleft", pal=col_pal, values=c("Negative", "Neutral", "Positive"), title="Sentiment",
                layerId="colorLegend")
    
    bar_locations
    
  })
  
  ## observing a click will return the `id` you assigned in the `layerId` argument
  observeEvent(input$yelp_marker_click, {
    
    click <- input$yelp_marker_click
    percentile <- round(ecdf(review_city()[,average_stars])(review_city()[business_id == click$id, average_stars]), 2)
    
    ## filter the data and output into a table
    output$table1 <- renderTable({
      review_city()[business_id == click$id]
    })
    
    output$plot1 <- renderPlot({
      ggplot(review_city(), aes(x=average_stars)) + geom_density() +
        geom_vline(aes(xintercept=review_city()[business_id == click$id, average_stars]),
                   color="red", linetype="dashed", size=1) +
        geom_text(aes(x=review_city()[business_id == click$id, average_stars]),
                  label=paste(business[business_id==click$id, name], " percentile in ", input$cityname, " : ", percentile), y=0.3, vjust=1.5, hjust=1, size=4) +
        ggtitle(paste0("Stars distirbution in ", input$cityname )) +
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
      
    })
    
    # output$plot2 <- renderPlot({
    #   
    #   ggplot(review_city(), aes(x=average_stars)) + geom_density() +
    #     geom_vline(aes(xintercept=review_city()[business_id == click$id, average_stars]),
    #                color="red", linetype="dashed", size=1) +
    #     geom_text(aes(x=review_city()[business_id == click$id, average_stars]),
    #               label=paste("percentile: ", percentile), y=0.3, vjust=1.5, hjust=1, size=4)
    #   
    # })
    
    output$text1 <- renderTable({
      x <- as.data.frame(customer_sug_function(click$id, input$cityname))
      names(x) <- "Suggestion"
      x
    })
    
    output$text2 <- renderTable({
      y <- as.data.frame(business_sug_function(click$id, input$cityname))
      names(y) <- "Suggestion"
      y
    })
    
    # output$text1 <- renderTable({
    #   y <- as.data.frame(customer_sug_function(click$id))
    #   names(y) <- "Suggestion"
    #   y
    # })
    
  })
  
}