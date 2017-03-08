# Map data
world <- getMap()

my.server <- function(input, output) {
  
  # Preparing imex data
  imex.reactive <- reactive({
    data <-
      imex %>%
      filter(SC_Attribute_Desc == input$imex) %>%
      filter(SC_Commodity_Desc == input$grain) %>%
      filter(Year_ID == input$year)
    
    data <- merge(world, data, by.x = "ISO3", by.y = "ISO3")
    
    return(data)
  })
  
  
  
  bins <- c(0, 1, 10, 50, 200, 500, 1000, 2000, Inf)
  
  # Create the map
  output$map <- renderLeaflet({
    
    pal <- colorBin("YlOrRd", domain = imex.reactive()$Amount, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g 1000 metric tons",
      imex.reactive()$SC_GeographyIndented_Desc, imex.reactive()$Amount
    ) %>% lapply(htmltools::HTML)
    
    m <- 
      leaflet(imex.reactive()) %>%
      # setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        # todo: need to be hidden from final version ###################
        accessToken = 
          'pk.eyJ1IjoiYnJ5b2NvIiwiYSI6ImNpenhzd2sxaDAyZXIzMms3anB2YnBmZnAifQ.yUJFrNDonPhL-W1bHC-WXg')) %>% 
      addPolygons(
        fillColor = ~pal(imex.reactive()$Amount),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~imex.reactive()$Amount, opacity = 0.7, title = NULL, position = "bottomright")
    
    return(m)
  })
  
  datatable.reactive <- reactive({
    data <-
      imex.all %>%
      filter(SC_Attribute_Desc == input$imex1) %>%
      filter(SC_Commodity_Desc == input$grain1) %>%
      filter(Year_ID == input$year1)
    
    return(data)
  })
  
  # Create data table
  output$datatable <- DT::renderDataTable({
    # Unnecessary fields
    drops <- c("SC_GroupCommod_Desc", "SC_Frequency_Desc", "SC_Unit_Desc", "Year_ID")
    data <- datatable.reactive()[, !(names(datatable.reactive()) %in% drops)]
    colnames(data) <- c("Country", "Commodity", "Import/Export", "Amount")
    
    return(data)
  })
  
}

# Create server
shinyServer(my.server)
