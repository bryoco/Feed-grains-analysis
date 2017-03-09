### Merge plot 1/3/6

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
  
  ### Map
  output$map <- renderLeaflet({
    
    pal <- colorBin("YlOrRd", domain = imex.reactive()$Amount, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g metric tons",
      imex.reactive()$SC_GeographyIndented_Desc, (imex.reactive()$Amount * 1000)
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
  
  output$market.price <- renderPlot({
    return(market.price.reactive())
  })
  
  
  ### dataTable
  datatable.reactive <- reactive({
    data <-
      imex %>%
      filter(SC_Attribute_Desc == input$imex1) %>%
      filter(SC_Commodity_Desc == input$grain1) %>%
      filter(Year_ID == input$year1)
    
    data$ISO3 <- NULL
    
    return(data)
  })
  
  output$datatable <- DT::renderDataTable({
    # Unnecessary fields
    drops <- c("SC_GroupCommod_Desc", "SC_Frequency_Desc", "SC_Unit_Desc", "Year_ID")
    data <- datatable.reactive()[, !(names(datatable.reactive()) %in% drops)]
    colnames(data) <- c("Country", "Commodity", "Import/Export", "Amount (1000 metric tons)")
    
    return(data)
  })
  
  market.price.reactive <- reactive({
    return(p.market.price + geom_vline(xintercept = input$year))
  })
  
  ### Plot 1
  which.city <- reactive({
    filter(market.grains, input$place == SC_GeographyIndented_Desc)
  })
  
  output$plot1 <- renderPlot({
    city <- 
      ggplot(data = which.city()) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      scale_color_discrete(name  = "Product")
    
    return(city)
  })
  
  ### Plot 3
  farm.price.change <- 
    filter(grains, SC_Group_Desc == "Prices") %>%
    filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_GeographyIndented_Desc == 'United States') 
  
  output$plot3 <- renderPlot({
    prices.farmers <- 
      ggplot(data = farm.price.change) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      facet_wrap(~SC_GroupCommod_Desc) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Farmer Compensation") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 13) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc), color = "blue")
    
    return(prices.farmers)
  })
  
  ### Plot 6
  import.countries <- 
    filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
    group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
    summarize(Amount = sum(Amount))
  
  output$plot6 <- renderPlot({
    country.port <- 
      ggplot(data = import.countries) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      facet_wrap(~SC_Attribute_Desc)
    
    return(country.port)
  })
  
  ### bar chart 
  major.countries <- filter(imex, ISO3 %in% c("CHN","MEX", "BRA","CAN","JPN")) %>%
                      filter(SC_Attribute_Desc %in% "Imports, to U.S. from specified source")%>%
                       filter(SC_Commodity_Desc %in% "Corn")
  major.countries = ddply(major.countries, .(Year_ID), transform, percent = Amount/sum(Amount) * 100)
  major.countries = ddply(major.countries, .(Year_ID), transform, pos = (cumsum(Amount) - 0.5 * Amount))
  major.countries$label = paste0(sprintf("%.0f", major.countries$percent), "%")
  
  major.export.countries <- filter(imex, ISO3 %in% c("CHN","MEX", "BRA","CAN","JPN")) %>%
    filter(SC_Attribute_Desc %in% "Exports, from U.S. to specified destination")%>%
    filter(SC_Commodity_Desc %in% "Corn")
  major.export.countries = ddply(major.export.countries, .(Year_ID), transform, percent = Amount/sum(Amount) * 100)
  major.export.countries = ddply(major.export.countries, .(Year_ID), transform, pos = (cumsum(Amount) - 0.5 * Amount))
  major.export.countries$label = paste0(sprintf("%.0f", major.export.countries$percent), "%")
  
  output$plot9 <- renderPlot({
     countries.percentage <-
       ggplot(major.countries, aes(x = factor(Year_ID), y = Amount, fill = ISO3)) +
       geom_bar(position = position_stack(), stat = "identity", width = .7) +
       geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2) +
       labs(x = "Year", y = "Amount", title = "Import amount for corns in major countries to U.S.")+
       coord_flip()
     
     return(countries.percentage)
  })
  
  output$plot10 <- renderPlot({
    countries.export.percentage <-
      ggplot(major.export.countries, aes(x = factor(Year_ID), y = Amount, fill = ISO3)) +
      geom_bar(position = position_stack(), stat = "identity", width = .7) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2) +
      labs(x = "Year", y = "Amount", title = "Export amount for corns in major countries to U.S.")+
      coord_flip()
    
      return(countries.export.percentage)
  })
  
  output$text.import.export <- renderUI({HTML(paste('From this two graphs, we could see how import and export for corns from US to these major countries',
                                                    'We can see that US corns import is increasing, and America have more cooperation with Mexico and Brazil insteat of Canada. U.S has more chooice.',
                                                    'And from the export gragh for conrs, we can see that there is not obviously increasing for corns export, but it shows that the percentage of Mexicos is increasing.',
                                                    'the old major export country JAPAN is sharing less and less percentage.',
                                                    sep='<br/><br/>'))
   })
}

# Create server
shinyServer(my.server)
