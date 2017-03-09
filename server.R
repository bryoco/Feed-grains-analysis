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
  
  ### Plots
  grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))
  
  which.city <- reactive({
    if(input$place == 'All Cities') {
      which.city <- filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
        filter(SC_Frequency_Desc == 'Annual') %>% 
        filter(SC_Unit_Desc == 'Dollars per bushel')
    } else{
      filter(grains, input$place == SC_GeographyIndented_Desc)
    }
  })
  
  output$plot.market.price <- renderPlot({
    city <- 
      ggplot(data = which.city()) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Market Price (dollars per Bushel)", title = "Change in Feed Grain Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      scale_color_discrete(name = "Product")
    
    return(city)
  })
  
  output$text.market.price <- renderText({HTML(paste('This graph looks at barley, corn, and oats market prices in 10 keystone cities in the US along with the cumulative of all of the cities.', 
                                                     
                                                     'The vast majority of cities only reported data about corn, with corn prices staying relatively constant from 1975 until 1993. Prices for all of these grain then took a general dip from 1993 until 2000, upon which they all drastically increase to present day. Prices dipped in the years prior to 2000 because the price of raw materials for many commodity goods was extremely low. This had a snowball effect, where the current economic crisis exacerbated price depression even more. Starting at 2000, an exponentially increasing large global population  combined with a decrease of food production in deference to biofuel crops led to a sharp rise in basic food prices. The last phenomenon is known as the 2000s commodities boom.',
                                                     
                                                     'Comparing the different types of grain, corn is very closely correlated to barley. When barley decreases, corn decreases a similar amount, and vice versa. In addition, corn and barley prices are very close in nominal value as well. Oats, on the other hand, is generally nominally cheaper than corn and barley. Also, while oats changes in similar directions to corn and barley, the correlation between oats and the other two is much weaker. Oats only increased at about half the rate of corn and barley. ',                                                                     
                                                     sep='<br/><br/>'))
  })
  
  
  farm.price.change <- 
    filter(grains, SC_Attribute_Desc == "Prices received by farmers") %>%
    filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_GeographyIndented_Desc == 'United States') 
  
  output$plot.farm.price <- renderPlot({
    prices.farmers <- 
      ggplot(data = farm.price.change) +
      geom_point(mapping = aes(x = Year_ID, y = Amount)) +
      facet_wrap(~SC_GroupCommod_Desc) +
      labs(x = "Year", y = "Price received by Farmer (dollars per bushel)", title = "Change in Farmer Compensation") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 13) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount), color = "blue")
    
    return(prices.farmers)
  })
  
  output$text.farm.price <- renderUI({HTML(paste('This graph looks at changes in how much farmers are paid for barley, corn, oats, and sorghum from the 1860s to present day.',
                                                 
                                                 'Once again, the prices farmers receive for barley and corn are extremely similar, in terms of nominal amount and trends; the best fit lines on the graphs look almost identical. Oats and sorghum both change in a similar direction to barley and corn, but are different in terms of volatility. Sorghum follows the same trends of increase and decrease that barley and corn do, but is much more volatile. Essentially, sorghum prices increase more than corn prices when they are increasing, but it also decreases more than corn prices when both are decreasing. Oats is on the opposite end of the spectrum, where the trends are similar to that of corn, but it is much less volatile than corn prices. In other words, when corn prices increase, oats prices increase a smaller amount, and when corn prices decrease, oats prices decrease a smaller amount. In essence, oats prices are more stable and less prone to fluctuations.',
                                                 sep='<br/><br/>'))
  })
  
  
  import.countries <- 
    filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
    group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
    summarise(Amount = sum(Amount))
  
  output$plot.imex <- renderPlot({
    country.port <- 
      ggplot(data = import.countries) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Total Amount Imported/Exported per Country (1000 metric tons)", title = "Change in Imports/Exports for the US") +
      facet_wrap(~SC_Attribute_Desc)
    
    return(country.port)
  })
  
  output$text.imex <- renderUI({HTML(paste('This graph looks at changes in how much farmers are paid for barley, corn, oats, and sorghum from the 1860s to present day.', 
                                           'Once again, the prices farmers receive for barley and corn are extremely similar, in terms of nominal amount and trends; the best fit lines on the graphs look almost identical. Oats and sorghum both change in a similar direction to barley and corn, but are different in terms of volatility. Sorghum follows the same trends of increase and decrease that barley and corn do, but is much more volatile. Essentially, sorghum prices increase more than corn prices when they are increasing, but it also decreases more than corn prices when both are decreasing. Oats is on the opposite end of the spectrum, where the trends are similar to that of corn, but it is much less volatile than corn prices. In other words, when corn prices increase, oats prices increase a smaller amount, and when corn prices decrease, oats prices decrease a smaller amount. In essence, oats prices are more stable and less prone to fluctuations.',
                                           sep='<br/><br/>'))
  })
  
  #####
  
  prices.grains.market <- 
    filter(grains, SC_Attribute_Desc %in% c('Prices, market')) %>%
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_GroupCommod_Desc == 'Corn') %>%
    filter(Year_ID > 1988 & Year_ID < 2016)
  
  # Determines mean market price for the US for every year. Goes by every year to standardize the columsn to be a static amount.
  prices.grains.market.mean <-  aggregate(prices.grains.market$Amount, by=list(prices.grains.market$Year_ID), FUN=mean) 
  
  prices.grains.farms <- 
    filter(grains, SC_Attribute_Desc %in% c('Prices received by farmers')) %>%
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_GroupCommod_Desc == 'Corn') %>%
    filter(Year_ID > 1988 & Year_ID < 2016)
  
  # Determines mean price received by farmers for the US for every year. Goes by every year to standardize the columsn to be a static amount.
  prices.grains.farms.mean <-  aggregate(prices.grains.farms$Amount, by=list(prices.grains.farms$Year_ID), FUN=mean) 
  
  
  import.countries.market <- function(country) {
    corn.imports <- filter(grains, SC_Commodity_Desc == "Corn") %>% 
      filter(SC_Attribute_Desc == "Imports, to U.S. from specified source") %>% 
      filter(SC_Frequency_Desc == 'Annual') %>% 
      filter(SC_GeographyIndented_Desc == country) %>% 
      filter(Year_ID < 2016) 
    
    corn.imports.sum <- aggregate(corn.imports$Amount, by=list(corn.imports$Year_ID), FUN=sum)
    # difference in the average market prices from the current year to the last year.
    market.dif <- prices.grains.market.mean[[2]][!(prices.grains.market.mean[[1]] == 1989)] - prices.grains.market.mean[[2]][!(prices.grains.market.mean[[1]] == 2015)]
    # difference in the sum import amount from the current year to the last year.
    im.dif <- corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 1989)] - corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 2015)]
    
    frame.market.im <- data.frame(market.dif, im.dif)
    
    im.market <- ggplot(data = frame.market.im, aes(y = market.dif, x = im.dif)) +
      geom_point() +
      labs(x = "Difference in Import Amount from Previous Year (1000 metric tons)", y = "Difference in Market Price from Previous Year (dollars per bushel)", title = paste("Change in Corn Market Prices in US Shown With Change in Imports from", country)) +
      geom_smooth()
    im.market
  }
  
  output$plot.canada.market <- renderPlot({
    import.countries.market('Canada')
  })
  
  output$text.canada.1 <- renderUI({HTML(paste('Using Import and Export data for the US, as well as data on prices received by the agricultural farmers and the market prices, we can see certain associations pop up over the years for different nations. We will use this data to specifically look at corn prices.',
                                           
                                           'Here, for Canada, we can see a positive correlation between the increase of our imports from them and the difference in market prices for corn in the US. Of course there is not enough data here to prove any causal effects, but we start to see how the change in imports affects the local prices of corn.',
                                           
                                           "<br/><br/>"))
  })
  
  output$plot.france.market <- renderPlot({
    import.countries.market('France')
  })
  
  output$text.france.1 <- renderUI({HTML(paste('In France we see a very similar trend. We can see a positive correlation between the increase of our imports from them and the difference in market prices for corn in the US. We chose to show France because it lies outside of North America and is a less prominent trader with the US. Again, this data supports our hypothesis that a change in imports and exports will affect prices in the US.',
                                               
                                               'With these plots, we propose that the US imports more corn when it has less supply, and when this occurs, the price of corn is likely to go up, as there is less quanitity available for everyone, so prices will increase to maximize profit of what smaller amount is left.',
                                           
                                               "<br/><br/>"))
 })
  
  export.countries.market <- function(country) {
    corn.exports <- filter(grains, SC_Commodity_Desc == "Corn") %>% 
      filter(SC_Attribute_Desc == "Exports, from U.S. to specified destination") %>% 
      filter(SC_Frequency_Desc == 'Annual') %>% 
      filter(SC_GeographyIndented_Desc == country) %>% 
      filter(Year_ID < 2016) 
    
    corn.imports.sum <- aggregate(corn.exports$Amount, by=list(corn.exports$Year_ID), FUN=sum)
    # difference in the average market prices from the current year to the last year.
    market.dif <- prices.grains.market.mean[[2]][!(prices.grains.market.mean[[1]] == 1989)] - prices.grains.market.mean[[2]][!(prices.grains.market.mean[[1]] == 2015)]
    # difference in the sum import amount from the current year to the last year.
    ex.dif <- corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 1989)] - corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 2015)]
    
    frame.market.ex <- data.frame(market.dif, ex.dif)
    
    ex.market <- ggplot(data = frame.market.ex, aes(y = market.dif, x = ex.dif)) +
      geom_point() +
      labs(x = "Difference in Export Amount from Previous Year (1000 metric tons)", y = "Difference in Market Price from Previous Year (dollars per bushel)", title = paste("Change in Corn Market Prices in US Shown With Change in Exports to", country)) +
      geom_smooth()
    ex.market
  }
  
  output$plot.japan.market <- renderPlot({
    export.countries.market("Japan")
  })
  
  output$text.japan.1 <- renderUI({HTML(paste('Now we will look at the market prices in the US when exports from the US change. Looking at Japan, one of the USs largest purchasers, we see the expected trend: market prices tending to go down when exporting increases.',
                                           
                                           'While in line with before, we can still only make predictions with this data. To explain this data, we believe that the US will export more corn when there is a surplus it, leading to local prices being cheaper as selling locally is often a cheaper alternative to selling over borders.',
                                           
                                           "<br/><br/>"))
  })
  
  
  import.countries.farm <- function(country) {
    corn.imports <- filter(grains, SC_Commodity_Desc == "Corn") %>% 
      filter(SC_Attribute_Desc == "Imports, to U.S. from specified source") %>% 
      filter(SC_Frequency_Desc == 'Annual') %>% 
      filter(SC_GeographyIndented_Desc == country) %>% 
      filter(Year_ID < 2016) 
    
    corn.imports.sum <- aggregate(corn.imports$Amount, by=list(corn.imports$Year_ID), FUN=sum)
    # difference in the average market prices from the current year to the last year.
    farm.dif <- prices.grains.farms.mean[[2]][!(prices.grains.farms.mean[[1]] == 1989)] - prices.grains.farms.mean[[2]][!(prices.grains.farms.mean[[1]] == 2015)]
    # difference in the sum import amount from the current year to the last year.
    ex.dif <- corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 1989)] - corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 2015)]
    
    frame.farm.im <- data.frame(farm.dif, ex.dif)
    
    ex.farm <- ggplot(data = frame.farm.im, aes(y = farm.dif, x = ex.dif)) +
      geom_point() +
      labs(x = "Difference in Import Amount from Previous Year (1000 metric tons)", y = "Difference in Farm Price from Previous Year (dollars per bushel)", title = paste("Change in Corn Farm Prices in US Shown With Change in Imports from", country)) +
      geom_smooth()
    ex.farm
  }
  
  output$plot.canada.farm <- renderPlot({
    import.countries.farm('Canada')
  })
  
  output$text.canada.2 <- renderUI({HTML(paste('Now we will go back and explore the change in prices received by farmers.',
                                           
                                           'First is Canada again. Exactly as before, there is a positive correlation between change in imports and change in prices for farmers. This only supports our hypotheses. There are really almost zero discrepencies between the market and farmer prices. This proves to be interesting, however, as we are led to believe through this that farmers prices for corn may be changing with the change in imports.',
                                           
                                           "<br/><br/>"))
  })
  
  output$plot.france.farm <- renderPlot({
    import.countries.farm('France')
  })
  
  output$text.france.2 <- renderUI({HTML(paste('Again, the prices for farmers in the US change as equally as market prices change when the amount of imports from France change.'))
  })
  
  export.countries.farm <- function(country) {
    corn.exports <- filter(grains, SC_Commodity_Desc == "Corn") %>% 
      filter(SC_Attribute_Desc == "Exports, from U.S. to specified destination") %>% 
      filter(SC_Frequency_Desc == 'Annual') %>% 
      filter(SC_GeographyIndented_Desc == country) %>% 
      filter(Year_ID < 2016) 
    
    # Sum all orn to get an aggregate of all corn products
    corn.imports.sum <- aggregate(corn.exports$Amount, by=list(corn.exports$Year_ID), FUN=sum)
    # difference in the average market prices from the current year to the last year.
    farm.dif <- prices.grains.farms.mean[[2]][!(prices.grains.farms.mean[[1]] == 1989)] - prices.grains.farms.mean[[2]][!(prices.grains.farms.mean[[1]] == 2015)]
    # difference in the sum import amount from the current year to the last year.
    ex.dif <- corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 1989)] - corn.imports.sum[[2]][!(corn.imports.sum[[1]] == 2015)]
    
    frame.farm.ex <- data.frame(farm.dif, ex.dif)
    
     im.farm <- ggplot(data = frame.farm.ex, aes(y = farm.dif, x = ex.dif)) +
      geom_point() +
      labs(x = "Difference in Export Amount from Previous Year (1000 metric tons)", y = "Difference in Farm Price from Previous Year (dollars per bushel)", title = paste("Change in Corn Farm Prices in US Shown With Change in Exports to", country)) +
      geom_smooth()
    im.farm
  }
  
  output$plot.japan.farm <- renderPlot({
    export.countries.farm('Japan')
  })
  
  output$text.japan.2 <- renderUI({HTML(paste('Yet again, farmer prices react to Japan the same way that market prices do.',
                                           
                                           'This data may seem insignificant, but we feel that it is still important to understand how voltatile ALL prices are and how when one price changes, they all can change, creating a chain reaction that may be hard to stop in some circumstances. Hoepfully with this data, you can gain new insight into the worlds agricultural system as it applies to the US and appreciate the sheer volume of data that is out there. We only scraped the surface of possibilities of understanding a simple grain: corn.',
                                           
                                           "<br/><br/>"))
  })
}

# Create server
shinyServer(my.server)
