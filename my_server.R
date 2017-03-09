library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")

my.server <- function(input, output) {
  
  grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  
  grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))
  
  which.city <- reactive({
    if(input$place == 'All Cities') {
     which.city <- filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
       filter(SC_Frequency_Desc == 'Annual') %>% 
       filter(SC_Unit_Desc == 'Dollars per bushel')
    } else{
        filter(market.grains, input$place == SC_GeographyIndented_Desc)
    }
  })
  
  output$plot.market.price <- renderPlot({
    city <- 
      ggplot(data = which.city()) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      scale_color_discrete(name  = "Product")
    
    return(city)
  })
  
  output$text.market.price <- renderText({HTML(paste('This graph looks at barley, corn, and oats market prices in 10 keystone cities in the US along with the cumulative of all of the cities.', 
                    
                                       'The vast majority of cities only reported data about corn, with corn prices staying relatively constant from 1975 until 1993. Prices for all of these grain then took a general dip from 1993 until 2000, upon which they all drastically increase to present day. Prices dipped in the years prior to 2000 because the price of raw materials for many commodity goods was extremely low. This had a snowball effect, where the current economic crisis exacerbated price depression even more. Starting at 2000, an exponentially increasing large global population  combined with a decrease of food production in deference to biofuel crops led to a sharp rise in basic food prices. The last phenomenon is known as the 2000s commodities boom.',
                                       
                                       'Comparing the different types of grain, corn is very closely correlated to barley. When barley decreases, corn decreases a similar amount, and vice versa. In addition, corn and barley prices are very close in nominal value as well. Oats, on the other hand, is generally nominally cheaper than corn and barley. Also, while oats changes in similar directions to corn and barley, the correlation between oats and the other two is much weaker. Oats only increased at about half the rate of corn and barley. ',                                                                     
                                       sep='<br/><br/>'))
  })
 
  
  farm.price.change <- 
    filter(grains, SC_Group_Desc == "Prices") %>%
    filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_GeographyIndented_Desc == 'United States') 
  
  output$plot.farm.price <- renderPlot({
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
      facet_wrap(~SC_Attribute_Desc)
    
    return(country.port)
  })
  
  output$text.imex <- renderUI({HTML(paste('This graph looks at changes in how much farmers are paid for barley, corn, oats, and sorghum from the 1860s to present day.', 
                                        'Once again, the prices farmers receive for barley and corn are extremely similar, in terms of nominal amount and trends; the best fit lines on the graphs look almost identical. Oats and sorghum both change in a similar direction to barley and corn, but are different in terms of volatility. Sorghum follows the same trends of increase and decrease that barley and corn do, but is much more volatile. Essentially, sorghum prices increase more than corn prices when they are increasing, but it also decreases more than corn prices when both are decreasing. Oats is on the opposite end of the spectrum, where the trends are similar to that of corn, but it is much less volatile than corn prices. In other words, when corn prices increase, oats prices increase a smaller amount, and when corn prices decrease, oats prices decrease a smaller amount. In essence, oats prices are more stable and less prone to fluctuations.',
                                        sep='<br/><br/>'))
                                           
  })
  ########################################################################
  
  
  
  
  
  ########################################################################
  # MAPS #
  
  # Countries in interest
  countries <-
    grains %>% 
    select(SC_Geography_ID, SC_GeographyIndented_Desc) %>% 
    unique() %>% 
    mutate(ISO3 = countrycode(SC_GeographyIndented_Desc, "country.name", "iso3c")) %>% 
    filter(!is.na(ISO3))
  
  # Remove unnecessary regions
  countries <- countries[!(grepl("U.S. -", countries$SC_GeographyIndented_Desc) | 
                             grepl("45", countries$SC_Geography_ID) | # Former Soviet Union-12
                             grepl("128", countries$SC_Geography_ID)),] # Former USSR
  
  # Import and export, by annual
  imex.all <- 
    grains %>% 
    filter(SC_Frequency_Desc == "Annual") %>%
    filter(SC_Group_Desc == "Exports and imports")
  
  # Remove unnecessary fields
  imex.all <- imex.all[!(grepl("1,000 liters", imex.all$SC_Unit_Desc)),] # alcohol
  drops <- c("SC_Group_ID", "SC_Group_Desc", "SC_GroupCommod_ID", "SortOrder", 
             "SC_Commodity_ID", "SC_Attribute_ID", "SC_Unit_ID", "SC_Frequency_ID", 
             "Timeperiod_ID", "Timeperiod_Desc")
  imex.all <- imex.all[, !(names(imex.all) %in% drops)]
  # Countries in interest
  imex.all <- filter(imex.all, SC_Geography_ID %in% unlist(countries$SC_Geography_ID))
  
  imex.reactive <- reactive({
    data <- 
      imex.all %>%
      filter(SC_Attribute_Desc == input$imex) %>% 
      filter(SC_Commodity_Desc == input$grain) %>% 
      filter(Year_ID == input$year)

    return(data)
  })
  
  # Preparing map data
  world <- map_data("world")
  world <- world[world$region != "Antarctica",] # no country is in Antarctica
  world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
  world$region <- NULL # dont need region
  world <- 
    right_join(countries, world) %>%
    filter(!is.na(SC_Geography_ID))

  # Preparing imex data
  world.combined <- reactive({
    data <- right_join(world, imex.reactive())
    data$subregion <- NULL # dont neet subregion
    
    return(data)
  })
  
  
  output$map <- renderPlot({
    map <-
      ggplot(data = world.combined()) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
      coord_quickmap()

    return(map)
  })
}