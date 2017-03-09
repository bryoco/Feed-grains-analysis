library("dplyr")
library("plyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")

my.server <- function(input, output) {
  
  grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  
  grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))
  
  # sample.grains <- sample_n(grains, 10000)
  
  # US cities grain data
  which.city <- reactive({
     filter(market.grains, input$place == SC_GeographyIndented_Desc)
  })
  
  # Creates a plot showing the cities in America with feed grain data. Relates it to their output amount per year.
  output$plot1 <- renderPlot({
    city <- 
      ggplot(data = which.city()) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      scale_color_discrete(name  = "Product")
    
    return(city)
  })
  
  # grain data per continent :/
  all.rel.grains <- 
    filter(grains) %>% # <-- wait wut 
    filter(unlist(SC_GeographyIndented_Desc) %in% 
             c("Asia", "Europe/Eurasia", "Oceania", "Canada", "Mexico", "South America", "Africa"))
  
  # Continent amounts per year
  output$plot2 <- renderPlot({
    facet.country.plot <- 
      ggplot(data = all.rel.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      facet_wrap(~SC_GeographyIndented_Desc) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 5000) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc))
    
    return(facet.country.plot)
  })
  
  # Farm price change over years. MIGHT BE BROKEN. This data is an inferior form of "money" plot at end before map
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
  
  # prices of grains per year.
  prices.grains <- 
    filter(grains, SC_Attribute_Desc %in% c('Prices, market', 'Prices received by farmers')) %>%
       filter(SC_Frequency_Desc == 'Annual') %>% 
          filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum"))
  
  # View(prices.grains)

  # Inferior version of "money" plot at end before map
  output$plot4 <- renderPlot({
    years.change <- 
      ggplot(data = prices.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount), color = 'black')+
      scale_color_discrete(name  = "Product")
    
    return(years.change)
  })
  
  # Market price of grains per year.
  market.grains <- 
    filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Unit_Desc == 'Dollars per bushel')
  
  # plots market.grains per year.
  output$plot5 <- renderPlot({
    market.price <- 
      ggplot(data = market.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      scale_color_discrete(name  = "Product") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc))
    
    return(market.price)
  })
  
  # Import data for US per year. Gives sum of a 
  import.countries <- 
    filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
     filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
       filter(SC_Frequency_Desc == 'Annual') %>% 
         select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
             group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
               summarize(Amount = sum(Amount))
  
  # Plots import.countries. Sums of amounts (import and export) of the 4 grains per year.
  output$plot6 <- renderPlot({
    country.port <- 
      ggplot(data = import.countries) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      facet_wrap(~SC_Attribute_Desc)
    
    return(country.port)
  })
  
  # actually a copy of whats above...
  import.sums <- 
    filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    group_by(SC_GroupCommod_Desc, SC_Attribute_Desc) %>% 
    summarize(sum = sum(Amount))
  
  
  ##################
  # farm price for the specified years. Oats 2000 and 2001 is a special case. So is before 1989 and after 2015.
  # Takes mean Amount of 4 grains individually per year.
  farm.price.change <- 
      grains %>% 
        filter(SC_Commodity_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
          filter(SC_GeographyIndented_Desc == 'United States') %>% 
           filter(SC_Attribute_Desc %in% c('Prices received by farmers')) %>% 
             filter(Year_ID > 1988 & Year_ID < 2016) %>% 
               filter(SC_Frequency_Desc == 'Annual') %>% 
                filter(!(SC_GroupCommod_Desc == 'Oats' & (Year_ID == 2000 | Year_ID == 2001))) %>% 
                 group_by(Year_ID, SC_GroupCommod_Desc, SC_Attribute_Desc)
  
  # market price for the specified years. Oats 2000 and 2001 is a special case. So is before 1989 and after 2015.
  # Takes mean Amount of 4 grains individually per year.
  prices.grains.market <- 
    filter(grains, SC_Attribute_Desc %in% c('Prices, market')) %>%
       filter(SC_Frequency_Desc == 'Annual') %>% 
           filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
             filter(Year_ID > 1988) %>% 
               group_by(Year_ID, SC_GroupCommod_Desc, SC_Attribute_Desc) 
    
  # Import info on countries. Oats 2000 and 2001 is a special case. So is before 1989 and after 2015.
  # Takes total Amount of 4 grains individually per year as a sum of EVERY country. This is to 
  # standardize data (only way i could think of to get equal cols for everything)
  import.countries.spef <- 
    filter(grains, SC_Commodity_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(Year_ID < 2016) %>% 
    filter(!(SC_GroupCommod_Desc == 'Oats' & (Year_ID == 2000 | Year_ID == 2001))) %>% 
    select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
    group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
    summarize(Amount = sum(Amount))
  
  # Export info on countries. Oats 2000 and 2001 is a special case. So is before 1989 and after 2015.
  # Takes total Amount of 4 grains individually per year as a sum of EVERY country. This is to 
  # standardize data (only way i could think of to get equal cols for everything)
  export.countries.spef <- 
    filter(grains, SC_Commodity_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
      filter(SC_Attribute_Desc %in% c("Exports, from U.S. to specified destination")) %>% 
        filter(SC_Frequency_Desc == 'Annual') %>% 
         filter(Year_ID < 2016) %>% 
           filter(!(SC_GroupCommod_Desc == 'Oats' & (Year_ID == 2000 | Year_ID == 2001))) %>% 
             select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
               group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
                 summarize(Amount = sum(Amount))
  

  ##############################################################################
  # difference in the average market prices from the current year to the last year.
  market.dif <- prices.grains.market$Year_ID[!(prices.grains.market$Year_ID == 1989)] - prices.grains.market$Year_ID[!(prices.grains.market$Year_ID == 2015)]
  # difference in the average farm prices from the current year to the last year.
  farm.dif <- farm.price.change$Year_ID[!(farm.price.change$Year_ID == 1989)] - farm.price.change$Year_ID[!(farm.price.change$Year_ID == 2015)]
  # difference in the sum export amount from the current year to the last year.
  ex.dif <- export.countries.spef$Amount[!(export.countries.spef$Year_ID == 1989)] - export.countries.spef$Amount[!(export.countries.spef$Year_ID == 2015)]
  # difference in the sum import amount from the current year to the last year.
  im.dif <- import.countries.spef$Amount[!(import.countries.spef$Year_ID == 1989)] - import.countries.spef$Amount[!(import.countries.spef$Year_ID == 2015)]
  # data frames results
  farm.market <- data.frame(market.dif, ex.dif, grains = export.countries.spef$SC_GroupCommod_Desc[!(export.countries.spef$Year_ID == 1989)])
  
  # Graphs a planar plot of above data. Will use either import or export data and compare that against either farm or market data.
  # For example, shows how export amounts for 2003 change mean farm prices from 2002 to 2003.
  money <- ggplot(data = farm.market, aes(y = market.dif, x = im.dif)) +
    geom_point(aes(color = grains)) +
    xlim(-50000000, 50000000) + #ex: 100000000
    ylim(-2, 2) +
    geom_smooth(aes(color = grains))
  money # Market (high) - farmer (low) : export (high) - import (low) :: High/High = market high, export high. High/Low = e
  
  
  import.countries.spef.china <- function(country) {
    a <- filter(grains, SC_Commodity_Desc == "Corn") %>% 
    filter(SC_Attribute_Desc == "Imports, to U.S. from specified source") %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_GeographyIndented_Desc == country) %>% 
    filter(Year_ID < 2016) 
    
    b <- aggregate(a$Amount, by=list(a$Year_ID), FUN=sum)
  }
  iii <- import.countries.spef.china('Colombia')
  
  
  prices.grains.market <- 
    filter(grains, SC_Attribute_Desc %in% c('Prices, market')) %>%
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_GroupCommod_Desc == 'Corn') %>%
    filter(Year_ID > 1988)
  
   ccc <-  aggregate(prices.grains.market$Amount, by=list(prices.grains.market$Year_ID), FUN=mean) 
  
  # difference in the average market prices from the current year to the last year.
  market.dif.china <- ccc[[2]][!(ccc[[1]] == 1989)] - ccc[[2]][!(ccc[[1]] == 2015)]
  # difference in the average farm prices from the current year to the last year.
  farm.dif.china <- farm.price.change$Year_ID[!(farm.price.change$Year_ID == 1989)] - farm.price.change$Year_ID[!(farm.price.change$Year_ID == 2015)]
  # difference in the sum export amount from the current year to the last year.
  ex.dif.china <- export.countries.spef.china$Amount[!(export.countries.spef$Year_ID == 1989)] - export.countries.spef$Amount[!(export.countries.spef$Year_ID == 2015)]
  # difference in the sum import amount from the current year to the last year.
  im.dif.china <- iii[[2]][!(iii[[1]] == 1989)] - iii[[2]][!(iii[[1]] == 2015)]
  # data frames results
  farm.market.china <- data.frame(market.dif.china, im.dif.china)
  
  # Graphs a planar plot of above data. Will use either import or export data and compare that against either farm or market data.
  # For example, shows how export amounts for 2003 change mean farm prices from 2002 to 2003.
  money <- ggplot(data = farm.market.china, aes(y = market.dif.china, x = im.dif.china)) +
    geom_point() +
    geom_smooth()
  money # Market (high) - farmer (low) : export (high) - import (low) :: High/High = market high, export high. High/Low = e

  ##############################################################################
  
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
      filter(SC_GroupCommod_Desc == input$grain) %>% 
      filter(Year_ID == input$year)

    return(data)
  })
  
  output$debug1 <- renderDataTable({
    return(input)
  })
  
  output$debug2 <- renderDataTable({
    return(imex.all)
  })
  
  # # Preparing map data
  # world <- map_data("world")
  # world <- world[world$region != "Antarctica",] # no country is in Antarctica
  # world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
  # world$region <- NULL # dont need region
  # world <- right_join(countries, world) %>% 
  #   filter(!is.na(SC_Geography_ID))
  
  # Preparing imex data
  # world.combined <- right_join(world, imex.reactive)
  # world.combined$subregion <- NULL # dont neet subregion
  # 
  # output$map <- renderPlot({
  #   map <-
  #     ggplot(data = world.combined) +
  #     geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
  #     coord_quickmap()
  #   
  #   return(map)
  # })
}