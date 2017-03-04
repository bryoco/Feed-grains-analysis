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
  
  # sample.grains <- sample_n(grains, 10000)
  
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
  
  all.rel.grains <- 
    filter(grains) %>% # <-- wait wut 
    filter(unlist(SC_GeographyIndented_Desc) %in% 
             c("Asia", "Europe/Eurasia", "Oceania", "Canada", "Mexico", "South America", "Africa"))
  
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
  
  prices.grains <- 
    filter(grains, SC_Group_Desc == 'Prices') %>%
    filter(SC_Frequency_Desc == 'Annual')
  
  # View(prices.grains)

  output$plot4 <- renderPlot({
    years.change <- 
      ggplot(data = prices.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc))+
      scale_color_discrete(name  = "Product")
    
    return(years.change)
  })
  
  market.grains <- 
    filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_Unit_Desc == 'Dollars per bushel')
  
  output$plot5 <- renderPlot({
    market.price <- 
      ggplot(data = market.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      scale_color_discrete(name  = "Product") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc))
    
    return(market.price)
  })
  
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
  
  import.sums <- 
    filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    group_by(SC_GroupCommod_Desc, SC_Attribute_Desc) %>% 
    summarize(sum = sum(Amount))
  
  
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
  
  # Preparing map data
  world <- map_data("world")
  world <- world[world$region != "Antarctica",] # no country is in Antarctica
  world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
  world$region <- NULL # dont need region
  world <- 
    right_join(countries, world) %>%
    filter(!is.na(SC_Geography_ID))

  # Preparing imex data
  world.combined <- right_join(world, imex.reactive())
  world.combined$subregion <- NULL # dont neet subregion
  
  output$map <- renderPlot({
    map <-
      ggplot(data = world.combined) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
      coord_quickmap()

    return(map)
  })
}