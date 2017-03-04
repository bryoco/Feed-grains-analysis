library('dplyr')
library('ggplot2')
library("shiny")
library("stringr")

server <- function(input, output) {
  grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  
  grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))
  
  sample.grains <- sample_n(grains, 10000)
  
  
  which.city <- reactive({
     filter(market.grains, input$place == SC_GeographyIndented_Desc)
  })
  
  output$plot1 <- renderPlot({
  
    city <- ggplot(data = which.city()) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      scale_color_discrete(name  = "Product")
    
    return(city)
  })
  
  all.rel.grains <- filter(sample.grains) %>% 
    filter(unlist(SC_GeographyIndented_Desc) %in% c("Asia", "Europe/Eurasia", "Oceania", "Canada", "Mexico", "South America", "Africa"))
  
  output$plot2 <- renderPlot({
    
    facet.country.plot <- ggplot(data = all.rel.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      facet_wrap(~SC_GeographyIndented_Desc) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 5000) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc))
    return(facet.country.plot)
  })
  
  farm.price.change <- filter(grains, SC_Group_Desc == "Prices") %>% ## Needed merge conflict/change
    filter(SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
       filter(SC_GeographyIndented_Desc == 'United States') 
       
  
  output$plot3 <- renderPlot({
    prices.farmers <- ggplot(data = farm.price.change) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      facet_wrap(~SC_GroupCommod_Desc) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Farmer Compensation") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 13) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc), color = "blue")
    return(prices.farmers)
  })
  
  prices.grains <- filter(grains, SC_Group_Desc == 'Prices') %>%
    filter(SC_Frequency_Desc == 'Annual')
  View(prices.grains)

  output$plot4 <- renderPlot({
    years.change <- ggplot(data = prices.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc))+
      scale_color_discrete(name  = "Product")
    return(years.change)
  })
  
  market.grains <- filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    filter(SC_Unit_Desc == 'Dollars per bushel')
  
  output$plot5 <- renderPlot({
    market.price <- ggplot(data = market.grains) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Feed Prices") +
      scale_color_discrete(name  = "Product") +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc))
    return(market.price)
  })
  
  import.countries <- filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
     filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
        filter(SC_Frequency_Desc == 'Annual') %>% 
           select(SC_Frequency_Desc, SC_GroupCommod_Desc, SC_Attribute_Desc, SC_GeographyIndented_Desc, Amount, Year_ID) %>% 
              group_by(SC_GroupCommod_Desc, Year_ID, SC_Attribute_Desc) %>% 
                  summarize(Amount = sum(Amount))
  
  output$plot6 <- renderPlot({
    country.port <- ggplot(data = import.countries) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
      facet_wrap(~SC_Attribute_Desc)
    return(country.port)
  })
  
  import.sums <- filter(grains, SC_GroupCommod_Desc %in% c("Corn", "Oats", "Barley", "Sorghum")) %>% 
    filter(SC_Attribute_Desc %in% c("Imports, to U.S. from specified source", "Exports, from U.S. to specified destination")) %>% 
    filter(SC_Frequency_Desc == 'Annual') %>% 
    group_by(SC_GroupCommod_Desc, SC_Attribute_Desc) %>% 
    summarize(sum = sum(Amount))
  

}