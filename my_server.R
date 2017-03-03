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
  
  farm.price.change <- filter(grains,  SC_Group_Desc %in% c("Corn", "Oats", "Barley", "Sorghum", "Prices")) %>%
    filter(SC_GeographyIndented_Desc == 'United States')
  
  output$plot3 <- renderPlot({
    prices.farmers <- ggplot(data = farm.price.change) +
      geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_Attribute_Desc)) +
      facet_wrap(~SC_GroupCommod_Desc) +
      labs(x = "Year", y = "Price (Respective Scales of Product)", title = "Change in Farmer Compensation") +
      scale_color_discrete(name  = "Product") +
      ylim(0, 25) +
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
  

}