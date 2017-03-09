library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")

grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))

# Range of year used in map
years <- grains %>% 
  filter(SC_Frequency_Desc == "Annual") %>%
  filter(SC_Group_Desc == "Exports and imports")

year.range <- range(years$Year_ID)

# sample.grains <- sample_n(grains, 10000)

market.grains <- 
  filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
  filter(SC_Frequency_Desc == 'Annual') %>% 
  filter(SC_Unit_Desc == 'Dollars per bushel')

my.ui <- fluidPage(
  # theme = shinytheme('sandstone'),
  titlePanel("US City Grain Prices"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('place', label = 'US City', 
                  choices = c(unique(market.grains$SC_GeographyIndented_Desc), "All Cities"),
                              selected = "All Cities"),
      selectInput('imex', label = "Type of trade",
                  choices = c("Imports, to U.S. from specified source",
                              "Exports, from U.S. to specified destination")),
      selectInput('grain', label = "Grain type", 
                  # Add 'sum' in the future
                  choices = c("Barley", "Corn", "Oats", "Sorghum")),
      sliderInput('year', label = "Year", 
                  min = year.range[1], max = year.range[2], value = 2016,
                  step = 1)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Market Price in the US", plotOutput('plot.market.price'), htmlOutput('text.market.price')),
                  
                  tabPanel("Prices Received by Farmers in the US", plotOutput('plot.farm.price'), htmlOutput('text.farm.price')),
                  
                  tabPanel("Import and Export Data Relative to the US", plotOutput('plot.imex'), htmlOutput('text.imex')),
                  
                  tabPanel("US Imports: Market Price - Canada", plotOutput('plot.canada.market')),
                  
                  tabPanel("US Imports: Market Price - France", plotOutput('plot.france.market')),
                  
                  tabPanel("US Exports: Market Price - Japan", plotOutput('plot.japan.market')),
                  
                  tabPanel("US Imports: Farm Price - Canada", plotOutput('plot.canada.farm')),
                  
                  tabPanel("US Imports: Farm Price - France", plotOutput('plot.france.farm')),
                  
                  tabPanel("US Exports: Farm Price - Japan", plotOutput('plot.japan.farm'))
                  )
    )
  )
)
    