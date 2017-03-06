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
  theme = shinytheme('sandstone'),
  titlePanel("US City Grain Prices"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('place', label = 'US City', 
                  choices = unique(market.grains$SC_GeographyIndented_Desc, 
                                   selected = "U.S. - Memphis, TN")),
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
                  tabPanel("Plot1", textOutput("Agriculture is becoming increasingly valuable as we enter an age where food production is "), plotOutput('plot1')),
                  tabPanel("Plot2", plotOutput('plot2')),
                  tabPanel("Plot3", plotOutput('plot3')),
                  tabPanel("Plot4", plotOutput('plot4')),
                  tabPanel("Plot5", plotOutput('plot5')),
                  tabPanel("Plot6", plotOutput('plot6')),
                  # Map
                  tabPanel("Map", 
                          
                           dataTableOutput('debug1'),
                           dataTableOutput('debug2')
                           
                           #plotOutput('map')
                           )
                  )
    )
  )
)
    