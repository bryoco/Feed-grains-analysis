library('dplyr')
library('ggplot2')
library("shiny")
library("stringr")

grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))

sample.grains <- sample_n(grains, 10000)

market.grains <- filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
  filter(SC_Frequency_Desc == 'Annual') %>% 
  filter(SC_Unit_Desc == 'Dollars per bushel')

my.ui <- fluidPage(
  
  titlePanel("US City Grain Prices"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('place', label = 'US City', 
                  choices = unique(market.grains$SC_GeographyIndented_Desc, 
                                   selected = "U.S. - Memphis, TN"))
    ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
          tabPanel("Plot1", plotOutput('plot1')),
          tabPanel("Plot2" , plotOutput('plot2')),
          tabPanel("Plot3" , plotOutput('plot3')),
          tabPanel("Plot4" , plotOutput('plot4')),
          tabPanel("Plot5" , plotOutput('plot5'))
  )
  )
))
    