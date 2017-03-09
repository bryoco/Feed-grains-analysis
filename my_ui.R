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
                  tabPanel("Market Price in the US", plotOutput('plot.farm.price'), cat(
                    'This graph looks at barley, corn, and oats market prices in 10 keystone cities in the US along with the cumulative of all of the cities. 
                    \n
                    The vast majority of cities only reported data about corn, with corn prices staying relatively constant from 1975 until 1993. Prices for all of these grain then took a general dip from 1993 until 2000, upon which they all drastically increase to present day. Prices dipped in the years prior to 2000 because the price of raw materials for many commodity goods was extremely low. This had a snowball effect, where the current economic crisis exacerbated price depression even more. Starting at 2000, an exponentially increasing large global population  combined with a decrease of food production in deference to biofuel crops led to a sharp rise in basic food prices. The last phenomenon is known as the 2000s commodities boom.
                    \n
                    Comparing the different types of grain, corn is very closely correlated to barley. When barley decreases, corn decreases a similar amount, and vice versa. In addition, corn and barley prices are very close in nominal value as well. Oats, on the other hand, is generally nominally cheaper than corn and barley. Also, while oats changes in similar directions to corn and barley, the correlation between oats and the other two is much weaker. Oats only increased at about half the rate of corn and barley. '                                                                          
                    )),
                  tabPanel("Prices Received by Farmers in the US", plotOutput('plot3')),
                  tabPanel("Import and Export Data Relative to the US", plotOutput('plot6'))
                  )
    )
  )
)
    