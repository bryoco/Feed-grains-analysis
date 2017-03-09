#### Dependencies and data ####

library("dplyr")
library("ggplot2")
library("shiny")
library("stringr")
library("maps")
library("countrycode")
library("RColorBrewer")
library("leaflet")
library("DT")
library("htmltools")
library("jsonlite")
library("data.table")
library("rworldmap")

# Load data
grains <- fread("./data/FeedGrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

#### Pre-processing map data ####

# Countries in interest
countries <-
  grains %>%
  select(SC_GeographyIndented_Desc) %>%
  unique() %>%
  mutate(ISO3 = countrycode(SC_GeographyIndented_Desc, "country.name", "iso3c")) %>%
  filter(!is.na(ISO3))

# Remove unnecessary regions
countries <- countries[!(grepl("U.S. -", countries$SC_GeographyIndented_Desc) |
                           grepl("Former Soviet Union-12", countries$SC_GeographyIndented_Desc) |
                           grepl("Former Ussr, Begins 1/1989 & Ends 1/1993", countries$SC_GeographyIndented_Desc)),]

# Import and export, by annual
imex <-
  grains %>%
  filter(SC_Frequency_Desc == "Annual") %>%
  filter(SC_Group_Desc == "Exports and imports")

# Remove unnecessary fields
imex <- imex[!(grepl("1,000 liters", imex$SC_Unit_Desc)),] # alcohol
drops <- c("SC_Group_ID", "SC_Group_Desc", "SC_GroupCommod_ID","SC_GroupCommod_Desc", "SC_Geography_ID",
           "SortOrder", "SC_Commodity_ID", "SC_Attribute_ID", "SC_Unit_ID", "SC_Frequency_ID", "SC_Frequency_Desc",
           "Timeperiod_ID", "Timeperiod_Desc")
imex <- imex[, !(names(imex) %in% drops)]
# Using only countries in interest
imex <- filter(imex, SC_GeographyIndented_Desc %in% unlist(countries$SC_GeographyIndented_Desc))

# Mutate ISO3 for `imex`
imex <- mutate(imex, ISO3 = countrycode(SC_GeographyIndented_Desc, "country.name", "iso3c"))

#### Plot data ####

# Market price of grains per year.
market.grains <- 
  filter(grains, SC_Attribute_Desc == 'Prices, market') %>% 
  filter(SC_Frequency_Desc == 'Annual') %>% 
  filter(SC_Unit_Desc == 'Dollars per bushel')

# Plots market.grains per year.
p.market.price <- 
  ggplot(data = market.grains) +
  geom_point(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc)) +
  labs(x = "Year", y = "Price (Dollars per bushel)", title = "Change in Feed Prices") +
  scale_color_discrete(name  = "Product") +
  geom_smooth(mapping = aes(x = Year_ID, y = Amount, color = SC_GroupCommod_Desc))

### Suraj
canada.imex.barley.imp <- filter(imex, imex$ISO3 == "CAN", SC_Commodity_Desc == "Barley", SC_Attribute_Desc == "Imports, to U.S. from specified source")
canada.imex.barley.exp <- filter(imex, imex$ISO3 == "CAN", SC_Commodity_Desc == "Barley", SC_Attribute_Desc == "Exports, from U.S. to specified destination")
canada.imex.oats.imp <- filter(imex, imex$ISO3 == "CAN", SC_Commodity_Desc == "Oats", SC_Attribute_Desc == "Imports, to U.S. from specified source")
canada.imex.oats.exp <- filter(imex, imex$ISO3 == "CAN", SC_Commodity_Desc == "Oats", SC_Attribute_Desc == "Exports, from U.S. to specified destination")

View(filter(imex, imex$ISO3 == "CHN"))

china.imex.barley.imp <- filter(imex, imex$ISO3 == "CHN", SC_Commodity_Desc == "Barley", SC_Attribute_Desc == "Imports, to U.S. from specified source")
china.imex.barley.exp <- filter(imex, imex$ISO3 == "CHN", SC_Commodity_Desc == "Barley", SC_Attribute_Desc == "Exports, from U.S. to specified destination")
china.imex.oats.imp <- filter(imex, imex$ISO3 == "CHN", SC_Commodity_Desc == "Oats", SC_Attribute_Desc == "Imports, to U.S. from specified source")
china.imex.oats.exp <- filter(imex, imex$ISO3 == "CHN", SC_Commodity_Desc == "Oats", SC_Attribute_Desc == "Exports, from U.S. to specified destination")
View(china.imex.oats.imp)
View(china.imex.barley.imp)

View(filter(imex, imex$ISO3 == "CAN"))

ggplot(canada.imex.barley, aes(Year_ID)) + 
  geom_line(aes(y = canada.imex.barley.imp$Amount, colour = "Barley Imports")) + 
  geom_line(aes(y = canada.imex.barley.exp$Amount, colour = "Barley Exports")) + 
  geom_line(aes(y = canada.imex.oats.imp$Amount, colour = "Oats Imports")) + 
  geom_line(aes(y = canada.imex.oats.exp$Amount, colour = "Oats Exports"))
  
ggplot(china.imex.oats.imp, aes(Year_ID)) +
  geom_line(aes(y = china.imex.barley.imp$Amount, colour = "China Barley Imports")) + 
  geom_line(aes(y = china.imex.barley.exp$Amount, colour = "China Barley Exports")) + 
  geom_line(aes(y = china.imex.oats.imp$Amount, colour = "China Oats Imports")) + 
  geom_line(aes(y = china.imex.oats.exp$Amount, colour = "China Oats Exports")) 

