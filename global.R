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
library('plyr')

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

source("ui.R")
source("server.R")

shinyApp(ui = my.ui, server = my.server)
