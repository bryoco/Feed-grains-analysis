library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")
library("RColorBrewer")
library("geojsonio")
library("leaflet")
library("DT")
library("htmltools")
library("jsonlite")

# Load data
grains <- read.csv("./data/FeedGrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

### Pre-processing map data ###

# Countries in interest
countries <-
  grains %>%
  select(SC_GeographyIndented_Desc) %>%
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
drops <- c("SC_Group_ID", "SC_Group_Desc", "SC_GroupCommod_ID","SC_GroupCommod_Desc",
           "SortOrder", "SC_Commodity_ID", "SC_Attribute_ID", "SC_Unit_ID", "SC_Frequency_ID",
           "Timeperiod_ID", "Timeperiod_Desc")
imex.all <- imex.all[, !(names(imex.all) %in% drops)]
# Using only countries in interest
imex.all <- filter(imex.all, SC_Geography_ID %in% unlist(countries$SC_Geography_ID))
# Mutate ISO3 for `imex.all`
imex.all <- mutate(imex.all, ISO3 = countrycode(SC_GeographyIndented_Desc, "country.name", "iso3c"))


# Prepare map data
world <- map_data("world")
world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
world$region <- NULL
world$subregion <- NULL
world <-
  right_join(imex.all, world) %>%
  filter(!is.na(SC_Geography_ID))

write.csv(world, "~/map_data.csv")
