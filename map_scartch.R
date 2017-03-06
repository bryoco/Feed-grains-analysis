library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")

grains <- read.csv("./feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

# countries in interest
countries <-
  grains %>% 
  select(SC_Geography_ID, SC_GeographyIndented_Desc) %>% 
  unique() %>% 
  mutate(ISO3 = countrycode(SC_GeographyIndented_Desc, "country.name", "iso3c")) %>% 
  filter(!is.na(ISO3))

countries <- countries[!(grepl("U.S. -", countries$SC_GeographyIndented_Desc) | 
                       grepl("45", countries$SC_Geography_ID) | # Former Soviet Union-12
                       grepl("128", countries$SC_Geography_ID)),] # Former USSR

# imex = import and export, by annual
imex.all <- 
  grains %>% 
  filter(SC_Frequency_Desc == "Annual") %>%
  filter(SC_Group_Desc == "Exports and imports") %>%
  filter(SC_GroupCommod_Desc %in% c("Barley", "Corn", "Oats", "Sorghum"))

# remove unnecessary columns
imex.all <- imex.all[!(grepl("1,000 liters", imex.all$SC_Unit_Desc)),] # alcohol
drops <- c("SC_Group_ID", "SC_Group_Desc", "SC_GroupCommod_ID", "SortOrder", 
           "SC_Commodity_ID", "SC_Attribute_ID", "SC_Unit_ID", "SC_Frequency_ID", 
           "Timeperiod_ID", "Timeperiod_Desc")
imex.all <- imex.all[, !(names(imex.all) %in% drops)]

# only countries in interest
imex.all <- filter(imex.all, SC_Geography_ID %in% unlist(countries$SC_Geography_ID))

# prepare data for creating map
world <- map_data("world")
world <- world[world$region != "Antarctica",] # no country is in Antarctica
world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
world$region <- NULL # dont need region
world <- right_join(countries, world) %>% 
  filter(!is.na(SC_Geography_ID))
world.combined <- right_join(world, imex.all)
world.combined$subregion <- NULL # dont neet subregion

map <-
  ggplot(data = world.combined) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
  coord_quickmap() +
  facet_wrap(~Year_ID)