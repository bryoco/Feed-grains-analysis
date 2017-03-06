library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")
library("leaflet")
library("RColorBrewer")
library("scales")
library("lattice")

grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

grains$SC_GeographyIndented_Desc <- str_trim(unlist(grains$SC_GeographyIndented_Desc))

my.server <- function(input, output) {
  
  # Countries in interest
  countries <-
    grains %>% 
    select(SC_Geography_ID, SC_GeographyIndented_Desc) %>% 
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
  drops <- c("SC_Group_ID", "SC_Group_Desc", "SC_GroupCommod_ID", "SortOrder", 
             "SC_Commodity_ID", "SC_Attribute_ID", "SC_Unit_ID", "SC_Frequency_ID", 
             "Timeperiod_ID", "Timeperiod_Desc")
  imex.all <- imex.all[, !(names(imex.all) %in% drops)]
  # Countries in interest
  imex.all <- filter(imex.all, SC_Geography_ID %in% unlist(countries$SC_Geography_ID))
  
  imex.reactive <- reactive({
    data <- 
      imex.all %>%
      filter(SC_Attribute_Desc == input$imex) %>% 
      filter(SC_Commodity_Desc == input$grain) %>% 
      filter(Year_ID == input$year)
    
    return(data)
  })
  
  # Preparing map data
  world <- map_data("world")
  world <- world[world$region != "Antarctica",] # no country is in Antarctica
  world$ISO3 <- countrycode(world$region, "country.name", "iso3c")
  world$region <- NULL # dont need region
  world <- 
    right_join(countries, world) %>%
    filter(!is.na(SC_Geography_ID))
  
  # Preparing imex data
  world.combined <- reactive({
    data <- right_join(world, imex.reactive())
    data$subregion <- NULL # dont neet subregion
    
    return(data)
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

}