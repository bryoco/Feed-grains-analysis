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

### some sources
# http://rstudio.github.io/leaflet/choropleths.html
# https://rpubs.com/walkerke/leaflet_choropleth

# Load data
grains <- read.csv("./data/FeedGrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

# World GeoJSON data
world.geojson <- geojson_read("./json/countries.geo.json")

# Range of year used in map
years <- grains %>% 
  filter(SC_Frequency_Desc == "Annual") %>%
  filter(SC_Group_Desc == "Exports and imports")
year.range <- range(years$Year_ID)

my.ui <- fluidPage(
  navbarPage("Grain", id = "nav",
             
             tabPanel("Interactive map",
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("./css/styles.css"),
                            includeScript("./javascript/gomap.js")
                          ),
                          
                          leafletOutput("map", width="100%", height="100%"),
                          
                          # Shiny versions prior to 0.11 should use class="modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Data explorer"),
                                        
                                        selectInput("imex", "Import/Export", c("Imports, to U.S. from specified source",
                                                                               "Exports, from U.S. to specified destination")),
                                        selectInput("grain", "Grain type", c("Barley", "Corn", "Oats", "Sorghum"),
                                                    selected = "Corn"),
                                        sliderInput('year', "Year", min = year.range[1], max = year.range[2], value = 2016, step = 1),
                                        
                                        plotOutput("all_year", height = 200) # all year import export
                          ),
                                        
                          tags$div(id="cite",
                                   'Data compiled for ', tags$em('Feed Grains Database'), ' by United States Department of Agriculture Economic Research Service.'
                          )
                      )
             )
  )
)


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
    m <-
      leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) 
    
    return(m)
  })
  
}

shinyApp(ui = my.ui, server = my.server)
