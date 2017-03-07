### Deprecated from app.R

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
                                        sliderInput("year", "Year", min = year.range[1], max = year.range[2], value = 2016, step = 1),
                                        
                                        plotOutput("all_year", height = 200) # all year import export
                          ),
                          
                          tags$div(id="cite",
                                   'Data compiled for ', tags$em('Feed Grains Database'), ' by United States Department of Agriculture Economic Research Service.'
                          )
                      )
             ),
             
             tabPanel("Data viewer",
                      selectInput("imex1", "Import/Export", c("Imports, to U.S. from specified source",
                                                              "Exports, from U.S. to specified destination")),
                      selectInput("grain1", "Grain type", c("Barley", "Corn", "Oats", "Sorghum"),
                                  selected = "Corn"),
                      sliderInput("year1", "Year", min = year.range[1], max = year.range[2], value = 2016, step = 1),
                      DT::dataTableOutput("datatable")
             )
  )
)

# Create UI
shinyUI(my.ui)