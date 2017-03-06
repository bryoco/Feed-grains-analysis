library("dplyr")
library("ggplot2")
library("shiny")
library("shinythemes")
library("stringr")
library("maps")
library("countrycode")
library("RColorBrewer")
library("leaflet")

# Load data
grains <- read.csv("feedgrains.csv", stringsAsFactors = FALSE, strip.white = TRUE)

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
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
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
                                        conditionalPanel(sliderInput('year', "Year", 
                                                                     min = year.range[1], max = year.range[2], value = 2016,
                                                                     step = 1
                                        ),
                                        
                                        plotOutput("all_year", height = 200) # all year import export
                                        ),
                                        
                                        tags$div(id="cite",
                                                 'Data compiled for ', tags$em('Feed Grains Database'), ' by United States Department of Agriculture Economic Research Service.'
                                        )
                          )
                      )
             )
  )
)


my.server <- function(input, output) {
  
}

shinyApp(ui = my.ui, server = my.server)
