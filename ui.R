my.ui <- fluidPage(
  navbarPage("US Feed Grain Data: Imports, Exports, and Fluxuating Prices", id = "nav",
             
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
                                        draggable = TRUE, top = 60, right = "auto", left = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Data explorer"),
                                        
                                        selectInput("imex", "Import/Export", c("Imports, to U.S. from specified source",
                                                                               "Exports, from U.S. to specified destination")),
                                        selectInput("grain", "Grain type", c("Barley", "Corn", "Oats", "Sorghum"),
                                                    selected = "Corn"),
                                        sliderInput("year", "Year", min = 1989, max = 2016, value = 1989, step = 1)
                                        
                                        #plotOutput("market.price", height = 200) # anything you want to put in
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
                      sliderInput("year1", "Year", min = 1989, max = 2016, value = 2016, step = 1),
                      DT::dataTableOutput("datatable")
             ),
             
             tabPanel("Market Price in the US", 
                      selectInput('place', label = 'US City', 
                                choices = c(unique(grains$SC_GeographyIndented_Desc[grains$SC_Attribute_Desc == 'Prices, market']), "All Cities"),
                                selected = "All Cities"),
                      plotOutput('plot.market.price'), htmlOutput('text.market.price')),
             
             tabPanel("Prices Received by Farmers in the US", plotOutput('plot.farm.price'), htmlOutput('text.farm.price')),
             
             tabPanel("Import and Export Data Relative to the US", plotOutput('plot.imex'), htmlOutput('text.imex')),
             
             tabPanel("US Imports and Exports in Relation to Market and Farm Price", plotOutput('plot.canada.market'), htmlOutput("text.canada.1"), 
                      plotOutput('plot.france.market'), htmlOutput("text.france.1"),
                      plotOutput('plot.japan.market'), htmlOutput("text.japan.1"),
                      plotOutput('plot.canada.farm'), htmlOutput("text.canada.2"),
                      plotOutput('plot.france.farm'), htmlOutput("text.france.2"),
                      plotOutput('plot.japan.farm'), htmlOutput("text.japan.2")
)))

# Create UI
shinyUI(my.ui)