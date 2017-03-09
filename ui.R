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
                                        draggable = TRUE, top = 60, right = "auto", left = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Data explorer"),
                                        
                                        selectInput("imex", "Import/Export", c("Imports, to U.S. from specified source",
                                                                               "Exports, from U.S. to specified destination")),
                                        selectInput("grain", "Grain type", c("Barley", "Corn", "Oats", "Sorghum"),
                                                    selected = "Corn"),
                                        sliderInput("year", "Year", min = 1989, max = 2016, value = 1989, step = 1),
                                        
                                        plotOutput("market.price", height = 200) # anything you want to put in
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
             
             tabPanel("Plot 1",
                      selectInput('place', label = 'US City', 
                                  choices = unique(market.grains$SC_GeographyIndented_Desc, 
                                                   selected = "U.S. - Memphis, TN")),
                      plotOutput("plot1")),
             
             tabPanel("Plot 3", 
                      plotOutput("plot3")),
             
             tabPanel("Plot 6",
                      plotOutput("plot6")),
             tabPanel("Plot 9",
                      plotOutput("plot9"), plotOutput("plot10"),htmlOutput("text.import.export"))
  )
)

# Create UI
shinyUI(my.ui)