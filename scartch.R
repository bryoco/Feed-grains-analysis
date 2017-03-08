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
           "Timeperiod_ID", "Timeperiod_Desc", "SC_Geography_ID")
imex.all <- imex.all[, !(names(imex.all) %in% drops)]

# Countries in interest
imex.all <- filter(imex.all, SC_Geography_ID %in% unlist(countries$SC_Geography_ID))

imex <-
  imex.all %>%
  filter(SC_Attribute_Desc == "Exports, from U.S. to specified destination") %>%
  filter(SC_Commodity_Desc == "Corn") %>%
  filter(Year_ID == 2016)

# Mutate ISO3
imex$ISO3 <- countrycode(imex$SC_GeographyIndented_Desc, "country.name", "iso3c")

world.geojson <- geojson_read("./json/countries.geo.json", what = "sp")

bins <- c(1, 20, 50, 100, 200, 500, 1000, 2000, Inf)

pal <- colorBin("YlOrRd", domain = imex.reactive()$Amount, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g 1000 metric tons",
  imex$SC_GeographyIndented_Desc, imex$Amount
) %>% lapply(htmltools::HTML)

m <- 
  leaflet(world.geojson) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = 
      'pk.eyJ1IjoiYnJ5b2NvIiwiYSI6ImNpenhzd2sxaDAyZXIzMms3anB2YnBmZnAifQ.yUJFrNDonPhL-W1bHC-WXg')) %>% 
  addPolygons(
    fillColor = ~pal(imex$Amount),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "padding" = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
      # add legends
      addLegend(pal = pal, values = ~imex.reactive()$Amount, opacity = 0.7, title = NULL, position = "bottomright")








mapStates = map(fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
