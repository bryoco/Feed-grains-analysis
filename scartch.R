

world.geojson <- geojson_read("./json/countries.geo.json", what = "sp")

m <- leaflet(world.geojson) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = 
      'pk.eyJ1IjoiYnJ5b2NvIiwiYSI6ImNpenhzd2sxaDAyZXIzMms3anB2YnBmZnAifQ.yUJFrNDonPhL-W1bHC-WXg'))

m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
imex.corn.ex.2016 <- imex.all %>%
  filter(SC_Attribute_Desc == "Exports, from U.S. to specified destination") %>% 
  filter(SC_Commodity_Desc == "Corn") %>% 
  filter(Year_ID == 2016)
pal <- colorBin("YlOrRd", domain = imex.corn.ex.2016$Amount, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(Amount),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)



m1 = map(world, fill = TRUE, plot = FALSE)
m1 <- leaflet(data = m1) %>% 
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)




