library(leaflet)

leaflet() %>% 
  setView(lng=126.9784, lat=37.566, zoom = 19) %>% 
  addTiles()

sb <- read.csv('./data/starbucks.csv', fileEncoding = "CP949")
leaflet(sb) %>% 
  setView(lng=126.9784, lat=37.566, zoom=11) %>% 
  addTiles() %>% 
  addCircles(lng=~long, lat=~lat) %>% 
  addMarkers(icon = makeIcon("./data/starbucks.jpg", "./data/starbucks.jpg", 15, 15), popup = ~address)