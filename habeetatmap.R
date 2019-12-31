library("leaflet")
dat <- read.csv("_data/HABEETAT.csv")
dat$content <- "HA BEE TAT"
head(dat)
map <- leaflet(dat)
map1.5 <- addTiles(map)
map2 <- addCircleMarkers(map = map1.5, lng = dat$Longitude, 
                         lat = dat$Latitude, 
                         #label = ~htmlEscape(species),
                         #popup = ~content,
                         #color = ~pal(family),
                         #clusterOptions = markerClusterOptions()
                         )
map2

map2 <- addMarkers(map = map1.5, 
                   lng = ~dat$Longitude, 
                   lat = ~dat$Latitude,
                   icon = list(iconUrl = 'http://www.abejassilvestres.es/media/Paca.png',
                               iconSize = c(153/3, 102/3))
                   )
map2

map2 <- addMarkers(map = map1.5, 
                   lng = ~dat$Longitude, 
                   lat = ~dat$Latitude,
                   icon = list(iconUrl = 'http://www.abejassilvestres.es/media/habeetat.png',
                               iconSize = c(20, 18.4))
)
map2


map2 <- addMarkers(map = map1.5, 
                   lng = ~dat$Longitude, 
                   lat = ~dat$Latitude,
                   icon = list(iconUrl = 'http://www.abejassilvestres.es/media/LogoAAS2.png',
                               iconSize = c(50, 27))
)
map2
