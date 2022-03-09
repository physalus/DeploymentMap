# read data from csv and ammend column names
data <- read.csv(file.choose(), sep=",",header=TRUE)
# subset by species
bm = subset(data, Species == 'bw')
bp = subset(data, Species == 'bp')
bb = subset(data, Species == 'bb')
orca = subset(data, Species == 'oo')
er = subset(data, Species == 'er')
mn = subset(data, Species == 'mn')


layerlist = sort(unique(data$Species))
colorFactors = colorFactor(c('red', 'orange', 'purple', 'blue', 'pink', 'brown'),
                           domain = data.df$Species)

# add the leaflet library to your script
library(leaflet)
# initiate the leaflet instance and store it to a variable
m = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addCircleMarkers( 
     lng = bm$Longitude, # we feed the longitude coordinates 
     lat = bm$Latitude,
     popup = bm$Name, 
     radius = 5, 
     stroke = FALSE, 
     fillOpacity = 0.75,
     clusterOptions = markerClusterOptions(),
     group = 'Blue'
  ) %>%
  addCircleMarkers( 
    lng = bp$Longitude, # we feed the longitude coordinates 
    lat = bp$Latitude,
    popup = bp$Name, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = 'Fin'
  ) %>%
  addCircleMarkers( 
    lng = bb$Longitude, # we feed the longitude coordinates 
    lat = bb$Latitude,
    popup = bb$Name, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = 'Brydes'
  ) %>%
  addCircleMarkers( 
    lng = orca$Longitude, # we feed the longitude coordinates 
    lat = orca$Latitude,
    popup = orca$Name, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = 'Orca'
  ) %>%
  addCircleMarkers( 
    lng = mn$Longitude, # we feed the longitude coordinates 
    lat = mn$Latitude,
    popup = mn$Name, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = 'Humpback'
  ) %>%
  addCircleMarkers( 
    lng = er$Longitude, # we feed the longitude coordinates 
    lat = er$Latitude,
    popup = er$Name, 
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    clusterOptions = markerClusterOptions(),
    group = 'Gray'
  ) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Oceans (default)", "OSM", "Toner"),
    overlayGroups = c('Blue', 'Fin','Humpback','Brydes','Gray','Orca'),
    options = layersControlOptions(collapsed = F),
    position = "topright"
  )
# we can "run"/compile the map, by running the printing it
m


# add the leaflet library to your script
library(leaflet)
# initiate the leaflet instance and store it to a variable
m1 = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Oceans (default)", "OSM", "Toner"),
    overlayGroups = sort(unique(data$Species)),
    options = layersControlOptions(collapsed = TRUE),
    position = "topright"
  ) %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addCircleMarkers( 
    lng = data$Longitude, # we feed the longitude coordinates 
    lat = data$Latitude,
    popup = paste(data$Name, data$DateTime, sep = ' - '),
    group = data$Species,
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    color = colorFactors(data$Species),
   # clusterOptions = markerClusterOptions()
    
  ) 
# we can "run"/compile the map, by running the printing it
m1


##
data.df <- split(data, data$Species)
names(data.df)

# add the leaflet library to your script
library(leaflet)
# initiate the leaflet instance and store it to a variable
m2 = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Oceans (default)", "OSM", "Toner"),
    overlayGroups =  names(data.df),
    options = layersControlOptions(collapsed = F),
    position = "topright"
  ) %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addCircleMarkers(data=data.df, 
    lng = data.df$Longitude, # we feed the longitude coordinates 
    lat = data.df$Latitude,
    popup = paste(data.df$Name, data.df$DateTime, sep = ' - '),
    group = data.df$Species,
    radius = 5, 
    stroke = FALSE, 
    fillOpacity = 0.75,
    color = colorFactors(data.df$Species),
    # clusterOptions = markerClusterOptions()
    
  ) 
# we can "run"/compile the map, by running the printing it
m2


colorFactors = colorFactor(c('red', 'orange', 'purple', 'blue', 'pink', 'brown'),
                           domain = data.df$Species)
library(leaflet)
# initiate the leaflet instance and store it to a variable
m3 = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner")
  
names(data.df) %>%
  purrr::walk( function(df) {
    m3 <<- m3 %>%
      addCircleMarkers(data=data.df[[df]],
                 lng=~Longitude, lat=~Latitude,
                 #label=~as.character(mag),
                 popup=~Name,
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 radius = 6, 
                 stroke = FALSE, 
                 fillOpacity = 0.5,
                 color = colorFactors(data.df[df]$Species)
                 #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 #labelOptions = labelOptions(noHide = T,
                                             #direction = 'auto')
                 )
  })

m3 %>%
  addLayersControl(
    baseGroups = c("Oceans (default)", "OSM", "Toner"),
    overlayGroups = names(data.df),
    options = layersControlOptions(collapsed = FALSE)
  )

