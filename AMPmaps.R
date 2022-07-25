# Make some leaflet maps for the AMP sampling sites out of the metadata files
# Code by Stephen Finnis July 2022


###############################################################################
## Get things set up

# Trying out a new way to install multiple packages at once
# Use the pacman package!
install.packages("pacman")

# Load them (I might not use all of them in the end)
pacman::p_load(dplyr, ggplot2, leaflet, mapr, mapview, 
               readxl)

# Set directory
# Look into here package and probably replace this 
setwd("C:/Users/FINNISS/Desktop")

# Load in metadata data files
# mar = Maritimes
marMeta = read_excel("FlowCamMetadata\\AMP Metadata Plankton_2021_Maritimes_21Dec2021.xlsx", sheet = "zoo") 
nlMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo")
pacMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_Pacific_Jan262022.xlsx", sheet = "zoo")
gulfMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_GULF_Feb22022_JB.xlsx", sheet = "zoo")

###############################################################################
## Pre-processing

# Create a function to only select relevant data from the metadata
processMeta = function(xlData) {
  dfProc = subset(xlData, sampleType == "Z" & # only get Zooplankton data
                    (netMesh == 250 | # only want net size of 236 or 250 um
                       netMesh == 236) & 
                    yearStart != 2019) # do not want 2019 data
  return(dfProc) # return processed data frame
}

# Process that data!
marZoo = processMeta(marMeta) # Maritimes zooplankton data
nlZoo = processMeta(nlMeta) # Newfoundland
pacZoo = processMeta(pacMeta) # Pacific
gulfZoo = processMeta(gulfMeta) # Gulf

###############################################################################
## Make the leaflet map

### MARITIMES
# Start the basemap (set zoomControl to false to hide the zoom buttons on map)
marMap = leaflet(options = leafletOptions(zoomControl = F)) %>% 
  
  # Add Esri world Imagery basemap from Esri
  # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldImagery) 

# Add the survey transects as lines from start (latitude/longitude) to end (-End)
for(i in 1:nrow(marZoo)){
  marMap = addPolylines(marMap, lat = c(marZoo[i,]$latitude, marZoo[i,]$latitudeEnd), 
                        lng = c(marZoo[i,]$longitude, marZoo[i,]$longitudeEnd))
}

# Show the map
marMap

#### NEWFOUNDLAND


#### MAKE A TEST MAPPING FUNCTION


# Create a function to only select relevant data from the metadata
mapMaker = function(mapData) {
  mapTemplate = leaflet(options = leafletOptions(zoomControl = F)) %>% 
    # Add Esri world Imagery basemap from Esri
    # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addCircleMarkers(data = mapData, ~as.numeric(longitude), ~as.numeric(latitude),
                     weight = 0.5,
                     col = 'black', 
                     fillColor = 'coral',
                     radius = 4, 
                     fillOpacity = 0.9, 
                     stroke = T)  %>%
  # add a map scalebar
  addScaleBar(position = 'topright')
  return(mapTemplate) # return processed data frame
}

nlMap = mapMaker(nlZoo)
nlMap
pacMap = mapMaker(pacZoo)
pacMap
marMap = mapMaker(marZoo)
marMap

# Start the basemap (set zoomControl to false to hide the zoom buttons on map)
nlMap = leaflet(options = leafletOptions(zoomControl = F)) %>% 
  # Add Esri world Imagery basemap from Esri
  # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data = nlZoo, ~longitude, ~latitude,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'coral',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T)
nlMap



#### Pacific

# A few checks
# I think there are a lot of sampling issues so I will have to review this all
pacZoo = subset(pacZoo, 
                !(is.na(latitude)))

# Start the basemap (set zoomControl to false to hide the zoom buttons on map)
pacMap = leaflet(options = leafletOptions(zoomControl = F)) %>% 
  # Add Esri world Imagery basemap from Esri
  # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data = pacZoo, ~as.numeric(longitude), ~as.numeric(latitude),
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'coral',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T)
pacMap


#### Gulf

# Start the basemap (set zoomControl to false to hide the zoom buttons on map)
gulfMap = leaflet(options = leafletOptions(zoomControl = F)) %>% 
  # Add Esri world Imagery basemap from Esri
  # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldImagery)

# Add the survey transects as lines from start (latitude/longitude) to end (-End)
for(i in 1:nrow(gulfZoo)){
  gulfMap = addPolylines(gulfMap, lat = c(gulfZoo[i,]$latitude, gulfZoo[i,]$latitudeEnd), 
                         lng = c(gulfZoo[i,]$longitude, gulfZoo[i,]$longitudeEnd))
}
gulfMap