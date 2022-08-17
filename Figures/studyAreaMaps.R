################################################################################
### STUDY AREA MAPS

## BACKGROUND:
# The Aquaculture Monitoring Program (AMP) has sampling locations in four 
# DFO regions: Maritimes, Gulf, Newfoundland, Pacific.
# Each year (2020 and 2021), different bays may have been sampled within each
# region.
# Sampling locations were generally designed to sample at 3 locations within 
# each bay: near the shellfish aquaculture farms, further away from shellfish, 
# and near the mouth of the bay. This was designed to test how zooplankton
# populations are affected by shellfish aquaculture, but also by tides (e.g., 
# water coming into/out of the bay).
# There may also have been different types of tows, depending on the location
# Punctual station: tow taken from one spot 
# Transect: tow was towed behind boat

## PURPOSE OF CODE: 
# This code will create maps for the AMP sampling sites for each AMP region
# Leaflet maps will be created, which create a panel in the Viewer panel where
# users can zoom in/out, over a basemap.
# Punctual stations (points) will be shown in one colour, transects (lines) as 
# another.

## EXTRA INFO:
# Metadata files are not public
# Code by Stephen Finnis 2022
################################################################################

## Get things set up

# Clear console
rm(list=ls())
graphics.off()

################################################################################
## Read in relevant data processing code

# Get the processed metadata files
# These contain the sampling location, tow type, etc.
# Will return data frames with metadata info for each AMP region
source("DataProcessing/metadataProcessing.R")

################################################################################
## Create function to Make leaflet maps 
# Function will read in the metadata for each region (mapData) and then create 
# maps. It is easier to have one function do this, rather than have separate
# code for each sampling region.
# Sampling locations will be displayed as purple circles for non-transects 
# (punctual stations). Transects will be drawn as blue lines.

# Define function
mapMaker = function(mapData) {
  
  # Separates point from line data. There is a column for punctual vs transect
  # but it's inconsistent (spelling, typos etc.) so instead base it if there is an
  # "end" latitude (could be longitude instead) column with data.
  # In some cases, "NA" was typed into the column. These must be checked for in two ways
  punctual = subset(mapData, is.na(latitudeEnd) | latitudeEnd == "NA")
  # Convert to a numeric for entries that are actual numbers
  transect = subset(mapData, as.numeric(latitudeEnd)>1)
  
  # Get the leaflet map set up  
  # Setting zoomControl as false removes the zoom icon from the map
  mapTemplate = leaflet(options = leafletOptions(zoomControl = F)) %>% 

    # Add Esri world Imagery basemap
    # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles(providers$Esri.WorldImagery) %>%
    
    # Add circles for stations that are not transects (i.e., 'punctual stations')
    addCircleMarkers(data = punctual, ~as.numeric(longitude), ~as.numeric(latitude),
                     weight = 0.5,
                     col = 'black', 
                     # NOTE: NOT MANY COLOUR OPTIONS
                     # Need a workaround if want a specific colour (otherwise will be black)
                     fillColor = 'purple', 
                     radius = 4, 
                     fillOpacity = 0.9, 
                     stroke = T) %>%
  
    # Add a scalebar
    addScaleBar(position = 'topright')

  # Add the survey transects as lines from start (latitude/longitude) to end (..End)
  # First need to make sure this actually has data or will get an error message
  if (nrow(transect)>=1){ # check: is there data?
    for(i in 1:nrow(transect)){
    mapTemplate = addPolylines(mapTemplate, lat = c(transect[i,]$latitude, transect[i,]$latitudeEnd), 
                           lng = c(transect[i,]$longitude, transect[i,]$longitudeEnd))
    }
  }
  
  # Return the leaflet map with all the data plotted on it
  return(mapTemplate) 
}

################################################################################
## Call the mapmaker function defined above to create the maps
# Might have to zoom in a bit more to get better views of the bays within each 
# region.

# Some sites with NAs may get errors. Just ignore these
mapMaker(nlZoo) # Newfoundland
mapMaker(pacZoo) # Pacific
mapMaker(marZoo) # Maritimes
mapMaker(gulfZoo) # Gulf

################################################################################
################################################################################
################################################################################
## THINGS I CAN PROBABLY DELETE BUT I'M KEEPING JUST IN CASE:

## Code for making the maps separately: 
# I might need these later if each map has very custom things to add

## Just in case I want to make them separately (Delete these later)

## Newfoundland 
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

# Draw the map
nlMap

## Pacific
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

# Draw the map
pacMap

## Gulf

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

# Draw the map
gulfMap


## Maritimes
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

# Draw the map
marMap