# Make some leaflet maps for the AMP sampling sites out of the metadata files
# Code by Stephen Finnis July 2022


###############################################################################
## Get things set up

# Clear console
rm(list=ls())
graphics.off()

# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("dplyr", "ggplot2", "leaflet", "mapr", "mapview", "readxl")
ipak(packages)

###############################################################################
## Load the data data

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
## Data cleaning

# Create a function to only select relevant data from the metadata
processMeta = function(xlData) {
  dfProc = subset(xlData, sampleType == "Z" & # only get Zooplankton data
                    (netMesh == 250 | # only want net size of 236 or 250 um
                       netMesh == 236) & 
                    yearStart != 2019) # do not want 2019 data
  return(dfProc) # return processed data frame
}

# Process the data
marZoo = processMeta(marMeta) # Maritimes zooplankton data
nlZoo = processMeta(nlMeta) # Newfoundland
pacZoo = processMeta(pacMeta) # Pacific
gulfZoo = processMeta(gulfMeta) # Gulf

###############################################################################
## Make leaflet maps (i.e., can zoom in/out in the Viewer panel)
# Sampling locations will be displayed as circles for non-transects
# Transects will be drawn as lines
# There's sampling data for 4 regions: Newfoundland, Maritimes, Gulf, and Pacific

# Create a function to create maps for any region
mapMaker = function(mapData) {
  
  # I'm sure there's a better way to do this. But first, split the data
  # This separates point from line data. There is a column for punctual vs transect
  # but it's inconsistent (spelling, typos etc.) so instead base it if there is an
  # "end" latitude (could be longitude instead) column with data
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
                     col = 'red', 
                     fillColor = 'red',
                     radius = 4, 
                     fillOpacity = 0.9, 
                     stroke = T) %>%
  
    # Add a scalebar
    addScaleBar(position = 'topright')

  # Add the survey transects as lines from start (latitude/longitude) to end (..End)
  # First eed to make sure this actually has data or will get an error message
  if (nrow(transect)>=1){ 
    for(i in 1:nrow(transect)){
    mapTemplate = addPolylines(mapTemplate, lat = c(transect[i,]$latitude, transect[i,]$latitudeEnd), 
                           lng = c(transect[i,]$longitude, transect[i,]$longitudeEnd))
    }
  }
  
  # Return processed data frame
  return(mapTemplate) 
}

# Call the functions and draw the maps!
# Some sites with NAs may get errors. Just ignore this
nlMap = mapMaker(nlZoo)
nlMap
pacMap = mapMaker(pacZoo)
pacMap
marMap = mapMaker(marZoo)
marMap
gulfMap = mapMaker(gulfZoo)
gulfMap

###############################################################################
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