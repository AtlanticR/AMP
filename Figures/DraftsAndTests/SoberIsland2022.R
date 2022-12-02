################################################################################
### TEST MAPS FOR 2022 SOBER ISLAND DATA

# Get the processed metadata files
# These contain the sampling location, tow type, etc.
# Will return data frames with metadata info for each AMP region


source("C:/Users/FINNISS/Desktop/AMP/DataProcessing/rPackages.R")

sober2022 = read.csv("C:/Users/FINNISS/Desktop/TEST2022_data_amp_maritime.csv")


# Start the basemap (set zoomControl to false to hide the zoom buttons on map)
soberMap = leaflet(options = leafletOptions(zoomControl = F)) %>% 
  
  # Add Esri world Imagery basemap from Esri
  # More options here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldImagery) 

# Add the survey transects as lines from start (latitude/longitude) to end (-End)
for(i in 1:nrow(sober2022)){
  soberMap = addPolylines(soberMap, lat = c(sober2022[i,]$latitude, sober2022[i,]$latitudeEnd), 
                        lng = c(sober2022[i,]$longitude, sober2022[i,]$longitudeEnd))
}

# Draw the map
soberMap
