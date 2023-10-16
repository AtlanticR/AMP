################################################################################
################################################################################
#### Prepare all the data for the study area maps!

# There are many ways of handling spatial data. 
# This was a bit of a learning experience, so I am combining several approaches.
# If I were to start over, I would probably use only sf objects, but I don't have 
# time for that! 

# For sf objects, data is stored pretty much in a dataframe. And there is a 
# Geometry column that specifies the vector data type (point, line polygon, etc)
# and the coordinates for that. 
# You can then plot in ggplot using geom_sf and it figures out how to plot it all for you

# If it's not in sf format, I need to get it into dataframe format (using fortify), and 
# then use the regular ggplot commands to plot everything manually (e.g., geom_polygon, geom_point, etc)

# For the maps of Canada, Atlantic and Pacific, I want the data to be in Lambert Conic Conformal projection
# For insets, I want data to be in UTM
# Data often need to be reprojected using spTransform or other approach


################################################################################
################################################################################

# Set up
source("TechReport/DataProcessing/rPackages.R")

# This specifies which samples were used for THIS analysis
source("QuantitativeAssessment/dataProcessing/QAcodeMatches.R")

# Now rearrange this data for plotting. This is what will be used to make the sf objects below
# Need to get the FlowCam codes, sampleCodes and the ones just identified in this analysis
samplesForMaps = qaID %>%
  left_join(fcDataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  select(regionYear, FlowCamID, selectForAnalysis) %>%
  filter(selectForAnalysis == "Yes") %>%
  distinct() 

samplesPac = samplesForMaps %>%
  filter(regionYear == "Pac 21") %>%
  left_join(pacJun21Loc, by = c("FlowCamID" = "flowcamCode")) %>%
  select(regionYear, FlowCamID, sampleCode, latitude, longitude)
  
samplesGulf = samplesForMaps %>%
  filter(regionYear == "Gulf 2020") %>%
  left_join(gulfLoc, by = c("FlowCamID" = "flowcamCode")) %>%
  select(regionYear, FlowCamID, sampleCode, latitude, longitude, latitudeEnd, longitudeEnd)

samplesNL = samplesGulf = samplesForMaps %>%
  filter(regionYear == "NL 2020" | regionYear == "NL 2021") %>%
  left_join(nlAllLoc, by = c("FlowCamID" = "flowcamCode")) %>%
  left_join(nlMeta) %>%
  select(regionYear, FlowCamID, sampleCode, latitude, longitude)



# Install this extra library which contains a Canadian province shapefile
devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearthhires")

################################################################################
## Reading in coastline data 

# Read in the data, reproject it to appropriate UTM zone, convert to a data frame for easier plotting with ggplot()
# I don't know what happened, but previously I was using tidy() instead of fortify() but then it randomly stopped working. I'm switching to fortify().

# All Gulf and Maritimes coastline were from Jeff Barrell. I'm not sure the original source, but in his email it was called maritimes_NAD83CSRS_UTM 
# These shapefiles for Sober Island were not detailed enough. Jeff prepared the dataset.
# It was from the “Nova Scotia topographic database - water features” https://nsgi.novascotia.ca/gdd/
# His note: It’s probably the inverse of what you want, in that the polygons are water rather than land, but that can be managed pretty easily with GIS. 
# I just created a square polygon covering the area of interest, then used an “erase” function to subtract the water polygons.

# For most of these, I selected only the relevant features within a bounding box (zoomed in), otherwise it's too detailed and takes too long to load
# I did this in QGIS (right click on layer, ), the code for clipping to a bounding box in R is pretty simple

# Gulf coastline
gulfCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/gulfCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Newfoundland coastline
seArmCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/newfoundlandCoastline.shp"), sp::CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Pacific coastline
# This is the Freshwater Atlas shapefile (I've had it since my undergrad)
lemmensCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/lemmensCoastline.shp"), sp::CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

################################################################################
## Get a map of Canadian provinces since the DFO Regions map above doesn't show them!!!

# See here for a helpful tutorial
# https://ahurford.github.io/quant-guide-all-courses/making-maps.html

# Get data for Canada
canada_map = ne_states(country = "canada", returnclass = "sf")
# Test what maps look like with more countries (didn't use)
countries_map = ne_countries(country = c("United States of America","Denmark","Greenland","Iceland","Russia","Japan"), scale = "medium", returnclass = "sf")

# Define the (Canada???) Lambert Conformal Conic projection
# See here for projection specifics: https://epsg.io/102002
can.lcc = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

################################################################################
### Get station coordinates and reproject them!

# Originally, I thought I could just use geom_point to plot the stations
# This is true if original projection is WGS 84 (i.e., crs = 4326)
# However, I have reprojected my coastline & lease layers, so these ALSO need to be reprojected

# Points (punctual stations) can easily be plotted with geom_sf() 
# Transects are more complicated. Ideally, could be created as sf line objects. But getting line data in the right format is very confusing!!
# So instead, I am getting the coordinates for the beginning and end of transect, then using geom_segment to connect them


## GULF 

# Get the coordinate positions for the beginning of the transect and convert to UTM for plotting 
gulfTransectWGS = st_as_sf(samplesGulf, coords = c("longitude", "latitude"), crs = 4326)
gulfTransectUTM = st_transform(gulfTransectWGS, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get the coordinate positions for the end of the transect and convert to UTM for plotting 
gulfTransectWGS.End = st_as_sf(samplesGulf, coords = c("longitudeEnd", "latitudeEnd"), crs = 4326)
gulfTransectUTM.End = st_transform(gulfTransectWGS.End, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Those commands DID convert the coordinates! But now they are the final column called "Geometry" and both coordinates are in the same cell
# I want to separate out into new columns
# Make new columns (lon and lat) with their respective UTM coordinates (these are the beginning of the transect)
gulfTransectUTM = gulfTransectUTM %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

# Make new columns (lonEnd and latEnd) with their respective UTM coordinates (end of the transect)
gulfTransectUTM.End = gulfTransectUTM.End %>%
  dplyr::mutate(lonEnd = sf::st_coordinates(.)[,1],
                latEnd = sf::st_coordinates(.)[,2])

# Add the end ones to the original data frame
gulfTransectUTM$lonEnd = gulfTransectUTM.End$lonEnd
gulfTransectUTM$latEnd = gulfTransectUTM.End$latEnd

# Remove the .End dataframe because it's probably taking up too much space
rm(gulfTransectUTM.End)


## PACIFIC
# All were punctual stations
# Need to remove the ones with NAs (these have no data, don't worry about them)
pacPunctualWGS = st_as_sf(samplesPac %>% drop_na(latitude), coords = c("longitude", "latitude"), crs = 4326) 
pacPunctualUTM = st_transform(pacPunctualWGS, CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## NEWFOUNDLAND

# All were punctual stations
# Need to remove the ones with NAs (these have no data, don't worry about them)
nlPunctualWGS = st_as_sf(samplesNL, coords = c("longitude", "latitude"), crs = 4326)
nlPunctualUTM = st_transform(nlPunctualWGS, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  filter(!sampleCode %in% rm.nl.samples) # exclude these samples

