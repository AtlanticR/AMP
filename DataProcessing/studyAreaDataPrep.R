################################################################################
################################################################################
#### Prepare all the data for the study area maps!




################################################################################
################################################################################

# Set up
source("DataProcessing/rPackages.R")

# Install this extra library 
devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearthhires")




# I am combining multiple steps because this is a LOT of data and I don't want to keep creating new variables at each step
# Read in the data, reproject it (WGS84), convert to a data frame for easier plotting with ggplot() (although this maybe isn't necessary?)
# Mapping in R seems to constantly be changing, so I hope this is still somewhat current.
# Maritimes data have to be transformed to WGS 84. I think the code is constantly changing lol ahhh
# I don't know what happened, but previously I was using tidy() instead of fortify() but then it randomly stopped working. I'm switching to fortify().

## Coastline shapefiles

# Maritimes coastline
argyleCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/argyleCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
countryCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/countryCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
whiteheadCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/whiteheadCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
soberCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/soberCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Gulf coastline
gulfCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/gulfCoastline.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Newfoundland coastline
seArmCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/newfoundlandCoastline.shp"), sp::CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Pacific coastline
lemmensCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/lemmensCoastline.shp"), sp::CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

## Lease shapefiles
nsLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/NS_leases_Apr_2022.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
nbLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/MASMPS_Data.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
peiLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/PEI_leases_March_2020.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# DFO regions
dfoRegions = sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/dfoRegions.shp"), CRS("+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Turn into a dataframe but keep the original column names. I could do this with the other datasets, but it's not important to me
dfoRegions.df = merge(fortify(dfoRegions), as.data.frame(dfoRegions), by.x="id", by.y=0)



################################################################################
### Converting station coordinates

## MARITIMES

# Maritimes sites that were points (punctual stations)
marPunctualWGS = st_as_sf(marMeta %>% filter(facilityName == "Country Harbour" | facilityName == "WhiteHead"),
                          coords = c("longitude", "latitude"), crs = 4326)
marPunctualUTM = st_transform(marPunctualWGS, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Maritimes sites that were transects
# This is a MESS. I am 100% sure there is a better way to do this but I am in a rush!!
# It is horrible to deal with actual line sf objects in R. Instead, I am converting coordinates to UTM (as points) and using geom_segment to map them
# Get the coordinate positions for the beginning of the transect and convert to UTM for plotting 
marTransectWGS = st_as_sf(marMeta %>% filter(facilityName == "Sober Island Oyster" | facilityName == "Argyle"),
                          coords = c("longitude", "latitude"), crs = 4326)
marTransectUTM = st_transform(marTransectWGS, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get the coordinate positions for the end of the transect and convert to UTM for plotting 
marTransectWGS.End = st_as_sf(marMeta %>% filter(facilityName == "Sober Island Oyster" | facilityName == "Argyle"),
                          coords = c("longitudeEnd", "latitudeEnd"), crs = 4326)
marTransectUTM.End = st_transform(marTransectWGS.End, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Those commands DID convert the coordinates! But now they are the final column called "Geometry" and both coordinates are in the same cell
# I want to separate out into new columns
# Make new columns (lon and lat) with their respective UTM coordinates (these are the beginning of the transect)
marTransectUTM = marTransectUTM %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
              lat = sf::st_coordinates(.)[,2])
# Make new columns (lonEnd and latEnd) with their respective UTM coordinates (end of the transect)
marTransectUTM.End = marTransectUTM.End %>%
  dplyr::mutate(lonEnd = sf::st_coordinates(.)[,1],
                latEnd = sf::st_coordinates(.)[,2])

# Add the end ones to the original data frame
marTransectUTM$lonEnd = marTransectUTM.End$lonEnd
marTransectUTM$latEnd = marTransectUTM.End$latEnd

# Remove the .End dataframe because it's probably taking up too much space
rm(marTransectUTM.End)

## GULF 
# All sites were transects

# Get the coordinate positions for the beginning of the transect and convert to UTM for plotting 
gulfTransectWGS = st_as_sf(gulfMeta, coords = c("longitude", "latitude"), crs = 4326)
gulfTransectUTM = st_transform(gulfTransectWGS, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get the coordinate positions for the end of the transect and convert to UTM for plotting 
gulfTransectWGS.End = st_as_sf(gulfMeta, coords = c("longitudeEnd", "latitudeEnd"), crs = 4326)
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
pacPunctualWGS = st_as_sf(pacMeta %>% drop_na(latitude), coords = c("longitude", "latitude"), crs = 4326)
pacPunctualUTM = st_transform(pacPunctualWGS, CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## NEWFOUNDLAND

# All were punctual stations
# Need to remove the ones with NAs (these have no data, don't worry about them)
nlPunctualWGS = st_as_sf(nlMeta, coords = c("longitude", "latitude"), crs = 4326)
nlPunctualUTM = st_transform(nlPunctualWGS, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


################################################################################
## Make top layer of map
# Map of (A) Canada, (B) inset of Pacific and (C) inset of Atlantic 

# See here for a helpful tutorial
# https://ahurford.github.io/quant-guide-all-courses/making-maps.html

# Get data for Canada
canada_map = ne_states(country = "canada", returnclass = "sf")
# Test what maps look like with more countries (didn't use)
countries_map = ne_states(country = c("United States of America","Canada","Denmark","Greenland","Iceland","Russia","Japan"), returnclass = "sf")

# Define the Lambert Conformal Conic projection
can.lcc = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# Get the locations of each bay (facilityName) within each region
# Only get the first row for each of these, otherwise all data points will plot. This is good enough since the map is zoomed out.
# These are for plotting the bay locations
# I don't totally understand why this works since I thought the units were different (Canada: LCC, inset: UTM) but whatever! It must just know.
marTrMap = marTransectUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)

marPunMap = marPunctualUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)

gulfTrMap = gulfTransectUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)

nlPunMap = nlPunctualUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)


