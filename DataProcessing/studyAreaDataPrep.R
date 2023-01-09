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

# The code for putting everything together is in: Figures/StudyAreaMaps.R

################################################################################
################################################################################

# Set up
source("DataProcessing/rPackages.R")

# Install this extra library 
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
# This is the Freshwater Atlas shapefile (I've had it since my undergrad)
lemmensCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/lemmensCoastline.shp"), sp::CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))


################################################################################
## Shellfish leases
# These were also from Jeff Barrell!! I need to double check the metadata. Some may be empty, old, etc. 
# His note: NS and NB are current as of 2022, PEI as of 2020 (but it hasn’t changed much to my knowledge)

## Lease shapefiles
nsLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/NS_leases_Apr_2022.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
nbLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/MASMPS_Data.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
peiLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/PEI_leases_March_2020.shp"), sp::CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Newfoundland
# I used the Newfoundland Land Use Atlas: https://www.gov.nl.ca/landuseatlas/details/
# However, I was not sure how to extract the data as shapefiles
# Instead, I zoomed into the study area and saw two leases and used the Measurement --> Location tool to get the coordinates of the vertices
# I copied and pasted these into csv files
# Leases are for Crown title 118040 and 108739. Both say "Issue-Lease". Original title owner: Terry Mills
# 500m blue aquaculture buffer around leases confirm these are aquaculture leases
# If there were more leases, I would find a better way to obtain shapefiles, but this will work for now!
lease1Sf = sfheaders::sf_polygon(
  obj = read.csv("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/NLlease108739.csv"),
  x = "Lon",
  y = "Lat"
)

lease2Sf = sfheaders::sf_polygon(
  obj = read.csv("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/NLlease118040.csv"),
  x = "Lon",
  y = "Lat"
)

# Set csr to WGS 84 and then reproject to UTM zone 21 for both leases
sf::st_crs(lease1Sf) = 4326
st_transform(lease1Sf, crs = "+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
sf::st_crs(lease2Sf) = 4326
st_transform(lease2Sf, crs = "+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Pacific
# Shellfish leases are from Tantalis crown features
# Available from various different sources but I found this is best: https://fisheries-map-gallery-crm.hub.arcgis.com/datasets/governmentofbc::tantalis-crown-tenures
# If from Open Data, you can't click on many of the polygons to see what the features represent
# Filter by TENURE_PURPOSE == "AQUACULTURE" and TENURE_SUBPURPOSE == "SHELL FISH"
# Also turn "filter as map moves" on, so only data in the view get downloaded. Export as a shapefile.
# Note for now I am combining leases that have status "Application" AND "Tenured"
pacLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/TANTALIS_-_Crown_Tenures.shp"), sp::CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))


################################################################################
## DFO regions
# Data are from here: https://open.canada.ca/data/en/dataset/3862c9fa-dbeb-4f00-ac03-c5da6551bf00
# (click Download on the first option)
# However, I don't think (???) these show provinces. So I had to add the provinces from a different coastline layer over top (next section)
# Reproject to Lambert Conic conformal. (It might actually be a specific subtype. I can't remember. The coordinates specified might be important lol)
# Is it the NAD 1983 Labmert Canada projection???? Whatever!
dfoRegions = sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/dfoRegions.shp"), CRS("+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Turn into a dataframe but keep the original column names. I could do this with the other datasets, but it's not important to me
# I need to do this so I don't lose the DFO region names
dfoRegions.df = merge(fortify(dfoRegions), as.data.frame(dfoRegions), by.x="id", by.y=0)

################################################################################
## Get a map of Canadian provinces since the DFO Regions map above doesn't show them!!!

# See here for a helpful tutorial
# https://ahurford.github.io/quant-guide-all-courses/making-maps.html

# Get data for Canada
canada_map = ne_states(country = "canada", returnclass = "sf")
# Test what maps look like with more countries (didn't use)
countries_map = ne_states(country = c("United States of America","Canada","Denmark","Greenland","Iceland","Russia","Japan"), returnclass = "sf")

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

## MARITIMES

# Maritimes sites that were points (punctual stations)
marPunctualWGS = st_as_sf(marMeta %>% filter(facilityName == "Country Harbour" | facilityName == "WhiteHead"),
                          coords = c("longitude", "latitude"), crs = 4326)
marPunctualUTM = st_transform(marPunctualWGS, CRS("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Maritimes sites that were transects
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
# See here for different approaches of how to do this: https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
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
# Get the coordinate locations for each bay to be added to inset maps

# Get the locations of each bay (facilityName) within each region
# These are to be added to the inset maps as rectangles
# Only get the first row for each of these, otherwise all data points will plot. This is good enough since the map is zoomed out.
# I don't totally understand why this works since I thought the units were different (Canada: LCC, inset: UTM) but whatever! sf objects must just know what they're doing!
marTrMap = marTransectUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1) # only get the first entry for each bay

marPunMap = marPunctualUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)

gulfTrMap = gulfTransectUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)

nlPunMap = nlPunctualUTM %>%
  group_by(facilityName) %>%
  filter(row_number()==1)


