################################################################################
################################################################################
#### STUDY AREA MAPS

# For making my study area map with all the bays identified
# Includes sampling locations for each bay and shellfish leases

# The final map 





################################################################################
################################################################################

# Set up

source("DataProcessing/rPackages.R")


# Install this extra library 
devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearthhires")

# For saving my maps, just so nothing changes, the plotting window on my screen is:
# Height: 25.5
# Width: 25cm











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

# Turn into a dataframe but keep the original column names
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


atlColour = c("Gulf" = "#F8766D", 
              "Maritimes" = "#7CAE00", 
              "Newfoundland" = "#00BFC4")
# Pacific Ocean 
# Note this is the colour when only Pacific OCEAN data is displayed
# When Pacific (region) data is broken down by field season (below), 4 colours will be set
pacColourOne = c("Pacific" = "#C77CFF")


cols <- c("Newfoundland & Labrador" = "#00BFC4", "Pacific" = "#C77CFF", "Ontario and Prairie" = "gray92", "Quebec" = "gray92",
          "Arctic" = "gray92", "Arctic-Water" = "gray92", "Maritimes" = "#7CAE00", "Gulf" = "#F8766D")

canMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", alpha = 0.7, linewidth = 0.1)+
  scale_fill_manual(values=cols, limits = c("Gulf", "Maritimes", "Newfoundland & Labrador", "Pacific"),
                    labels = c("Gulf", "Maritimes", "Newfoundland", "Pacific", "Other"), name = "Regions")+
  geom_sf(data = canada_map, linewidth = 0.01, fill = NA, col = "black")+
  coord_sf(crs = can.lcc, xlim = c(-2414929, 3168870), ylim = c(343395, 4960120))+ # limits of entire map+
  geom_rect(aes(xmin = -2243138, xmax = -1907966, ymin = 1323684, ymax = 1843861), col = "red", fill = NA)+ # Pacific inset outline
  geom_rect(aes(xmin = 2173445, xmax = 3097367, ymin = 911226, ymax = 2161814), col = "red", fill = NA)+ # Atlantic inset outline
  ggtitle("(A) Canada")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.title=element_text(size=10))


pacMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", alpha = 0.7, linewidth = 0.1)+
  #geom_sf(data = canada_map, colour = "black", fill = "grey92", linewidth = 0.1)+
  geom_sf(data = pacPunctualUTM[1,], pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # outline square for Lemmens (why did this work?)
  theme_bw()+
  ggtitle("(B) Pacific")+
  coord_sf(crs = can.lcc, xlim = c(-2243138, -1907966), ylim = c(1323684, 1843861))+ # limits for Pacific inset outline

  scale_fill_manual(values=cols)+
  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")

atlMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", alpha = 0.7, linewidth = 0.1)+
 # geom_sf(data = canada_map, colour = "black", fill = "grey92", linewidth = 0.1)+
  # Filter for each bay. Then only plot first value from that (otherwise all will plot)
  # Probably should have added as geom_rect, but instead I'm adding as empty squares (my original method)
  geom_sf(data = marTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # stroke changes width of square outline
  geom_sf(data = marPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  geom_sf(data = gulfTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  geom_sf(data = nlPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  theme_bw()+
  coord_sf(crs = can.lcc, xlim = c(2263445, 3057367), ylim = c(911226, 2161814))+

  scale_fill_manual(values=cols)+
  ggtitle("(C) Atlantic")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")








################################################################################
## PACIFIC 

ggPacMap = ggplot()+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_sf(data = pacPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(726115, 730885), ylim = c(5453319, 5458079), crs = 32609)+
  # There might be a better way to do this but reduce the # of axis ticks:

  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  ggtitle("(D) Lemmens")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
        panel.grid = element_blank())
  
################################################################################
## MARITIMES


# Argyle                   
ggArgMap = ggplot()+
  geom_polygon(argyleCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(259213, 267918), ylim = c(4846335, 4855031), crs = 32620)+
  #coord_sf(xlim = c(-65.98, -65.897), ylim = c(43.73290, 43.80595), crs = 4326)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.8, "cm"))+
  ggtitle("(E) Argyle")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Sober Island
ggSobMap = ggplot()+
  geom_polygon(soberCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.70, linewidth = 2.5, lineend = "round")+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(541154, 542825), ylim = c(4964661, 4966410), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  ggtitle("(F) Sober Island")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Country Harbour                   
ggChMap = ggplot()+
  geom_polygon(countryCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(596383, 611074), ylim = c(4996642, 5011378), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  ggtitle("(G) Country Harbour")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Whitehead
ggWhMap = ggplot()+
  geom_polygon(whiteheadCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(641669, 647881), ylim = c(5013443, 5019630), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(4.1, "cm"))+
  ggtitle("(H) Whitehead")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())


################################################################################
## GULF

# Cocagne
ggCocMap = 
  ggplot()+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(373453, 382190), ylim = c(5131250, 5140014), crs = 32620)+
    annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.7, "cm"))+
    ggtitle("(I) Cocagne")+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())

# Malpeque
ggMalMap = 
  ggplot()+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
    coord_sf(xlim = c(431139, 450611), ylim = c(5147976, 5167271), crs = 32620)+
    annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
    ggtitle("(J) Malpeque")+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())

# St. Peters
ggStPMap = 
  ggplot()+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    #geom_segment(gulfMeta %>% filter(facilityName == "StPeters"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "lightpink", alpha = 0.6, linewidth = 4, lineend = "round")+
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
  
  # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(519487, 532582), ylim = c(5136283, 5149220), crs = 32620)+
    annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.7, "cm"))+
    ggtitle("(K) St. Peters")+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())

################################################################################
## Newfoundland 

# Southeast Arm (what name should I be using for this?)
ggSeArmMap = 
  ggplot()+
    geom_polygon(seArmCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    #geom_point(nlMeta %>% filter(facilityName == "Southeast Arm"), mapping = aes(x = longitude, y = latitude), col = "lightpink", alpha = 0.6)+
    geom_sf(data = nlPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+
  
  # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(618843, 623646), ylim = c(5464557, 5469483), crs = 32621)+
    annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
    ggtitle("(L) Southeast Arm")+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())


################################################################################






studyMaps = ggarrange(ggPacMap, ggArgMap, ggSobMap, ggChMap, ggWhMap, ggCocMap, ggMalMap, ggStPMap, ggSeArmMap, ncol = 3, nrow = 3)


(canMap | pacMap | atlMap) /
  (studyMaps) +
  plot_layout(heights = (c(0.25, 0.75)))







ggplot()+
  geom_polygon(dfoRegions, mapping = aes(x = long, y = lat, group=group, fill = id, col = piece))
  

