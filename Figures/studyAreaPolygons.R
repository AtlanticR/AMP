# Clipping land to a bounding box!
# I definitely deleted a bunch of packages. But here's 

source("DataProcessing/rPackages.R")


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
## PACIFIC 

ggPacMap = ggplot()+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_sf(data = pacPunctualUTM, pch = 21, col = "black", fill = "#C77CFF", size = 5, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(726115, 730885), ylim = c(5453319, 5458079), crs = 32609)+
  # There might be a better way to do this but reduce the # of axis ticks:

  annotation_scale(location = "br", text_cex = 1)+
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
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "darkgreen", alpha = 0.6, linewidth = 3, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(259213, 267918), ylim = c(4846335, 4855031), crs = 32620)+
  #coord_sf(xlim = c(-65.98, -65.897), ylim = c(43.73290, 43.80595), crs = 4326)+
  annotation_scale(location = "br", text_cex = 1)+
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
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "mediumspringgreen", alpha = 0.6, linewidth = 3, lineend = "round")+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(541154, 542825), ylim = c(4964661, 4966410), crs = 32620)+
  annotation_scale(location = "br", text_cex = 1)+
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
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "green3", size = 5, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(596383, 611074), ylim = c(4996642, 5011378), crs = 32620)+
  annotation_scale(location = "br", text_cex = 1)+
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
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "mediumspringgreen", size = 5, alpha = 0.6)+  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(641669, 647881), ylim = c(5013443, 5019630), crs = 32620)+
  annotation_scale(location = "br", text_cex = 1)+
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
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "red4", alpha = 0.6, linewidth = 3, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(373453, 382190), ylim = c(5131250, 5140014), crs = 32620)+
    annotation_scale(location = "br", text_cex = 1)+
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
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "red2", alpha = 0.6, linewidth = 3, lineend = "round")+
    coord_sf(xlim = c(431139, 450611), ylim = c(5147976, 5167271), crs = 32620)+
    annotation_scale(location = "br", text_cex = 1)+
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
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "lightpink", alpha = 0.6, linewidth = 3, lineend = "round")+
  
  # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(519487, 532582), ylim = c(5136283, 5149220), crs = 32620)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("(K) Malpeque")+
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
    geom_sf(data = nlPunctualUTM, pch = 21, col = "black", fill = "lightpink", size = 5, alpha = 0.6)+
  
  # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = c(618843, 623646), ylim = c(5464557, 5469483), crs = 32621)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("(L) Southeast Arm")+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())


################################################################################

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
devtools::install_github("ropensci/rnaturalearthhires")

library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")

# devtools::install_github("yutannihilation/ggsflabel")
# library("ggsflabel")


# Get data for Canada
canada_map = ne_states(country = "canada", returnclass = "sf")
# Test what maps look like with more countries (didn't use)
countries_map = ne_states(country = c("United States of America","Canada","Denmark","Greenland","Iceland","Russia","Japan"), returnclass = "sf")

# Define the Lambert Conformal Conic projection
can.lcc = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

canMap = 
  ggplot()+
  geom_sf(data = canada_map, colour = "black", fill = "grey92")+
  theme_bw()+
  ggtitle("(A) Canada")+
  geom_rect(aes(xmin = -2243138, xmax = -1907966, ymin = 1323684, ymax = 1843861), col = "red", fill = NA)+ # Pacific inset
  geom_text(data = canada_map, aes(x = mean(c(-2243138, -1907966)), y=1323684-200000), label="B", col = "red", size = 5)+ # label for Pacific
  geom_rect(aes(xmin = 2173445, xmax = 3097367, ymin = 911226, ymax = 2161814), col = "red", fill = NA)+ # Atlantic inset
  geom_text(data = canada_map, aes(x = mean(c(2173445, 3097367)), y=907226-200000), label="C", col = "red", size = 5)+ # label for Atlantic
  #theme(panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(crs = can.lcc, xlim = c(-2414929, 3168870), ylim = c(343395, 4960120))+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank())



pacMap = 
  ggplot()+
    geom_sf(data = canada_map, colour = "black", fill = "grey92")+
    geom_sf(data = pacPunctualUTM[1,], pch = 21, col = "black", fill = "red", size = 5)+ # label for Pacific
    #ggsflabel::geom_sf_text_repel(data = pacPunctualUTM[1,], aes(label = "HI"), colour = "red", size = 5)+
    theme_bw()+
    ggtitle("(B) Pacific")+
    coord_sf(crs = can.lcc, xlim = c(-2243138, -1907966), ylim = c(1323684, 1843861))+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())


atlMap = 
  ggplot()+
    geom_sf(data = canada_map, colour = "black", fill = "grey92")+
    # Filter for each bay. Then only plot first value from that (otherwise all will plot)
    geom_sf(data = marTransectUTM %>%
              filter(facilityName == "Argyle") %>%
              filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+ # circle for Argyle
    geom_sf(data = marPunctualUTM %>%
              filter(facilityName == "Country Harbour") %>%
              filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+ # circle for CH
    geom_sf(data = marTransectUTM %>%
              filter(facilityName == "Sober Island Oyster") %>%
              filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+ # circle for Sober Island
    geom_sf(data = marPunctualUTM %>%
              filter(facilityName == "WhiteHead") %>%
              filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+ # circle for Whitehead
    geom_sf(data = gulfTransectUTM %>%
            filter(facilityName == "Cocagne") %>%
            filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+  
    geom_sf(data = gulfTransectUTM %>%
            filter(facilityName == "Malpeque") %>%
            filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+ 
    geom_sf(data = gulfTransectUTM %>%
            filter(facilityName == "StPeters") %>%
            filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+
    geom_sf(data = nlPunctualUTM %>%
            filter(row_number() == 1), pch = 21, col = "black", fill = "red", size = 5)+
    theme_bw()+
    coord_sf(crs = can.lcc, xlim = c(2263445, 3057367), ylim = c(911226, 2161814))+
    ggtitle("(C) Atlantic")+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())

studyMaps = ggarrange(ggPacMap, ggArgMap, ggSobMap, ggChMap, ggWhMap, ggCocMap, ggMalMap, ggStPMap, ggSeArmMap, ncol = 3, nrow = 3)

x = marTransectUTM[1,] %>% filter(facilityName == "Sober Island Oyster")

(canMap | pacMap | atlMap) /
  (studyMaps) +
  plot_layout(heights = (c(0.25, 0.75)))


 


library(ggpubr)

data("ToothGrowth")
data("mtcars")

P1 <- ggplot(mtcars, aes(x = wt, y = mpg, color=cyl))+
  geom_point()       # Add correlation coefficient

P2 <- ggboxplot(ToothGrowth, x = "dose", y = "len",
                color = "dose", palette = "jco")+
  scale_y_continuous(breaks=c(10.5, 20.5, 30.5))

P3 <- ggdotplot(ToothGrowth, x = "dose", y = "len",
                color = "dose", palette = "jco", binwidth = 1) +
  scale_y_continuous(breaks=c(10.5, 20.5, 30.5))


ggarrange(P1,
          ggarrange(P2, P2, ncol = 2, labels = c("b", "d"), align = "h",widths = c(1.5,2)), 
          ggarrange(P3, P3, ncol = 2, labels = c("c", "e"), align = "h",widths = c(1.5,2)), 
          nrow = 3, 
          heights = c(1.5, 1, 1),
          labels = "a" 
) 


ggarrange(P1,
          ggarrange(P2, P2, P2, ncol = 3, labels = c("b", "d", "i"), align = "h",widths = c(1,1,1)), 
          ggarrange(P3, P3, P3, ncol = 3, labels = c("c", "e", "i"), align = "h",widths = c(1,1,1)), 
          ggarrange(P3, P3, P3, ncol = 3, labels = c("c", "e", "i"), align = "h",widths = c(1,1,1)),
          nrow = 4, 
          heights = c(1.5, 1, 1, 1),
          labels = "a" 
) 


ggarrange(canMap,
          ggarrange(ggArgMap, ggSobMap, ggChMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggWhMap, ggCocMap, ggMalMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggStPMap, ggSeArmMap, ggPacMap, ncol = 3, align = "h",widths = c(1,1,1)),
          nrow = 4, 
          heights = c(1.5, 1, 1, 1)
) 



ggarrange(
    ggarrange(canMap, atlMap, pacMap, ncol = 3, align = "h", widths = c(2,1), heights = c(2,1)),
          ggarrange(ggSobMap, ggArg, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)),
          nrow = 4, 
          heights = c(2, 1, 1, 1), align = "v"
) 










