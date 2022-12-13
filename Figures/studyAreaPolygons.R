# Clipping land to a bounding box!
# I definitely deleted a bunch of packages. But here's 

source("DataProcessing/rPackages.R")


lemmensCoastline = readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/FWApoly_reduced.shp")
#cocagneCoastline = tidy(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/cocagneCoastline.shp"))

# I am combining multiple steps because this is a LOT of data and I don't want to keep creating new variables at each step
# Read in the data, reproject it (WGS84), convert to a data frame for easier plotting with ggplot() (although this maybe isn't necessary?)
# Mapping in R seems to constantly be changing, so I hope this is still somewhat current.
# Maritimes data have to be transformed to WGS 84. I think the code is constantly changing lol ahhh
argyleCoastline = tidy(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/argyleCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
countryCoastline = tidy(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/countryCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
whiteheadCoastline = tidy(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/whiteheadCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))


bayMaps = function(coastlineDf, regionMeta, latLimits, lonLimits){
  
  ggplot()+
    geom_polygon(coastlineDf, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_point(regionMeta, mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = lonLimits, ylim = latLimits, crs = 4236)+
    annotation_scale(location = "br", text_cex = 1)+
    theme_bw()+
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"))
  
}


pacBay = bayMaps(lemmensCoastline, pacMeta, c(49.15, 49.24), c(-125.9472, -125.82))

argMet= marMeta %>% filter(facilityName == "Argyle")

argBay = bayMaps(argyleCoastline, argMet, c(43.73, 43.82), c(-66.00, -65.89))

grid.arrange(pacBay, argBay, ncol = 2)

ggarrange(pacBay, argBay, ncol = 2)

plot_grid(pacBay, argBay, ncol = 2)


# Pacific
ggplot()+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_point(pacMeta, mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = , ylim = , crs = 4236)+
  annotation_scale(location = "br", text_cex = 1)+
  theme_bw()+
  theme(
        axis.text = element_text(size = 14),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"))
  
# Argyle                   
ggplot()+
  geom_polygon(argyleCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_point(marMeta %>% filter(facilityName == "Argyle"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-66.00, -65.89), ylim = c(43.73, 43.82), crs = 4236)+
  annotation_scale(location = "br", text_cex = 1)+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Country Harbour                   
ggplot()+
  geom_polygon(countryCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_point(marMeta %>% filter(facilityName == "Country Harbour"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-61.5, -61.8), ylim = c(45.08, 45.25), crs = 4236)+
  annotation_scale(location = "br", text_cex = 1)+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid = element_blank())







#########################

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
devtools::install_github("ropensci/rnaturalearthhires")

library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")


canada_map <- ne_states(country = "canada", returnclass = "sf")
plot(canada_map)

# Define the Lambert Conformal Conic projection
# Taken from here: https://ahurford.github.io/quant-guide-all-courses/making-maps.html
can.lcc = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

ggplot()+
  geom_sf(data = canada_map, colour = "black", fill = "grey92")+
  theme_bw()+
  #theme(panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(crs = can.lcc)


map_nl <- canada_map[canada_map$name == "Newfoundland and Labrador",]


canada_again = ne_countries(country = "canada", returnclass = "sf", scale = "medium")


ggplot()+
  geom_sf(data = canada_again, colour = "black", fill = "grey92")+
  theme_bw()+
  coord_sf(crs = can.lcc)



ggplot()+
  geom_polygon(argCoastDf, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  theme_bw()

ggplot()+
  geom_polygon(counCoastDf, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  theme_bw()

ggplot()+
  geom_polygon(whitCoastDf, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  theme_bw()




regions = readOGR("C:/Users/FINNISS/Desktop/dfoRegions.shp")
d = fortify(regions, region = regions$data$region_EN)


atlColour = c("Gulf" = "#F8766D", 
              "Maritimes" = "#7CAE00", 
              "Newfoundland" = "#00BFC4")
# Pacific Ocean 
# Note this is the colour when only Pacific OCEAN data is displayed
# When Pacific (region) data is broken down by field season (below), 4 colours will be set
pacColourOne = c("Pacific" = "#C77CFF")



ggplot()+
  geom_polygon(d, mapping = aes(x = long, y = lat, group=group, fill = id), col = "black")+
  theme_bw()+
  scale_fill_manual(values = c("#00BFC4", "#C77CFF", "grey90", "gray90", "gray90", "gray90", "#7CAE00", "#F8766D"))
  

