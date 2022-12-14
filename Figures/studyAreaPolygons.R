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
argyleCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/argyleCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
countryCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/countryCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
whiteheadCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/whiteheadCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
soberCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/soberCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))

# Gulf coastline
gulfCoastline = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/gulfCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))

# Newfoundland coastline
seArmCoastline = fortify(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/newfoundlandCoastline.shp"))

# Pacific coastline
lemmensCoastline = fortify(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/lemmensCoastline.shp"))

## Lease shapefiles
nsLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/NS_leases_Apr_2022.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
nbLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/MASMPS_Data.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))
peiLeases = fortify(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/PEI_leases_March_2020.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))


# TBH it probably should go in a function but I'm going to make individual plots first
bayMaps = function(coastlineDf, regionMeta, latLimits, lonLimits){
  ggplot()+
    geom_polygon(coastlineDf, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_point(regionMeta, mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(xlim = lonLimits, ylim = latLimits, crs = 4326, breaks = scales::pretty_breaks(n=4))+
    annotation_scale(location = "br", text_cex = 1)+
    theme_bw()+
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"))
}


pacBay = bayMaps(lemmensCoastline, pacMeta, c(49.15, 49.24), c(-125.9472, -125.82))

################################################################################
## PACIFIC 

ggPacMap = ggplot()+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_point(pacMeta, mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+

  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-125.9472, -125.82), ylim = c(49.15, 49.24), crs = 4326)+
  # There might be a better way to do this but reduce the # of axis ticks:
  scale_y_continuous(label = c(49.15, 49.19, 49.23), breaks = seq(from = 49.15, to = 49.24, 0.04))+
  scale_x_continuous(label = c(-125.95, -125.90, -125.85), breaks = seq(from = -125.95, to = -125.82, 0.05))+
  annotation_scale(location = "br", text_cex = 1)+
  ggtitle("Lemmens")+
  theme_bw()+
  theme(
        axis.text = element_text(size = 10),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"))
  
################################################################################
## MARITIMES


# Argyle                   
ggArgMap = ggplot()+
  geom_polygon(argyleCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  # geom_point(marMeta %>% filter(facilityName == "Argyle"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  geom_segment(marMeta %>% filter(facilityName == "Argyle"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "darkgreen", alpha = 0.6, linewidth = 4, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-66.00, -65.89), ylim = c(43.73, 43.82), crs = 4326)+

  annotation_scale(location = "br", text_cex = 1)+
  ggtitle("Argyle")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #axis.text = element_text(size = 10),
    #axis.title = element_blank(),
    panel.grid = element_blank())

# Country Harbour                   
ggChMap = ggplot()+
  geom_polygon(countryCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_point(marMeta %>% filter(facilityName == "Country Harbour"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-61.5, -61.8), ylim = c(45.08, 45.25), crs = 4326)+
  annotation_scale(location = "br", text_cex = 1)+
  ggtitle("Country Harbour")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Whitehead
ggWhMap = ggplot()+
  geom_polygon(whiteheadCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  geom_point(marMeta %>% filter(facilityName == "WhiteHead"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "#C77CFF", size = 7, alpha = 0.6)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-61.2, -61.14), ylim = c(45.25, 45.315), crs = 4326)+
  annotation_scale(location = "br", text_cex = 1)+
  ggtitle("Whitehead")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Sober Island
ggSobMap = ggplot()+
  geom_polygon(soberCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  #geom_point(marMeta %>% filter(facilityName == "Sober Island Oyster"), mapping = aes(x = longitude, y = latitude), pch = 21, col = "black", fill = "darkgreen", size = 7, alpha = 0.6)+
  geom_segment(marMeta %>% filter(facilityName == "Sober Island Oyster"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "mediumspringgreen", alpha = 0.6, linewidth = 4, lineend = "round")+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-62.48, -62.457), ylim = c(44.835, 44.85), expand = F, crs = 4326)+
  #coord_map()
  #st_crop(soberCoastline, xmin = -62.490, xmax = -62.470, ymin = 44.832, ymax = 44.85)+
  annotation_scale(location = "br", text_cex = 1)+
  ggtitle("Sober Island")+
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
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
  
      geom_segment(gulfMeta %>% filter(facilityName == "Cocagne"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "red4", alpha = 0.6, linewidth = 4, lineend = "round")+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(ylim = c(46.3, 46.41), xlim = c(-64.54, -64.66), expand = F, crs = 4326)+
  
    #coord_sf(xlim = c(-62.48, -62.457), ylim = c(44.835, 44.85), expand = F, crs = 4326)+
    #coord_map()
    #st_crop(soberCoastline, xmin = -62.490, xmax = -62.470, ymin = 44.832, ymax = 44.85)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("Cocagne")+
    theme_bw()+
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())

# Malpeque
ggMalMap = 
  ggplot()+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "red")+
    geom_segment(gulfMeta %>% filter(facilityName == "Malpeque"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "red2", alpha = 0.6, linewidth = 4, lineend = "round")+
  
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    #coord_sf(ylim = c(46.2, 46.6), xlim = c(-64.3, -64.4), crs = 4326)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("Malpeque")+
    theme_bw()+
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())

# St. Peters
ggStPMap = 
  ggplot()+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_segment(gulfMeta %>% filter(facilityName == "StPeters"), mapping = aes(x = longitude, xend = longitudeEnd, y = latitude, yend = latitudeEnd), col = "lightpink", alpha = 0.6, linewidth = 4, lineend = "round")+
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "black")+
  # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    #coord_sf(ylim = c(46.2, 46.6), xlim = c(-64.3, -64.4), crs = 4326)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("Malpeque")+
    theme_bw()+
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())


################################################################################
## Newfoundland 

# Southeast Arm (what name should I be using for this?)
ggSeArmMap = 
  ggplot()+
    geom_polygon(seArmCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
    geom_point(nlMeta %>% filter(facilityName == "Southeast Arm"), mapping = aes(x = longitude, y = latitude), col = "lightpink", alpha = 0.6)+
    # Use this instead of coord_map to get the scalebar thing to work. 
    # annotation_scale needs the crs to be set here too
    coord_sf(crs = 4326)+
    annotation_scale(location = "br", text_cex = 1)+
    ggtitle("Malpeque")+
    theme_bw()+
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank())




ggarrange(ggSobMap, ggSobMap, ggCh, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap)

plot_grid(ggSobMap, ggSobMap, ggSobMap)


ggarrange(ggPac, ggArg, ggwh, ggPac, ggCh, ggArg, ggCh, ggSobMap, ggPac, ncol = 3, nrow = 3)




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

canMap = 
ggplot()+
  geom_sf(data = canada_map, colour = "black", fill = "grey92")+
  theme_bw()+
  #theme(panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(crs = can.lcc)


ggarrange(canMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ggSobMap, ncol = 3, nrow = 4, widths = c(2,1))


ggarrange(
  ggarrange(canMap, canMap, widths = c(2, 1)),
  ggarrange(ggSobMap, ggSobMap, ggSobMap), 
          nrow = 2, 
          heights = c(2, 1)

) 


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
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)),
          nrow = 4, 
          heights = c(1.5, 1, 1, 1)
) 

ggarrange(
    ggarrange(canMap, ggSobMap, ncol = 2, align = "h", widths = c(2,1), heights = c(2,1)),
          ggarrange(ggSobMap, ggArg, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)), 
          ggarrange(ggSobMap, ggSobMap, ggSobMap, ncol = 3, align = "h",widths = c(1,1,1)),
          nrow = 4, 
          heights = c(2, 1, 1, 1), align = "v"
) 


#####
# GARBAGE

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


tidy(sp::spTransform(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/soberCoastline.shp"), sp::CRS("+proj=longlat +datum=WGS84 +no_dfs")))


regions = tidy(readOGR("C:/Users/FINNISS/Desktop/AMPDataFiles/shapefiles/dfoRegions.shp"))
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
  
ggplot()+
  geom_polygon(regions, mapping = aes(x = long, y = lat, group=group, fill = id), col = "black")+
  theme_bw()+
  scale_fill_manual(values = c("#00BFC4", "#C77CFF", "grey90", "gray90", "gray90", "gray90", "#7CAE00", "#F8766D"))










