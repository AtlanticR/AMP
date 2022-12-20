################################################################################
################################################################################
#### STUDY AREA MAPS

# For making my study area map with all the bays identified
# Includes sampling locations for each bay and shellfish leases

# The final map 





################################################################################
################################################################################

# Set up
source("Figures/colourPchSchemes.R")


# Colour scheme is defined in colourPchSchemes.R
# Note that transparency of alpha = 0.7 was given to my regions of interest (Pac, Mar, Gulf, NL) so they are less aggressive
# All others were "gray92", my standard colour for land

canMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
  scale_fill_manual(values=regionMapCols)+
  
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
    legend.position = "none")


pacMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
  #geom_sf(data = canada_map, colour = "black", fill = "grey92", linewidth = 0.1)+
  geom_sf(data = pacPunctualUTM[1,], pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # outline square for Lemmens (why did this work?)
  theme_bw()+
  ggtitle("(B) Pacific")+
  coord_sf(crs = can.lcc, xlim = c(-2243138, -1907966), ylim = c(1323684, 1843861))+ # limits for Pacific inset outline

  scale_fill_manual(values = regionMapCols)+
  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")

atlMap = 
  ggplot()+
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
 # geom_sf(data = canada_map, colour = "black", fill = "grey92", linewidth = 0.1)+
  # Filter for each bay. Then only plot first value from that (otherwise all will plot)
  # Probably should have added as geom_rect, but instead I'm adding as empty squares (my original method)
  geom_sf(data = marTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # stroke changes width of square outline
  geom_sf(data = marPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  geom_sf(data = gulfTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  geom_sf(data = nlPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  theme_bw()+
  coord_sf(crs = can.lcc, xlim = c(2263445, 3057367), ylim = c(911226, 2161814))+

  scale_fill_manual(values = regionMapCols)+
  ggtitle("(C) Atlantic")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")








################################################################################
## PACIFIC 

# Lemmens 
ggLemMap = ggplot()+
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
### Make the final map

# Just in case, the plotting window on my screen is:
# Height: 25.5
# Width: 25cm

# Arrange each study area map using the function from the egg package
# This makes each plot line up perfectly. But I think it squishes the maps slightly to match the longest (?) dimension of each ggplot object
# My maps are almost exactly square, so that's not too much of an issue
studyMaps = ggarrange(ggLemMap, ggArgMap, ggSobMap, ggChMap, ggWhMap, ggCocMap, ggMalMap, ggStPMap, ggSeArmMap, ncol = 3, nrow = 3)


# Combine inset maps with study area maps using the patchwork package
# studyMaps could instead be individual maps, but since they are not perfect squares, they are slightly unaligned
# All of this could probably be combined with the ggarrange package, but I wasn't sure how to combine everything together with the correct sizes
(canMap | pacMap | atlMap) /
  (studyMaps) +
  plot_layout(heights = (c(0.25, 0.75))) # top panel takes up 1/4 of plot. studyMaps make up 3/4



