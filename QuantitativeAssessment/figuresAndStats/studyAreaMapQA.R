################################################################################
################################################################################
#### STUDY AREA MAPS

# This is based on the study area maps created for the Tech Report
# However, shellfish leases are not displayed, and there are now only 3 sites:
# Lemmens, St. Peters and South Arm

################################################################################
################################################################################

# Set up
# Read in all the data that's already prepped
source("QuantitativeAssessment/dataProcessing/StudyAreaDataPrepQA.R") # preps all the data for plotting

################################################################################

# Map of Canada but also Greenland, USA, Russia
canMap = 
  ggplot()+
  # Add other countries
  geom_sf(data = countries_map, fill = "gray92", col = "black", linewidth = 0.1)+ # IF I JUST WANT CANADA OUTLINE (no DFO regions)
  # Add Canada
  geom_sf(data = canada_map, fill = "gray92", col = "black", linewidth = 0.09)+ # IF I JUST WANT CANADA OUTLINE (no DFO regions)
  geom_sf(data = nlPunctualUTM[1,], col = "red", size = 2.8, pch = 22, fill = "red")+ # Add red square over NL data (just plot first data point)
  geom_sf(data = pacPunctualUTM[1,], col = "red", size = 2.8, fill = "red", pch = 22)+ # Add red square over Pac data
  geom_sf(data = gulfTransectUTM[1,], col = "red", size = 2.8, pch = 22, fill = "red")+
    # Note that if an sf object is not plotted first, you need to use coord_sf to situate everything
  # This will add graticules to the map. It also makes sure the scale bar is in correct units (but N/A for this map- no scale bar)
  coord_sf(crs = can.lcc, xlim = c(-2414929, 3168870), ylim = c(343395, 4960120))+ # limits of entire map. In Lambert Conformal Conic (defined in data prep)
  ggtitle("(A) Canada")+
  theme_bw()+
  theme(
    axis.title = element_blank(),
    # Idk if I needed to do both steps below, but put a black outline around legend
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    # Put legend in bottom left of plot and have it align with the actual plot
    legend.justification = c(0,0),
    legend.position = c(0,0),
    # Make the size of the legend elements smaller
    legend.key.size = unit(0.2, "cm"),
    # Remove legend title
    legend.title = element_blank()
  ) 

################################################################################
# PACIFIC 

# Lemmens 
ggLemMap = ggplot()+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  # geom_polygon(pacLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+ # add shellfish leases
  geom_sf(data = pacPunctualUTM, pch = 21, col = "black", fill = "#F8766D", size = 5, alpha = 0.7)+ # add sampling locations
  # crs = 32609 is UTM zone 9
  coord_sf(xlim = c(726115, 730885), ylim = c(5453319, 5458079), crs = 32609)+
  annotation_scale(location = "br", text_cex = 0.8)+
  #annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+ # see note above about scale bar
  # Add a north arrow to Lemmens only in the top right
  annotation_north_arrow(location = "tr", which_north = "grid",
                         height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_minimal)+
  # manually specify the x and y axes label so they don't get crowded. Here, longitudes should be negative
  scale_x_discrete(breaks = c(-125.89, -125.87, -125.85, -125.83))+
  scale_y_discrete(breaks = c(49.19, 49.21, 49.23))+
  ggtitle("(B) Lemmens")+
  theme_bw()+
  theme(
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

################################################################################
## GULF

# St. Peters
ggStPMap = 
  ggplot()+
  geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  # geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
  geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "#F8766D", alpha = 0.7, linewidth = 2.7, lineend = "round")+
  coord_sf(xlim = c(519487, 532582), ylim = c(5136283, 5149220), crs = 32620)+
  annotation_scale(location = "br", text_cex = 0.8)+
  # annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.7, "cm"))+
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_minimal)+
  scale_y_discrete(breaks = c(46.38, 46.42, 46.46, 46.50))+
  ggtitle("(C) St. Peters")+
  theme_bw()+
  theme(
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

################################################################################
## NEWFOUNDLAND

# South Arm
ggSeArmMap = 
  ggplot()+
  geom_polygon(seArmCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  # geom_sf(data = lease1Sf, col = "#cc3d6f", fill = "pink")+
  # geom_sf(data = lease2Sf, col = "#cc3d6f", fill = "pink")+
  geom_sf(data = nlPunctualUTM, pch = 21, col = "black", fill = "#F8766D", size = 5, alpha = 0.7)+
  # coord_sf(xlim = c(618843, 623646), ylim = c(5464557, 5469483), crs = 32621)+ # UTM zone 21N
  coord_sf(xlim = c(618447, 626387), ylim = c(5462394, 5469827), crs = 32621)+
  annotation_scale(location = "br", text_cex = 0.8)+
  # Add a north arrow to map
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_minimal)+
  scale_x_discrete(breaks = c(-55.36, -55.32, -55.28))+
  scale_y_discrete(breaks = c(49.30, 49.33, 49.36))+
  ggtitle("(D) South Arm")+
  theme_bw()+
  theme(
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())


################################################################################
### Make the final map

# Arrange each study area map using the function from the egg package
studyMaps = ggarrange(ggLemMap, ggStPMap, ggSeArmMap, ncol = 3, nrow = 1)

# Combine inset maps with study area maps using the patchwork package
# studyMaps could instead be individual maps, but since they are not perfect squares, they are slightly unaligned
# All of this could probably be combined with the ggarrange package, but I wasn't sure how to combine everything together with the correct sizes
(canMap) /
  (studyMaps) +
  plot_layout(heights = (c(0.5, 0.5))) 

# Save it with these dimensions (through trial and error.. this looked good)
ggsave("test.png", width = 10.15, height = 7.14, units = "in", dpi = 300)

