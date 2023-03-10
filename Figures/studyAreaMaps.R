################################################################################
################################################################################
#### STUDY AREA MAPS

# The map will have 11 panels total:
# Top (3 panels): (1) map of Canada & DFO regions, (2) zoomed in view of Pacific, (3) zoomed in View of Atlantic
# Then next 3 panels are the 9 sampling bays, which show the sampling stations and shellfish leases

# Some labels (e.g., "Inner", "Mid", "Outer") will be labelled by hand in PowerPoint or other software. 
# It is too much work to set all the exact coordinates for each label, especially since things are still in draft mode and may 
# be removed.

# Note that the 9 bays should have approximately the same size dimensions (square)
# The Pacific and Atlantic maps are also roughly the same relative dimensions

# Data were prepared in a different script. Originally I had it all here, but it was too much

# Making this figure was truly a nightmare, so if you're trying to copy this, good luck!

################################################################################
################################################################################

# Set up
source("DataProcessing/studyAreaDataPrep.R") # preps all the data for plotting
source("Figures/colourPchSchemes.R") # defines colour schemes for the region map

################################################################################
# Create the top panel of my maps
# One with all of Canada (canMap)
# One zoomed in on Pacific sites 
# One zoomed in on Atlantic sites
# I mostly just included the Pac/Atl maps because there was extra space I needed to fill
# I couldn't show the sampled bays well enough with just one map of all of Canada


# Colour scheme is defined in colourPchSchemes.R
# Note that transparency of alpha = 0.7 was given to my regions of interest (Pac, Mar, Gulf, NL) so they are less aggressive
# All others were "gray92", my standard colour for land

# Map of Canada
# Shows all of Canada. DFO regions I am analyzing are coloured. All other regions are in grey (gray92)
# Also has a red outline around the insets that focus in more on the Pacific and Atlantic areas so it's easier to see the sampling bays
# can_map needs to be added over top of the regions so we can see the outlines of each province
# I thought I could add it below and adjust transparency of dfoRegions.df but something weird is happening and I think the data are stacking 
# Multiple areas on top of each other, so the grey doesn't look grey. Parts look a lot darker.
canMap = 
  ggplot()+
  geom_sf(data = canada_map, fill = "gray92", col = "black", linewidth = 0.1)+ # IF I JUST WANT CANADA OUTLINE (no DFO regions)
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
  # Adjust the names of the legend items so there's enough space on the map for them
  scale_fill_manual(values=regionMapCols, labels = c("Gulf", "Mar", "Nfld", "Other", "Pac"))+
  geom_sf(data = canada_map, linewidth = 0.01, fill = NA, col = "black")+ # this shows the provinces. Make the linewidth very small
  # Note that if an sf object is not plotted first, you need to use coord_sf to situate everything
  # This will add graticules to the map. It also makes sure the scale bar is in correct units (but N/A for this map- no scale bar)
  coord_sf(crs = can.lcc, xlim = c(-2414929, 3168870), ylim = c(343395, 4960120))+ # limits of entire map. In Lambert Conformal Conic (defined in data prep)
  geom_rect(aes(xmin = -2243138, xmax = -1907966, ymin = 1323684, ymax = 1843861), col = "red", fill = NA)+ # Pacific inset outline
  geom_rect(aes(xmin = 2173445, xmax = 3097367, ymin = 911226, ymax = 2161814), col = "red", fill = NA)+ # Atlantic inset outline
  ggtitle("(A) Canada")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
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
  ) +
  # Set legend to have 3 columns
  guides(fill = guide_legend(ncol = 3))

# The Pacific and Atlantic inset maps need to be approx the same size (relative height/width) to be combined together properly
# I eyeballed this in QGIS to make sure each map was roughly the same height/width on the screen. Then I wrote down the coordinates.
 
# Pacific inset
pacMap = 
  ggplot()+
  geom_sf(data = canada_map, fill = "gray92", col = "black", linewidth = 0.1)+ # IF I JUST WANT CANADA OUTLINE (no DFO regions)
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
  geom_sf(data = pacPunctualUTM[1,], pch = 22, col = "red", fill = "red", size = 3)+ # outline square for Lemmens (why did this work?)
  # geom_sf(data = pacPunctualUTM[1,], pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # outline square for Lemmens (why did this work?)
  theme_bw()+
  ggtitle("(B) Pacific")+
  coord_sf(crs = can.lcc, xlim = c(-2243138, -1907966), ylim = c(1323684, 1843861))+ # set map extents and define coordinate system
  scale_fill_manual(values = regionMapCols)+ # Make this map purple
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")

# Atlantic inset
atlMap = 
  ggplot()+
  geom_sf(data = canada_map, fill = "gray92", col = "black", linewidth = 0.1)+ # IF I JUST WANT CANADA OUTLINE (no DFO regions)
  geom_polygon(dfoRegions.df, mapping = aes(x = long, y = lat, group=group, fill = Region_EN), col = "black", linewidth = 0.1)+
  # Add markers (red outlines) for each of the bays that were sampled
  # "Tr" refers to data that were transects. "Pun" is "punctual stations" (discrete locations)
  # This only plots one point per region (the first one). 
  # Could have added as geom_rect, but this might be better if I want them filled
  # geom_sf(data = marTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ # stroke changes width of square outline
  # geom_sf(data = marPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  # geom_sf(data = gulfTrMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+
  # geom_sf(data = nlPunMap, pch = 22, col = "red", fill = NA, size = 3, stroke = 0.7)+ 
  # Used filled squares if displaying DFO regions so they show up better
  geom_sf(data = marTrMap, pch = 22, col = "red", fill = "red", size = 3)+
  geom_sf(data = marPunMap, pch = 22, col = "red", fill = "red", size = 3)+
  geom_sf(data = gulfTrMap, pch = 22, col = "red", fill = "red", size = 3)+
  geom_sf(data = nlPunMap, pch = 22, col = "red", fill = "red", size = 3)+
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
################################################################################
##### Maps of individual bays

## A note about the size of each plot:
# I want these all to be squares. Otherwise, they don't line up properly when putting them all together
# If I use ggarrange from egg package to line them up, this MOSTLY isn't an issue. It lines them up.
# But that can distort the maps slightly. So when selecting xlim/ylim in coord_sf command, each axis should be approx the same
# I could adjust them to be EXACTLY the same length, but that's too much work! 
# To get ~square coordinates, I drew out a perfect square on my screen in QGIS, and wrote down the coordinates at these limits

## A note about the stupid scalebar:
# dding the scalebar with annotation_scale is very annoying! If you add it to the bottom right "location = "br", the text 
# (distance) is on the left hand side of the scalebar. But I want it on the right, or it appears over top of coastline and it's difficult
# to read. I didn't see/couldn't figure out if there was an easy way to move it.
# Instead, if you set location = "bl" (bottom left) the text is on the left hand side of the scalebar
# I then used pad_x to push the scalebar over to the right of the map.
# Note these are relative units and the size of my plotting window has to be the same each time or the scalebar might get pushed out of the plot

################################################################################
## PACIFIC 

# Lemmens 
ggLemMap = ggplot()+
  geom_polygon(lemmensBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
  geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(pacLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+ # add shellfish leases
  geom_sf(data = pacPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+ # add sampling locations
  # crs = 32609 is UTM zone 9
  coord_sf(xlim = c(726115, 730885), ylim = c(5453319, 5458079), crs = 32609)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+ # see note above about scale bar
  # Add a north arrow to Lemmens only in the top right
  annotation_north_arrow(location = "tr", which_north = "grid",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)+
  ggtitle("(D) Lemmens")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
        panel.grid = element_blank())

# Question for Terri Sutherland
# ggLemMap = ggplot()+
#   geom_polygon(lemmensCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
#   geom_polygon(pacLeases, mapping = aes(x = long, y= lat, group = group, fill = TENURE_STA), col = "black", linewidth = 0.1)+
#   scale_fill_discrete(name = "TENURE_STATUS")+ # add shellfish leases
#   geom_sf(data = pacPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+ # add sampling locations
#   # crs = 32609 is UTM zone 9
#   coord_sf(xlim = c(726115, 730885), ylim = c(5453319, 5458079), crs = 32609)+
#   annotation_scale(location = "br", text_cex = 0.8)+ # see note above about scale bar
#   ggtitle("Lemmens")+
#   theme_bw()+
#   theme(
#     #axis.text = element_blank(),
#     #axis.ticks = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank())
  
################################################################################
## MARITIMES
# These were all UTM Zone 20N ie crs = 32620

# Argyle                   
ggArgMap = ggplot()+
  geom_polygon(argBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
  geom_polygon(argyleCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  
  # The coastline layer wasn't detailed enough for Eel Lake, where there were some shellfish farms.
  # Add the eelLakeBoundary file in white first, otherwise it'll overlay on grey and this will look incorrect when alpha is set to 0.1
  # Then, add the eelLakeBoundary in blue with the alpha set to 0.1
  geom_polygon(eelLakeBoundary, mapping = aes(x = long, y = lat, group=group), fill = "white", col = NA, linewidth = 0.1)+
  geom_polygon(eelLakeBoundary, mapping = aes(x = long, y = lat, group=group), fill = "blue", col = "black", linewidth = 0.1, alpha = 0.1)+
  # Then, add the islands
  geom_polygon(eelLakeIslands, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+ # add shellfish leases
  # Data are transects. In theory, they could be plotted as lines using geom_sf
  # However, idk how to get the data into the right format for that lol. So I am using geom_segment to connect between start/end coordinates of the tow
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
  #coord_sf(xlim = c(259213, 267918), ylim = c(4846335, 4855031), crs = 32620)+ # UTM zone 20
  coord_sf(xlim = c(255400, 280573), ylim = c(4838335, 4863182), crs = 32620)+ # UTM zone 20
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.8, "cm"))+
  # # Add a north arrow to map
  # annotation_north_arrow(location = "tr", which_north = "true",
  #                        height = unit(1, "cm"), width = unit(1, "cm"),
  #                        style = north_arrow_fancy_orienteering)+
  ggtitle("(E) Argyle")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Sober Island
ggSobMap = ggplot()+
  geom_polygon(soberBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
  geom_polygon(soberCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
  geom_segment(marTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.70, linewidth = 2.5, lineend = "round")+
  coord_sf(xlim = c(541154, 542825), ylim = c(4964661, 4966410), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  # # Add a north arrow to map
  # annotation_north_arrow(location = "tr", which_north = "true",
  #                        height = unit(1, "cm"), width = unit(1, "cm"),
  #                        style = north_arrow_fancy_orienteering)+
  ggtitle("(F) Sober Island")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Country Harbour                   
ggChMap = ggplot()+
  geom_polygon(countryBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
  geom_polygon(countryCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  # coord_sf(xlim = c(596383, 611074), ylim = c(4996642, 5011378), crs = 32620)+
  coord_sf(xlim = c(593384, 606378), ylim = c(5000120, 5011138), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  # Add a north arrow to map
  # annotation_north_arrow(location = "tr", which_north = "true",
  #                        height = unit(1, "cm"), width = unit(1, "cm"),
  #                        style = north_arrow_fancy_orienteering)+
  ggtitle("(G) Country Harbour")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Whitehead
ggWhMap = ggplot()+
  geom_polygon(whiteheadBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
  geom_polygon(whiteheadCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_polygon(nsLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
  geom_sf(data = marPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+  # Use this instead of coord_map to get the scalebar thing to work. 
  coord_sf(xlim = c(641669, 647881), ylim = c(5013443, 5019630), crs = 32620)+
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(4.1, "cm"))+
  # Add a north arrow to map
  # annotation_north_arrow(location = "tr", which_north = "true",
  #                        height = unit(1, "cm"), width = unit(1, "cm"),
  #                        style = north_arrow_fancy_orienteering)+
  ggtitle("(H) Whitehead")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

################################################################################
## GULF
# These are also all UTM Zone 20N i.e., crs = 32620

# Cocagne
ggCocMap = 
  ggplot()+
  geom_polygon(cocagneBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_polygon(nbLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
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
  geom_polygon(malpequeBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
    # coord_sf(xlim = c(431139, 450611), ylim = c(5147976, 5167271), crs = 32620)+
    coord_sf(xlim = c(423090, 451168), ylim = c(5142615, 5168319), crs = 32620)+
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
  geom_polygon(stPetersBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
    geom_polygon(gulfCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_polygon(peiLeases, mapping = aes(x = long, y= lat, group = group), fill = "pink", col = "#cc3d6f", linewidth = 0.1)+
    geom_segment(gulfTransectUTM, mapping = aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), col = "blue", alpha = 0.7, linewidth = 2.5, lineend = "round")+
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
  geom_polygon(seArmBoundary, mapping = aes(x = long, y= lat, group = group), fill = "blue", col = NA, linewidth = 0.1, alpha = 0.1)+
    geom_polygon(seArmCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
    geom_sf(data = lease1Sf, col = "#cc3d6f", fill = "pink")+
    geom_sf(data = lease2Sf, col = "#cc3d6f", fill = "pink")+
    geom_sf(data = nlPunctualUTM, pch = 21, col = "black", fill = "blue", size = 3, alpha = 0.7)+
    # coord_sf(xlim = c(618843, 623646), ylim = c(5464557, 5469483), crs = 32621)+ # UTM zone 21N
    coord_sf(xlim = c(618447, 626387), ylim = c(5462394, 5469127), crs = 32621)+
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


ggsave("test.png", width = 12.32, height = 12.07, units = "in", dpi = 300)

