################################################################################
################################################################################
### NMDS Ordinations for regional FlowCam data
# But in this script I'm going to add symbols to distinguish each plot

# This makes NMDS ordinations when data are displayed by REGIONS
# Likely, not all plots will be used in Tech Report
# There is code to make ordinations for:
# All regions together, just the Atlantic Ocean, and each region separately
# (with data by bays identified)

################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("Figures/colourPchSchemes.R")

#################################################################################
#################################################################################
## Create NMDS for all data
# Including Pacific and Atlantic data

# Plot is a bit more complicated because I need 2 legend items: Atlantic Ocean (and DFO regions listed underneath)
# and Pacific Ocean (with DFO region listed underneath)
# This has to be done by making 3 ggplots to:
# 1. Get the legend (only, not the actual plot) from Atlantic data
# 2. Get the Legend (only) from Pacific data
# 3. Plot (with no legend) of both Pacific and Atlantic data
# All three will then be combined with grid.arrange()


# This will display each DFO region (Gulf, Maritimes, Newfoundland, Pacific) with a different colour
# Possibly different symbols for each ocean (Pacific, Atlantic). TBD how I handle this.

# Combine all the data together
allRegions = rbind(marMerge, nlMerge, pacMerge, gulfMerge)

# Convert it to wide format
allRegionsWide = allRegions %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

# For NMDS calculations, must only include species data from dataframe
# I will constantly be removing columns, adding columns etc. 
# Instead define as the index where there's Acartia species (first species colum in dataframe) to the end (final column)
beginNMDSAll = which(colnames(allRegionsWide)== "Acartia spp.")
endNMDSAll = ncol(allRegionsWide)

# Do NMDS ordination but only include species data
ordAll = metaMDS(sqrt(allRegionsWide[,c(beginNMDSAll:endNMDSAll)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
ordCoordsAll = as.data.frame(scores(ordAll, display="sites"))
# Add NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressAll = paste("2D Stress: ", format(round(ordAll$stress, digits=2), nsmall=2))

# Get the number of facets there should be (either # of bays, or # of sampling months (Pacific))
# create array for pch symbols. e.g., if 4 factors will give: 21, 22, 23, 24
oceanArray = c(21:(20+length(unique(allRegions$ocean)))) 
regionArray = c(21:(20+length(unique(allRegions$region)))) 

# Add region and ocean onto this data frame (it's easier for plotting in aes for ggplot)
ordCoordsAll = ordCoordsAll %>%
  mutate(region = allRegionsWide$region, ocean = allRegionsWide$ocean)


# Compute the group centroids
centOcean = aggregate(cbind(NMDS1, NMDS2)~ocean, data = ordCoordsAll, FUN = mean) # centroid of the oceans
centRegion = aggregate(cbind(NMDS1, NMDS2)~region, data =ordCoordsAll, FUN = mean) # centroid of the regions

# Add these centroids by merging with ordCoordsAll. Rename the centroids to 'oNMDS1' and 'oNMDS2' to represent NMDS coordinate centroids
segs = merge(ordCoordsAll, setNames(centOcean, c('ocean','oNMDS1','oNMDS2')),
              by = 'ocean', sort = FALSE)

# Now merge the region coordinates
segs2 = merge(segs, setNames(centRegion, c('region', 'rNMDS1', 'rNMDS2')),
              by = "region", sort = FALSE)

# Create plot for Atlantic
ggAtlantic = ggplot()+
  geom_point(data = ordCoordsAll %>% filter(ocean == "Atlantic"), aes(x = NMDS1, y = NMDS2, fill = region), pch = 21, size = 5)+
  scale_fill_manual(values = atlColour, name = "Atlantic Ocean")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

# Create plot for Pacific
ggPacific = ggplot()+
  geom_point(data = ordCoordsAll %>% filter(ocean == "Pacific"), aes(x = NMDS1, y = NMDS2, fill = region), pch = 22, size = 5)+
  scale_fill_manual(values = pacColourOne, name = "Pacific Ocean")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))


# Get the legend and then turn it into a grob
pacLegend = as_grob(get_legend(ggPacific))
atlLegend = as_grob(get_legend(ggAtlantic))

# Create plot with both data and no legend
ggBoth = 
  ggplot() + 
  #geom_segment(data = segs, mapping = aes(x = NMDS1, xend = oNMDS1, y = NMDS2, yend = oNMDS2), col = "grey49")+ # map segments for ocean
  geom_segment(data = segs2, mapping = aes(x = NMDS1, xend = rNMDS1, y = NMDS2, yend = rNMDS2), col = "grey49")+ # map segments for regions
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, pch = allRegionsWide$ocean, fill = allRegionsWide$region), alpha= 0.9, size = 6)+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_shape_manual(values = regionArray, name = "Region")+ 
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4, hjust=1)+
  ggtitle("Atlantic and Pacific")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

# Put everything together
# This gets me PRETTY CLOSE to the Figure that I want, except that the legend items aren't totally lined up
# Will just fix this in PowerPoint (for now...)
grid.arrange(ggBoth, pacLegend, atlLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,2),
                                   c(1,1,1,3),
                                   c(1,1,1,NA)))

#################################################################################
# NMDS of regions but "Ocean" is not specified as a heading for legend items
# Instead, each region is a different colour AND symbol

ggplot() + 
  #geom_segment(data = segs, mapping = aes(x = NMDS1, xend = oNMDS1, y = NMDS2, yend = oNMDS2), col = "grey49")+ # map segments for ocean
  geom_segment(data = segs2, mapping = aes(x = NMDS1, xend = rNMDS1, y = NMDS2, yend = rNMDS2), col = "grey49")+ # map segments for regions
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, fill = allRegionsWide$region, pch = allRegionsWide$region), alpha= 0.9, size = 6)+
  scale_shape_manual(values=c(21:24), name = "Region")+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_fill_discrete(name = "Region")+
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 12),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14),
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

#################################################################################
#################################################################################
## Now do the same thing for Atlantic ocean

# I need to create 3 legend items for:
# 1. Gulf as title (subpoints are Cocagne, Malpeque, StPeters)
# 2. Maritimes (Argyle, Country Harbour, Malpeque, Sober Island Oyster)
# 3. Newfoundland (rename as Southeast Arm)


# Get only the Atlantic Ocean data
atlOnly = allRegionsWide %>%
  filter(ocean == "Atlantic")

# Set the start/stop column indices for NMDS to run on
beginNMDSAtl = which(colnames(atlOnly)== "Acartia spp.")
endNMDSAtl = ncol(atlOnly)

# Do NMDS ordination
ordAtl = metaMDS(sqrt(atlOnly[,c(beginNMDSAtl:endNMDSAtl)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot and add back in certain columns (easier for aes commands)
ordCoordsAtl = as.data.frame(scores(ordAtl, display="sites")) %>%
  mutate(tidePhase = atlOnly$tidePhase) %>%
  mutate(facetFactor = atlOnly$facetFactor) %>%
  mutate(myLabel = atlOnly$myLabel) %>%
  mutate(region = atlOnly$region)

# GET NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressAtl = paste("2D Stress: ", format(round(ordAtl$stress, digits=2), nsmall=2))

# Gulf
ggGulf = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Gulf"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 6)+
  scale_fill_manual(values = c("red4", "red2", "lightpink"), name = "Gulf Region")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

# Maritimes
ggMaritimes = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Maritimes"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 22, size = 6)+
  scale_fill_manual(values = c("darkgreen", "green3", "darkolivegreen2", "mediumspringgreen"), name = "Maritimes Region")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

# Newfoundland
ggNewfoundland = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Newfoundland"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 24, size = 6)+
  scale_fill_manual(values = c("#00BFC4"), name = "Newfoundland", labels = "Southeast Arm")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

# Get (just) the legends
gulfLegend = as_grob(get_legend(ggGulf))
marLegend = as_grob(get_legend(ggMaritimes))
nlLegend = as_grob(get_legend(ggNewfoundland))

# Now make a plot of everything (without the legend)
ggAtlanticOnly = ggplot()+
  geom_point(data = ordCoordsAtl, aes(x = NMDS1, y = NMDS2, fill = facetFactor, pch = region), size = 6)+
  scale_shape_manual(values=c(21, 22, 24), name = "Bay")+
  # Note that fill is alphabetical by facetFactor. Use sort(unique(ordCoordsAtl$facetFactor)) to determine order
  scale_fill_manual(values = c("darkgreen", "red4", "green3", "red2", "darkolivegreen2", "#00BFC4", "lightpink", "mediumspringgreen"), name = "Region")+
  annotate("text", x = max(ordCoordsAtl$NMDS1), y=max(ordCoordsAtl$NMDS2), label = ordStressAtl, size=4, hjust=1)+
  ggtitle("Atlantic")+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

# Plot it all together
grid.arrange(ggAtlanticOnly, gulfLegend, marLegend, nlLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,2),
                                   c(1,1,1,3),
                                   c(1,1,1,4),
                                   c(1,1,1,NA)))
#################################################################################

#### MAYBE DO PACIFIC THINGS HERE???? TBD

#################################################################################
## Now Plot each bay separately
# This is easier because there is only one legend item
# Run as a function and pass in the various  


nmdsPrep = function(mergeData, bayColours) {
  # alter the dataframe so it is in appropriate format for NMDS
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  mergeData = mergeData %>% 
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0
  
  # For NMDS calculations, must only include species data from dataframe
  # I will constantly be removing columns, adding columns etc. 
  # Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
  beginNMDS = which(colnames(mergeData)== "Acartia spp.")
  endNMDS = ncol(mergeData)
  
  # Do NMDS ordination but only include species data
  ord = metaMDS(sqrt(mergeData[,c(beginNMDS:endNMDS)]), distance = "bray", autotransform=FALSE)
  
  # Get NMDS coordinates from plot
  ordCoords = as.data.frame(scores(ord, display="sites")) %>%
    mutate(tidePhase = mergeData$tidePhase) %>%
    mutate(facetFactor = mergeData$facetFactor) %>%
    mutate(myLabel = mergeData$myLabel)
  # Add NMDS stress
  # Note that round() includes UP TO 2 decimal places. Does not include 0s 
  ordStress = paste("2D Stress: ", format(round(ord$stress, digits=2), nsmall=2))
  

  # Need to get the legend title for each plot
  # For Maritimes/Newfoundland/Gulf, it's "Bay"
  # For Pacific, they only sampled one bay (Lemmens) but had multiple field seasons that will be displayed instead
  legendTitle = ifelse(mergeData$region[1] == "Pacific", # only need to check first entry
                              "Field Season", 
                              "Bay")
  
  # Get centroid of each bay (facetFactor) to be plotted on NMDS as lines
  centFacet = aggregate(cbind(NMDS1, NMDS2)~facetFactor, data =ordCoords, FUN = mean) # centroid of the regions
  # Now merge the region coordinates. These now form "segments" from centroid to actual coordinates
  segs = merge(ordCoords, setNames(centFacet, c('facetFactor', 'fNMDS1', 'fNMDS2')),
               by = "facetFactor", sort = FALSE)
  
  # Make the ggplot item for each DFO region
  ggBay =
    ggplot() + 
    geom_segment(data = segs, mapping = aes(x = NMDS1, xend = fNMDS1, y = NMDS2, yend = fNMDS2), col = "grey49")+ # map segments for distance to centroid
    geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = facetFactor, pch = facetFactor), alpha = 0.9, size = 5)+ # Use pch=21 to get black outline circles
    scale_fill_manual(name = legendTitle, values = bayColours)+
    scale_shape_manual(values = c(21:24),  name = legendTitle)+ 
    ggtitle(mergeData$region)+
    #annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4.5, hjust=1)+ # for all others
    annotate("text", x = min(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4.5, hjust = -0.01)+ # for Maritimes (otherwise 2D stress gets blocked)
    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title = element_text(size = 12), # don't want 
          axis.ticks = element_blank(),
          legend.text=element_text(size = 13),
          legend.title = element_text(size = 14),
          panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"),
          plot.title = element_text(size=16))

  return(ggBay)
  
}

marNMDS = nmdsPrep(marMerge, marColours)
nlNMDS = nmdsPrep(nlMerge, nlColours)
pacNMDS = nmdsPrep(pacMerge, pacColours)
gulfNMDS = nmdsPrep(gulfMerge, gulfColours)

# This works better than grid.arrange! It aligns all the plots with unequal legends
plot_grid(marNMDS, nlNMDS, pacNMDS, gulfNMDS, align = "v")

plot_grid(marNMDS, gulfNMDS, ncol = 1, align = "v")


