################################################################################
################################################################################
### NMDS Ordinations for FlowCam data


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")

################################################################################


#################################################################################
# Do an NMDS for everything
# Come back to this at a later date: difficulty with aligning legends
# / need 2 legend items

# Define the colours for everything

atlColour = c("Gulf" = "#F8766D", 
              "Maritimes" = "#7CAE00", 
              "Newfoundland" = "#00BFC4")
pacColourOne = c("Pacific" = "#C77CFF")


marColours = c("Argyle" = "darkgreen", 
               "Country Harbour" = "green3", 
               "Sober Island Oyster" = "darkolivegreen2", 
               "WhiteHead" = "mediumspringgreen")
nlColours = c("Newfoundland 2020" = "#00BFC4")
gulfColours = c("Cocagne" = "red4", 
                "Malpeque" = "red2", 
                "StPeters" = "lightpink")
pacColours = c("Pacific August 2020" = "plum1", 
               "Pacific June 2021" = "magenta4", 
               "Pacific March 2021" = "maroon", 
               "Pacific September 2021" = "maroon1")

# Add new columns for DFO regions and which ocean they are located in
marMerge = marMerge %>%
  mutate(region = "Maritimes", ocean = "Atlantic")
gulfMerge = gulfMerge %>%
  mutate(region= "Gulf", ocean = "Atlantic")
nlMerge = nlMerge %>%
  mutate(region = "Newfoundland", ocean = "Atlantic")
pacMerge = pacMerge %>% 
  mutate(region = "Pacific", ocean = "Pacific")

allRegions = rbind(marMerge, nlMerge, pacMerge, gulfMerge)

allRegionsWide = allRegions %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

# For NMDS calculations, must only include species data from dataframe
# I will constantly be removing columns, adding columns etc. 
# Instead define as the index where there's Acartia species (first species colum in dataframe) to the end (final column)
beginNMDSAll = which(colnames(allRegionsWide)== "Acartia spp. ")
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


nmdsNames = ordCoordsAll %>%
  mutate(region = allRegionsWide$region, ocean = allRegionsWide$ocean)

pacificData = nmdsNames %>%
  filter(ocean == "Pacific")
atlanticData = nmdsNames %>%
  filter(ocean == "Atlantic")

# This is how you get the GGPLOT colours. Put the # of classes in brackets
hue_pal()(4)

# OK BUT UGH i actually have to split this
# Atlantic
ggAtlantic = ggplot()+
  geom_point(data = atlanticData, aes(x = NMDS1, y = NMDS2, fill = region), pch = 21, size = 5)+
  scale_fill_manual(values = atlColour, name = "Atlantic Ocean")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

ggPacific = ggplot()+
  geom_point(data = pacificData, aes(x = NMDS1, y = NMDS2, fill = region), pch = 22, size = 5)+
  scale_fill_manual(values = pacColourOne, name = "Pacific Ocean")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))


# Get the legend and then turn it into a grob so patchwork can be used
pacLegend = as_grob(get_legend(ggPacific))
atlLegend = as_grob(get_legend(ggAtlantic))

ggBoth = 
  ggplot() + 
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, pch = allRegionsWide$ocean, fill = allRegionsWide$region), size = 5)+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_shape_manual(values = regionArray, name = "Region")+ 
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))

# This gets me PRETTY CLOSE to the Figure that I want, except that the legend items aren't totally lined up
# Will just fix this in PowerPoint (for now...)
grid.arrange(ggBoth, pacLegend, atlLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,2),
                                   c(1,1,1,3),
                                   c(1,1,1,NA)))

#################################################################################
#################################################################################
# Now just the Atlantic bays

beginNMDSAtl = which(colnames(allRegionsWide)== "Acartia spp. ")
endNMDSAtl = ncol(allRegionsWide)

atlOnly = allRegionsWide %>%
  filter(ocean == "Atlantic")
  
# Do NMDS ordination but only include species data
ordAtl = metaMDS(sqrt(atlOnly[,c(beginNMDSAtl:endNMDSAtl)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
ordCoordsAtl = as.data.frame(scores(ordAtl, display="sites")) %>%
  mutate(tidePhase = atlOnly$tidePhase) %>%
  mutate(facetFactor = atlOnly$facetFactor) %>%
  mutate(myLabel = atlOnly$myLabel) %>%
  mutate(region = atlOnly$region)

# Add NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressAtl = paste("2D Stress: ", format(round(ordAtl$stress, digits=2), nsmall=2))


# I need to create 3 legend items for:
# 1. Gulf as title (subpoints are Cocagne, Malpeque, StPeters)
# 2. Maritimes (Argyle, Country Harbour, Malpeque, Sober Island Oyster)
# 3. Newfoundland (rename as Southeast Arm)

# Gulf
ggGulf = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Gulf"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 7)+
  scale_fill_manual(values = c("red4", "red2", "lightpink"), name = "Gulf Region")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))


# Maritimes
ggMaritimes = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Maritimes"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 7)+
  scale_fill_manual(values = c("darkgreen", "green3", "darkolivegreen2", "mediumspringgreen"), name = "Maritimes Region")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))

# Maritimes
ggNewfoundland = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Newfoundland"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 7)+
  scale_fill_manual(values = c("#00BFC4"), name = "Newfoundland", labels = "Southeast Arm")+
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14))


# Get the legends
gulfLegend = as_grob(get_legend(ggGulf))
marLegend = as_grob(get_legend(ggMaritimes))
nlLegend = as_grob(get_legend(ggNewfoundland))

ggAtlanticOnly = ggplot()+
  geom_point(data = ordCoordsAtl, aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 7)+
  scale_shape_manual(values=c(21:23), name = "Bay")+
  scale_fill_manual(values = c("darkgreen", "red4", "green3", "red2", "#00BFC4", "darkolivegreen2", "lightpink", "mediumspringgreen"), name = "Region")+
  annotate("text", x = max(ordCoordsAtl$NMDS1), y=max(ordCoordsAtl$NMDS2), label = ordStressAtl, size=4, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))

grid.arrange(ggAtlanticOnly, gulfLegend, marLegend, nlLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,2),
                                   c(1,1,1,3),
                                   c(1,1,1,4),
                                   c(1,1,1,NA)))
#################################################################################



#################################################################################
#################################################################################



nmdsPrep = function(mergeData, bayColours) {
  # alter the dataframe so it is in appropriate format for NMDS
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  mergeData = mergeData %>% 
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0
  
  # For NMDS calculations, must only include species data from dataframe
  # I will constantly be removing columns, adding columns etc. 
  # Instead define as the index where there's Acartia species (first species colum in dataframe) to the end (final column)
  beginNMDS = which(colnames(mergeData)== "Acartia spp. ")
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
  
  # # Get the number of facets there should be (either # of bays, or # of sampling months (Pacific))
  # numTide = length(unique(mergeData$tidePhase))
  # # create array for pch symbols. e.g., if 4 factors will give: 21, 22, 23, 24
  # numPchTide = c(21:(20+numTide))
  
  # numLoc = length(unique(mergeData$myLabel))
  # numPchLoc = c(21:(20+numLoc))
  
  ggBay =
    ggplot() + 
    geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = facetFactor), pch =21, size = 5)+ # Use pch=21 to get black outline circles
    scale_fill_manual(name = "Bay", values = bayColours)+
    #scale_shape_manual(values=numPchTide, name = "Tide Phase")+ 
    annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=3.5, hjust=1)+
    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title.x = element_blank(), # don't want 
          axis.ticks = element_blank(),
          #legend.position = "none",
          panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
    # Set the shape as 21 otherwise they will not show up as coloured circles
    # Set the order to 1 so the "Bay" legend item will always be above "Tide Phase"
    guides(fill = guide_legend(override.aes = list(shape=21), order = 1))
  
  
  return(ggBay)
  
}



marNMDS = nmdsPrep(marMerge, marColours)
nlNMDS = nmdsPrep(nlMerge, nlColours)
pacNMDS = nmdsPrep(pacMerge, pacColours)
gulfNMDS = nmdsPrep(gulfMerge, gulfColours)









marColours = c("darkgreen", "green3", "darkolivegreen2", "mediumspringgreen")
































#################################################################################
#################################################################################
# CHANGING MY MIND ABOUT HOW TO DO THINGS
# THIS SECTION WILL PROBABLY BE DELETED. MOVING IT OUT OF THE WAY BUT NOT DELETING

# Create function to make NMDS ordinations for each region separately





nmdsPrep = function(mergeData) {
  # alter the dataframe so it is in appropriate format for NMDS
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  mergeData = mergeData %>% 
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0
  
  # For NMDS calculations, must only include species data from dataframe
  # I will constantly be removing columns, adding columns etc. 
  # Instead define as the index where there's Acartia species (first species colum in dataframe) to the end (final column)
  beginNMDS = which(colnames(mergeData)== "Acartia spp. ")
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
  
  # Get the number of facets there should be (either # of bays, or # of sampling months (Pacific))
  numTide = length(unique(mergeData$tidePhase))
  # create array for pch symbols. e.g., if 4 factors will give: 21, 22, 23, 24
  numPchTide = c(21:(20+numTide))
  
  numLoc = length(unique(mergeData$myLabel))
  numPchLoc = c(21:(20+numLoc))
  
  
  ggTide =
    ggplot() + 
    geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, pch = tidePhase, fill = facetFactor), size = 5)+ # Use pch=21 to get black outline circles
    scale_fill_discrete(name = "Bay")+
    scale_shape_manual(values=numPchTide, name = "Tide Phase")+ 
    annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=3.5, hjust=1)+
    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title.x = element_blank(), # don't want 
          axis.ticks = element_blank(),
          #legend.position = "none",
          panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
    # Set the shape as 21 otherwise they will not show up as coloured circles
    # Set the order to 1 so the "Bay" legend item will always be above "Tide Phase"
    guides(fill = guide_legend(override.aes = list(shape=21), order = 1))
  
  
  
}




