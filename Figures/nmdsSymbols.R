################################################################################
################################################################################
### NMDS Ordinations for FlowCam data
# But in this script I'm going to add symbols to distinguish each plot

# It has the same plots as in nmds.R but it's easier to just create a separate script
# and then choose my favourite one


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")

################################################################################
## Define the colour palettes for each bay/region

# Set the colours for data from each ocean
# Note, these are just the default ggplot colours when there are 4 items to be displayed
# Can get the colours from this function. Put the # of classes in brackets
hue_pal()(4)

#### Based on ocean
# Atlantic Ocean
atlColour = c("Gulf" = "#F8766D", 
              "Maritimes" = "#7CAE00", 
              "Newfoundland" = "#00BFC4")
# Pacific Ocean 
# Note this is the colour when only Pacific OCEAN data is displayed
# When Pacific (region) data is broken down by field season (below), 4 colours will be set
pacColourOne = c("Pacific" = "#C77CFF")


#### Based on bay
# I just looked here and tried to find different shades of each colour set above:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# These are not final colours. Also I do not need to specify the bay name, but it helps to 
# have them written down somewhere 
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

### Also set the symbols for the tides
pchTide = c("High" = 21,
            "Low" = 22,
            "Mid-Falling" = 23,
            "Mid-Rising" = 24)

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

# Add region and ocean onto this data frame (it's easier for plotting in aes for ggplot)
ordCoordsAll = ordCoordsAll %>%
  mutate(region = allRegionsWide$region, ocean = allRegionsWide$ocean)

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

# Put everything together
# This gets me PRETTY CLOSE to the Figure that I want, except that the legend items aren't totally lined up
# Will just fix this in PowerPoint (for now...)
grid.arrange(ggBoth, pacLegend, atlLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,2),
                                   c(1,1,1,3),
                                   c(1,1,1,NA)))

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
beginNMDSAtl = which(colnames(atlOnly)== "Acartia spp. ")
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

# Get (just) the legends
gulfLegend = as_grob(get_legend(ggGulf))
marLegend = as_grob(get_legend(ggMaritimes))
nlLegend = as_grob(get_legend(ggNewfoundland))

# Now make a plot of everything (without the legend)
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

grid.arrange(marNMDS, nlNMDS, pacNMDS, gulfNMDS)

# This works better than grid.arrange! It aligns all the plots with unequal legends
plot_grid(marNMDS, nlNMDS, pacNMDS, gulfNMDS, align = "v")


#################################################################################
#################################################################################
### Each bay separately so I can plot tides & location



nmdsBay = function(regionData, regionColour) {
  
  # alter the dataframe so it is in appropriate format for NMDS
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  regionData = regionData %>% 
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0
  
  # Initialize a list to store multiple ggplots
  ggList = list()
  
  # Loop through all the bays within each region
  for(i in 1:length(unique(regionData$facetFactor))){
    
    # Get the data for each bay. Sort them alphabetically
    bayData = regionData %>%
      filter(facetFactor == sort(unique(regionData$facetFactor))[i])
    # Region colours are already alphabetized, don't need to worry about sorting (yay!)
    bayColour = regionColour[i]
    
    # For NMDS calculations, must only include species data from dataframe
    # I will constantly be removing columns, adding columns etc. 
    # Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
    beginNMDS = which(colnames(bayData)== "Acartia spp. ")
    endNMDS = ncol(bayData)
    
    # Do NMDS ordination but only include species data
    ord = metaMDS(sqrt(bayData[,c(beginNMDS:endNMDS)]), distance = "bray", autotransform=FALSE)
    
    # Get NMDS coordinates from plot
    ordCoords = as.data.frame(scores(ord, display="sites")) %>%
      mutate(tidePhase = bayData$tidePhase) %>%
      mutate(facetFactor = bayData$facetFactor) %>%
      mutate(myLabel = bayData$myLabel) %>%
      mutate(sampleCode = bayData$sampleCode)
    
    # Add NMDS stress
    # Note that round() includes UP TO 2 decimal places. Does not include 0s 
    ordStress = paste("Stress: ", format(round(ord$stress, digits=2), nsmall=2))
    
    # Create the ggplot
    ggBay =
      ggplot() + 
      geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, pch = tidePhase), fill = bayColour, size = 5)+ # Use pch=21 to get black outline circles
      geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= myLabel), colour = "gray30")+ # Use pch=21 to get black outline circles
      #geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= sampleCode), colour = "gray30")+ # Use pch=21 to get black outline circles
      # adding "breaks" will make sure only the tidePhases actually present in each plot will show up
      # sorting them will make sure they display alphabetically/consistently between each plot
      scale_shape_manual(values = pchTide, name = "Tide Phase", breaks = sort(unique(ordCoords$tidePhase)))+
      ggtitle(bayData$facetFactor)+
      #scale_shape_manual(values=numPchLoc, name = "Location")+ 
      #annotate("text", x = min(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=3.5, hjust=1)+
      #annotate("text", x = min(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=3.5, hjust = -0.02)+
      theme_bw()+
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            #legend.position = "none",
            panel.border=element_rect(color="black", size=1), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            # This affects the amount of space around each plot
            # If there is not enough space, plot_grid will make them too close together
            plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"))+
      # Set the shape as 21 otherwise they will not show up as coloured circles
      # Set the order to 1 so the "Bay" legend item will always be above "Tide Phase"
      guides(fill = guide_legend(override.aes = list(shape=21), order = 1))
    
    # Add each ggplot to a list
    ggList[[i]] = ggBay
    
  }
  
  # Arrange the plots. COME BACK TO THIS!!!!! How do I just this to just be i number of gglLists
  #arrangePlot = plot_grid(ggList[[1]], ggList[[2]], ggList[[3]], ggList[[4]], align = "v")
  # ^ This can be replaced with do.call()!!!
  # I think this can replace a lot of my garbage code!!
  # Align them vertically so each PLOT lines up even if legend sizes differ slightly
  # Setting ncol/nrow will mean all plots have same size (NL2020 only has one bay. otherwise plot is huge)
  gridOfPlots = do.call("plot_grid", c(ggList, align = "v", ncol = 2, nrow = 2))
  
  return(gridOfPlots)
  
}

# Run the function by passing in the data and the colour scheme for the region
nmdsBay(marMerge, marColours)
nmdsBay(gulfMerge, gulfColours)
nmdsBay(nlMerge, nlColours)

# PACIFIC: need to remove March data because it only has 2 data points and can't do NMDS on that
# Also remove the two "outliers" (from Pacific June 2021) because otherwise distorts individual NMDS
nmdsBay(pacMerge %>% filter(facetFactor != "Pacific March 2021") %>%
          filter(sampleCode != c("AMMP_PA_S04W15_20210610HT_250um"))%>%
          filter(sampleCode != c("AMMP_PA_S04W01_20210611HT_250um")), pacColours)


