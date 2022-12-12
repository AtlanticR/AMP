#################################################################################
#################################################################################
### Create NMDS ordinations for each bay

# Created a script that will read in the data for each region, and create separate
# NMDS ordinations for each bay within those regions

# Note that the legends for these figures (showing tide phase as symbols and 
# stations as colours) are complex. Those are added in a different script:
# nmdsBaysWithLegend.R
# Those could have been added here, but it would be too long

# Instead, this creates the plots, stores them as a list, and the other script
# makes custom legends for each bay.

#################################################################################
## Set-up

# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("Figures/colourPchSchemes.R")

#################################################################################
#################################################################################
### Create NMDS ordinations for each bay

# Pass in the data for each region. Will create separate ordinations for each bay
# Data will be returned as a list, with each list element containing the ordination
# for each bay within the region

# Plots will show stations (i.e., location in bay) with colours
# Tide phases as symbols
# Those are defined in colourPchSchemes.R and passed into function as second element

nmdsBay = function(regionData, stationCol) {
  
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
    
    # Getting rid of this: no longer colouring data by bay. Instead, colour by station
    # Region colours are already alphabetized, don't need to worry about sorting (yay!)
    # bayColour = regionColour[i]
    
    # For NMDS calculations, must only include species data from dataframe
    # I will constantly be removing columns, adding columns etc. 
    # Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
    beginNMDS = which(colnames(bayData)== "Acartia spp.")
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
    ordStress = paste("Stress:", format(round(ord$stress, digits=2), nsmall=2))
    
    # For Newfoundland, there is only one plot. So I do not want want a letter in brackets, e.g., (A) Southeast Arm, 2020
    # For all others, create label so it's e.g., (A) Argyle. Default includes a space between each element, so set sep = "" to remove that
    facetLabel = ifelse(bayData$facetFactor[1] == "Southeast Arm 2020", # only need to check first entry
                        bayData$facetFactor, 
                        paste("(", LETTERS[i], ") ", bayData$facetFactor, sep =""))
    
    # Create the ggplot
    ggBay =
      ggplot() + 
      geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, pch = tidePhase, fill = myLabel), size = 5)+ # Use pch=21 to get black outline circles
      #geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= myLabel), colour = "gray30")+ # Use pch=21 to get black outline circles
      #geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= sampleCode), colour = "gray30")+ # Use pch=21 to get black outline circles
      # adding "breaks" will make sure only the tidePhases actually present in each plot will show up
      # sorting them will make sure they display alphabetically/consistently between each plot
      scale_shape_manual(values = pchTide, name = "Tide Phase", breaks = sort(unique(ordCoords$tidePhase)))+
      scale_fill_manual(values = stationCol)+
      
      # Add a bit of extra space on y-axis so stress can be added
      #coord_cartesian(ylim = c(min(ordCoords$NMDS2* 1.05), max(ordCoords$NMDS2)*1.3))+
      
      ggtitle(facetLabel)+
      # Add 2D stress to the top right. I don't understand the units of hjust? Or the direction.
      
      # Add stress to plot. Add a bit of extra space to y-axis to add stress without it overlapping with points
      annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2 * 1.28), label = ordStress, size= 4.2, hjust = 0.9)+
      
      theme_bw()+
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = 12),
            legend.position = "none",
            #legend.text = element_text(size = 13),
            #legend.title = element_text(size = 14),
            panel.border=element_rect(color="black", linewidth= 0.8), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            # This affects the amount of space around each plot
            # If there is not enough space, plot_grid will make them too close together
            plot.margin=unit(c(0.3, 0.1, 0.3, 0.3),"cm"),
            plot.title = element_text(size=16))+
      # Set the shape as 21 otherwise they will not show up as coloured circles
      # Set the order to 1 so the "Bay" legend item will always be above "Tide Phase"
      guides(fill = guide_legend(override.aes = list(shape=21), order = 1))
    
    # Add each ggplot to a list. List will be created for each region, and each list item will be ordination for each bay
    ggList[[i]] = ggBay
    
  }
  
  # No longer need this. But if I want to arrange all plots, it would be like this:
  # gridOfPlots = do.call("plot_grid", c(ggList, align = "v", ncol = 2, nrow = 2))

  return(ggList)
  
}

# Run the function by passing in the data and the colour scheme for the region
marNMDSbays = nmdsBay(marMerge, stationCol)
gulfNMDSbays = nmdsBay(gulfMerge, stationCol)
nlNMDSbays = nmdsBay(nlMerge, stationColNL) # remember that NL has different colour scheme

# Pacific: remove March data because it only has 2 data points and can't do NMDS on that
pacNMDSbays = nmdsBay(pacMerge  %>% filter(facetFactor != "March 2021"), stationCol) # without removing outliers

# May also need to remove the two "outliers" (from Pacific June 2021) because otherwise distorts individual NMDS
# However, after fixing typos in November, this may not be needed anymore
nmdsBay(pacMerge %>% filter(facetFactor != "March 2021") %>%
          filter(sampleCode != c("AMMP_PA_S04W15_20210610HT_250um"))%>%
          filter(sampleCode != c("AMMP_PA_S04W01_20210611HT_250um")), pacColours)

# Note, this combines all ggplots for each bay into a list. To access, do this:
# marNMDSbays[[1]] # e.g., for Argyle (the first one)
