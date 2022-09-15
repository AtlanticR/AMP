################################################################################
################################################################################
### NMDS Ordinations for FlowCam data


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################

# Create function to make NMDS ordinations

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
  ordCoords = as.data.frame(scores(ord, display="sites"))
  # How stressed am I today
  ordStress = paste("2D Stress: ", round(ord$stress, digits=2))
  
  # Get the number of facets there should be (either # of bays, or # of sampling months (Pacific))
  numFacet = length(unique(marMerge$facetFactor))
  # create array for pch symbols. e.g., if 4 factors will give: 21, 22, 23, 24
  numPch = c(21:(20+numFacet))
  
  g1 =
  ggplot() + 
    geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, pch = mergeData$facetFactor, fill = mergeData$facetFactor, group=mergeData$facetFactor), size = 5)+ # Use pch=21 to get black outline circles
    # Note, for legends to be combined (instead of 1 legend for points, one for fill, the name must be the same!)
    scale_fill_discrete(name = "Bay")+
    scale_shape_manual(values=numPch, name = "Bay")+ 
    annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=3.5, hjust=1)+
    theme_bw()+
    theme(axis.text = element_blank(),
          #axis.title = element_blank(),
          axis.ticks = element_blank(),
          #legend.position = "none",
          panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))
  
  # Test that ordination stress is created (i.e., nmds works)
  return(g1)
  
}

marNMDS = nmdsPrep(marMerge)
nlNMDS = nmdsPrep(nlMerge)
pacNMDS = nmdsPrep(pacMerge)
gulfNMDS = nmdsPrep(gulfMerge)


