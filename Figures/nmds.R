################################################################################
################################################################################
### NMDS Ordinations for FlowCam data


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")

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
  # Add NMDS stress
  # Note that round() includes UP TO 2 decimal places. Does not include 0s 
  ordStress = paste("2D Stress: ", format(round(ord$stress, digits=2), nsmall=2))
  
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


##### 
# Do an NMDS for everything

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

# OK BUT UGH i actually have to split this
# Atlantic
ggAtlantic = ggplot()+
  geom_point(data = atlanticData, aes(x = NMDS1, y = NMDS2, pch = region), fill = "#F8766D", size = 5)+
  scale_shape_manual(values = 21:23, name = "Atlantic")+
  theme_bw()

ggPacific = ggplot()+
  geom_point(data = pacificData, aes(x = NMDS1, y = NMDS2, pch = region), fill = "#00BFC4", size = 5)+
  scale_shape_manual(values = 24, name = "Pacific")+
  theme_bw()

install.packages("cowplot")
library("cowplot")

pacLegend = get_legend(ggPacific) 
atlLegend = get_legend(ggAtlantic)

ggBoth = 
ggplot() + 
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, fill = allRegionsWide$ocean, pch = allRegionsWide$region), size = 5)+ # Use pch=21 to get black outline circles

  #scale_fill_manual(name = "Ocean")+
  scale_shape_manual(values = regionArray, name = "Region")+ 
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=3.5, hjust=1)+
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

grid.arrange(pacLegend, atlLegend, ggBoth, ncol = 3)



grid.arrange(
  grobs = c(ggBoth, pacLegend, atlLegend),
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3, 4))
)


