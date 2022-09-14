################################################################################
################################################################################
### NMDS Ordinations for FlowCam data


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################



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
  
  # Test that ordination stress is created (i.e., nmds works)
  return(ordStress)
  
}


marNMDS = nmdsPrep(marMerge)
nlNMDS = nmdsPrep(nlMerge) #ERROR BECAUSE OF EMPTY THING
pacNMDS = nmdsPrep(pacMerge)
gulfNMDS = nmdsPrep(gulfMerge)



# NEWFOUNDLAND

# Find a more elegant way to do this!!
nlSpecies = nlMerge %>%
  # Remember there will be duplicates because of the 5mm data! These need to be combined!!
  group_by(sample, class, waterVolume, yearStart, myLabel) %>% 
  summarize(abund = sum(abund)) %>%
  # Remove a blank entry. I don't know where this came from!
  subset(class != "") %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0


# Do NMDS but only include species data
nlNMDS = metaMDS(sqrt(nlSpecies[,c(5:ncol(nlSpecies))]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
nlCoords = as.data.frame(scores(nlNMDS, display="sites"))
# How stressed am I today
nlStress = paste("2D Stress: ", round(nlNMDS$stress, digits=2))




################################################################################
## Make the plots

# Plot with Bay and Tide Range
g1 = 
ggplot() + 
  geom_point(data = marCoords, aes(x=NMDS1, y=NMDS2, fill=as.factor(marSpecies$facilityName),
                                       pch = as.factor(marSpecies$tideRange)), size = 5)+ # Use pch=21 to get black outline circles
  scale_fill_discrete(name = "Bay")+
  scale_shape_manual(values=c(21, 22, 23), name = "Tide Range")+ 
  annotate("text", x = max(marCoords$NMDS1), y=max(marCoords$NMDS2), label = marStress, size=3.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
  # Need to override and show these as a new new shape otherwise it won't show up
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))


# Need to use ggsave to make Figures
# In Windows (vs Mac), the plots in the Viewer are pixelated and just so ugly
# ggsave("test.png", g1)

#########

# Maritimes
g2 = 
ggplot() + 
  geom_point(data = marCoords, aes(x=NMDS1, y=NMDS2, fill=as.factor(marSpecies$facilityName),
                                   pch = as.factor(marSpecies$location)), size = 5)+ # Use pch=21 to get black outline circles
  # Change legend names
  scale_fill_discrete(name = "Location")+
  scale_shape_manual(values=c(21, 22, 23), name = "Location within bay")+ 
  
  annotate("text", x = max(marCoords$NMDS1), y=max(marCoords$NMDS2), label = stress09, size=3.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
  # Need to override and show these as a new new shape otherwise it won't show up
  guides(fill = guide_legend(override.aes = list(shape = 21), order = 1),
         shape = guide_legend(override.aes = list(fill = "black")), order=2)

# ggsave("test2.png", g2)


##### GULF ##### 
ggplot() + 
  geom_point(data = gulfCoords, aes(x=NMDS1, y=NMDS2, fill=as.factor(gulfSpecies$facilityName)),
                                   pch = 21, size = 7, alpha = 0.85)+ # Use pch=21 to get black outline circles
  # Change legend names
  scale_fill_discrete(name = "Location")+
  scale_shape_manual(values=c(21, 22, 23, 24, 25), name = "Location within bay")+ 
  
  annotate("text", x = max(gulfCoords$NMDS1), y=max(gulfCoords$NMDS2), label = gulfStress, size=3.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
  # Need to override and show these as a new new shape otherwise it won't show up
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))


# ggsave("test3.png", g3)



##### PACIFIC ##### 
g4 = 
  ggplot() + 
  geom_point(data = pacCoords, aes(x=NMDS1, y=NMDS2, fill=as.factor(pacSpecies$dataset)),
             pch = 21, size = 7, alpha = 0.85)+ # Use pch=21 to get black outline circles
  # Change legend names
  scale_fill_discrete(name = "Location")+
  scale_shape_manual(values=c(21, 22, 23, 24, 25), name = "Location within bay")+ 
  
  annotate("text", x = max(pacCoords$NMDS1), y=max(pacCoords$NMDS2), label = pacStress, size=3.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))+
  # Need to override and show these as a new new shape otherwise it won't show up
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))

##### NEWFOUNDLAND #####  
g5 = 
  ggplot() + 
  geom_point(data = nlCoords, aes(x=NMDS1, y=NMDS2), fill = "blue",
             pch = 21, size = 7, alpha = 0.85)+ # Use pch=21 to get black outline circles
  # Change legend names
  #scale_fill_discrete(name = "Location")+
  #scale_shape_manual(values=c(21, 22, 23, 24, 25), name = "Location within bay")+ 
  
  annotate("text", x = max(nlCoords$NMDS1), y=max(nlCoords$NMDS2), label = nlStress, size=3.5, hjust=1)+
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

