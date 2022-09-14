################################################################################
################################################################################
### NMDS Ordinations for FlowCam data


################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################
## Process the data



### TEST FUNCTION: 


nmdsPrep = function(mergeData) {
  
  # Group the stations so 5mm species are added to the regular counts 
  group_by(sampleCode, class, facilityName, waterVolume, tideRange, yearStart, myLabel) %>% summarize(abund = sum(abund)) %>%
    
}



# Find a more elegant way to do this!!
gulfSpecies = gulfMerge %>%
  # Remember there will be duplicates because of the 5mm data! These need to be combined!!
  group_by(sampleCode, class, facilityName, waterVolume, tideRange, yearStart, myLabel) %>% summarize(abund = sum(abund)) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0












# MARITIMES

# alter the dataframe so it is in appropriate format for NMDS
# names_from: The column whose values will be used as column names
# values_from: The column whose values will be used as cell values
marSpecies = marMerge %>% pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

# Do NMDS but only include species data
marNMDS = metaMDS(sqrt(marSpecies[,c(9:ncol(marSpecies))]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
marCoords = as.data.frame(scores(marNMDS, display="sites"))
# How stressed am I today
marStress = paste("2D Stress: ", round(marNMDS$stress, digits=2))


# GULF

# Find a more elegant way to do this!!
gulfSpecies = gulfMerge %>%
  # Remember there will be duplicates because of the 5mm data! These need to be combined!!
  group_by(sampleCode, class, facilityName, waterVolume, tideRange, yearStart, myLabel) %>% summarize(abund = sum(abund)) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0


# Do NMDS but only include species data
gulfNMDS = metaMDS(sqrt(gulfSpecies[,c(7:ncol(gulfSpecies))]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
gulfCoords = as.data.frame(scores(gulfNMDS, display="sites"))
# How stressed am I today
gulfStress = paste("2D Stress: ", round(gulfNMDS$stress, digits=2))


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


# PACIFIC

# Find a more elegant way to do this!!
pacSpecies = pacMerge %>%
  # Remember there will be duplicates because of the 5mm data! These need to be combined!!
  group_by(sample, class, dataset, sumWaterVolume, yearStart, myLabel) %>% summarize(abund = sum(abund)) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0


# Do NMDS but only include species data
pacNMDS = metaMDS(sqrt(pacSpecies[,c(6:ncol(pacSpecies))]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
pacCoords = as.data.frame(scores(pacNMDS, display="sites"))
# How stressed am I today
pacStress = paste("2D Stress: ", round(pacNMDS$stress, digits=2))

################################################################################
# Put all the data together for a mega-NMDS

hi = rbind(nlMerge, pacMerge, gulfMerge, marMerge)




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

