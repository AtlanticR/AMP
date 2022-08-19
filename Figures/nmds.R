### NMDS TESTS

# Set directory
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

# alter the dataframe so it is in appropriate format for NMDS
# names_from: The column whose values will be used as column names
# values_from: The column whose values will be used as cell values
marSpecies = marMerge %>% pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

# Do NMDS but only include species data
marNMDS = metaMDS(sqrt(marSpecies[,c(6:50)]), distance = "bray", autotransform=FALSE)

# Making base plot NMDS
plot(marNMDS$points, type="n")
# Add the sites in
text(marNMDS, display="sites", labels=row.names(marSpecies), cex=1, pos=3)
# Get the text for the legend. Round the NMDS stress to 2 values
stress09 = paste("2D Stress: ", round(marNMDS$stress, digits=2))
# Add stress text to top right corner of plot
legend("topright", legend = c(stress09), cex=1, bty="n") #btw=n gets rid of black box


# Get NMDS coordinates from plot
marCoords = as.data.frame(scores(marNMDS, display="sites"))

g1 = 

ggplot() + 
  geom_point(data = marCoords, aes(x=NMDS1, y=NMDS2, fill=as.factor(marSpecies$facilityName),
                                       pch = as.factor(marSpecies$tideRange)), size = 5)+ # Use pch=21 to get black outline circles
  # Change legend names
  scale_fill_discrete(name = "Bay")+
  scale_shape_manual(values=c(21, 22, 23), name = "Tide Range")+ 

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
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))


# Need to use ggsave to make Figures
# In Windows (vs Mac), the plots in the Viewer are pixelated and just so ugly
# ggsave("test.png", g1, scale = 1.7)
