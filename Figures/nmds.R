### NMDS TESTS





marSpecies = marMerge %>% pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0



marNMDS = metaMDS(sqrt(marSpecies[,c(6:50)]), distance = "bray", autotransform=FALSE)
plot(marNMDS$points, type="n")
# Add the sites in
text(marNMDS, display="sites", labels=row.names(marSpecies), cex=1, pos=3)
# Get the text for the legend. Round the NMDS stress to 2 values
stress09 = paste("2D Stress: ", round(marNMDS$stress, digits=2))
# Add stress text to top right corner of plot
legend("topright", legend = c(stress09), cex=1, bty="n") #btw=n gets rid of black box


# Get NMDS coordinates from plot
data.scores10 = as.data.frame(scores(marNMDS, display="sites"))

g1 = 

ggplot() + 
  geom_point(data = data.scores10, aes(x=NMDS1, y=NMDS2, fill=as.factor(marSpecies$facilityName),
                                       pch = as.factor(marSpecies$tideRange)), size = 5)+ # Use pch=21 to get black outline circles
  #scale_size(range=c(2,8), name="Distance", limits=c(minDis, maxDis))+
  scale_fill_discrete(name = "Bay")+
  scale_shape_manual(values=c(21, 22, 23), name = "Tide Range")+ 

  annotate("text", x = max(data.scores10$NMDS1), y=max(data.scores10$NMDS2), label = stress09, size=3.5, hjust=1)+
  #labs(fill="Bay", shape="Tide Range")+
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


ggsave("test.png", g1, scale = 1.7)
