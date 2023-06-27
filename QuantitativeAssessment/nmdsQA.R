################################################################################
################################################################################
#### NMDS FOR QUANTITATIVE ASSESSMENT DATA





################################################################################
# Prep the data

# Run the script that preps the QA and FlowCam data
source("QuantitativeAssessment/QAcodeMatches.R")


qaOrdPrep = allQAData %>%
  select(newName, FlowCamID, regionYear, abund) %>%
  mutate(type  = "QA")


fcOrdPrep = flowCamData %>%
  ungroup() %>%
  select(newName, FlowCamID, regionYear, abund) %>%
  mutate(type  = "FC") 

# Combine all the data together
ordPrepDf = rbind(qaOrdPrep, fcOrdPrep)

# Convert it to wide format
ordPrepWide = ordPrepDf %>% 
  pivot_wider(names_from = newName, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))   # replace NAs with 0 



nmdsMaker = function(df, regYearSelect, plotTitle){

  
  df = df %>%
    filter(regionYear == regYearSelect)

  beginNMDS = which(colnames(df)== "Acartia spp.")
  endNMDS = ncol(df)
  
  # Do NMDS ordination but only include species data
  ord = metaMDS(sqrt(df[,c(beginNMDS:endNMDS)]), distance = "bray", autotransform=FALSE)
  
  # Get NMDS coordinates from plot
  ordCoords = as.data.frame(scores(ord, display="sites")) %>%
    mutate(type = df$type,
           FlowCamID = df$FlowCamID) 
  
  # Add NMDS stress
  # Note that round() includes UP TO 2 decimal places. Does not include 0s 
  ordStress = paste("2D Stress: ", format(round(ord$stress, digits=2), nsmall=2))
  
  
  # Make the ggplot item for each DFO region
  ggplot() + 
      geom_line(data = ordCoords, aes(x = NMDS1, y = NMDS2, group = FlowCamID), col = "gray20")+
      geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = type), pch = 21, size = 5)+ # Use pch=21 to get black outline circles
      ggtitle(plotTitle)+
      theme_bw()+
      theme(axis.text = element_blank(),
          axis.title = element_text(size = 12), # don't want 
          axis.ticks = element_blank(),
          legend.text=element_text(size = 13),
          legend.title = element_blank(),
          #panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"),
          plot.title = element_text(size=16))
  
}
  
  
a = nmdsMaker(allRegionsWide, "Gulf 2020", "Gulf 2020")
b = nmdsMaker(allRegionsWide, "Pac 21", "Pacific 2021")
c = nmdsMaker(allRegionsWide, "NL 2020", "Newfoundland 2020")
d = nmdsMaker(allRegionsWide, "NL 2021", "Newfoundland 2021")


ggarrange(a, b, c, d)
