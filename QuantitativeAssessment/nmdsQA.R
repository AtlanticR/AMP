################################################################################
################################################################################
#### NMDS FOR QUANTITATIVE ASSESSMENT DATA





################################################################################
# Prep the data

# Run the script that preps the QA and FlowCam data
source("QuantitativeAssessment/QAcodeMatches.R")



ordPrepDf = fcQaDf %>%
  select(newName, FlowCamID, regionYear, abund, type, qaSampleID) %>%
  # filter(newName != "EGG CHECK" & newName != "Calanoida (unid)") %>%
  # Get the relative abundance for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = abund / sum(abund)*100) %>%
  select(-abund)



# Convert it to wide format
ordPrepWide = ordPrepDf %>% 
  pivot_wider(names_from = newName, values_from = relAbund) %>%
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
      geom_point_interactive(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = type), pch = 21, size = 5, data_id = ordCoords$FlowCamID, tooltip = ordCoords$FlowCamID, onclick =ordCoords$FlowCamID)+ # Use pch=21 to get black outline circles
      ggtitle(plotTitle)+
      annotate("text", x = min(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4, hjust = -0.01)+
    
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
  
  
gulf21.Ord = nmdsMaker(ordPrepWide, "Gulf 2020", "Gulf 2020")
pac21.Ord = nmdsMaker(ordPrepWide, "Pac 21", "Pacific 2021")
nl20.Ord = nmdsMaker(ordPrepWide, "NL 2020", "Newfoundland 2020")
nl21.Ord = nmdsMaker(ordPrepWide, "NL 2021", "Newfoundland 2021")


ggarrange(gulf21.Ord, pac21.Ord, nl20.Ord, nl21.Ord)


################################################################################
### Track down some data
# Two of the plots had distinct groupings
# This is: Gulf 2020 (left hand side has 3 samples vs RHS has 7)
# For Newfoundland, there is an Upper cluster (4 points) and Lower cluster (6 points) on the NMDS

# For Gulf, need to go back to the original data that specifies if they're inner/mid/outer etc
# Too many sample labelling issues to get that from just the code 
# For NL, just point and click on the interactive plot. Can get info from FlowCamCodes.

# This creates an interactive ggplot that I can click on to see the FlowCamCodes
girafe(ggobj = gulf21.Ord)
girafe(ggobj = nl21.Ord)

# Gulf has 3 that form 1 group. They are:
# THESE ARE ALL "OUTER" STATIONS
# AMMP_Gulf_StPeters_1_20200903HT_250UM
# AMMP_Gulf_StPeters_1_20200904HT_250UM
# AMMP_Gulf_StPeters_3_20200903LT_250UM

# Need to go back to the original code from QAcodeMatches.R before i select for certain columns
findWeirdGulfStations = qaID %>%
  left_join(fcDataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  # Only select the samples we're interested in
  filter(selectForAnalysis == "Yes") %>%
  left_join(fcTaxaChanges) %>%
  filter(regionYear == "Gulf 2020") %>%
  filter(!(FlowCamID %in% c("AMMP_Gulf_StPeters_1_20200903HT_250UM", 
                          "AMMP_Gulf_StPeters_1_20200904HT_250UM", 
                          "AMMP_Gulf_StPeters_3_20200903LT_250UM")))

# End result: 
# For Gulf, LHS are Outer stations. RHS are Inner/Mid stations
# For NL, upper is station 41 ("Mid-B"). Lower = station 17 ("Outer")



################################################################################
### Find out what's behind these differences using SIMPER analysis


testNL = ordPrepWide %>%
  filter(regionYear == "NL 2021")

simTest = simper(sqrt(testNL[,which(colnames(testNL)== "Acartia spp."):ncol(testNL)]), 
                 group=testNL$type)
summary(simTest)













