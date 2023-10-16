################################################################################
################################################################################
#### NMDS FOR QUANTITATIVE ASSESSMENT DATA

# Note about transformations: arcsine square root transformed data, then Bray Curtis (ew)
# https://link.springer.com/article/10.1007/s10886-016-0784-x

# Square root transformed: https://peerj.com/articles/3437/

# square root: https://peerj.com/articles/3437/


################################################################################
# Prep the data

# Run the script that preps the QA and FlowCam data
source("QuantitativeAssessment/dataProcessing/redistributeTaxa.R")

# If I want to use redistributed abundances (from redistributeTaxa.R), just set this:
fcQaDf = fcQaDf.redist

# Adjust dataframe, calculate rel abundances
ordPrepDf = fcQaDf %>%
  select(newName, FlowCamID, regionYear, abund, type, qaSampleID) %>%
  # Get the relative abundance for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = abund / sum(abund)*100)


# Convert dataframes to wide format

# Make one for abundance in seawater
ordPrepWideAbund = ordPrepDf %>% 
  select(-relAbund) %>%
  pivot_wider(names_from = newName, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))   # replace NAs with 0 

# Make the other using relative abundance
ordPrepWideRelAbund = ordPrepDf %>% 
  select(-abund) %>% # have to remove this for some reason or data doesn't pivot properly
  pivot_wider(names_from = newName, values_from = relAbund) %>%
  mutate_all(~replace(., is.na(.), 0))   # replace NAs with 0 


# Create NMDS of both the FC and QA data
# Connect the samples from each method with a black line
# NMDS from each regionYear is made separately
nmdsMaker = function(df, regYearSelect, plotTitle){

  df = df %>%
    filter(regionYear == regYearSelect)

  # Select the beginning and end of the dataframe (with the species)
  beginNMDS = which(colnames(df)== "Acartia spp.")
  endNMDS = ncol(df)
  
  # Do NMDS ordination but only include species data
  ord = metaMDS(df[,c(beginNMDS:endNMDS)], distance = "bray", autotransform=FALSE)
  
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
    scale_fill_discrete(labels=c('FC', 'MC'))+
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
  
# Using abundances
gulf20.Ord = nmdsMaker(ordPrepWideAbund, "Gulf 2020", "(A) Gulf 2020")
pac21.Ord = nmdsMaker(ordPrepWideAbund, "Pac 21", "(B) Pacific 2021")
nl20.Ord = nmdsMaker(ordPrepWideAbund, "NL 2020", "(C) Newfoundland 2020")
nl21.Ord = nmdsMaker(ordPrepWideAbund, "NL 2021", "(D) Newfoundland 2021")

# Using relative abundances
gulf20.OrdRel = nmdsMaker(ordPrepWideRelAbund, "Gulf 2020", "(A) Gulf 2020")
pac21.OrdRel = nmdsMaker(ordPrepWideRelAbund, "Pac 21", "(B) Pacific 2021")
nl20.OrdRel = nmdsMaker(ordPrepWideRelAbund, "NL 2020", "(C) Newfoundland 2020")
nl21.OrdRel = nmdsMaker(ordPrepWideRelAbund, "NL 2021", "(D)Newfoundland 2021")


# Put them all together
# Abundances
ggarrange(gulf20.Ord, pac21.Ord, nl20.Ord, nl21.Ord)
# Relative abundance
ggarrange(gulf20.OrdRel, pac21.OrdRel, nl20.OrdRel, nl21.OrdRel)


# There are 3 Gulf samples with very high Bivalvia larvae that seem very different. Plot these separately
gulf3Weird = nmdsMaker(ordPrepWideRelAbund %>%
  filter((FlowCamID %in% c("AMMP_Gulf_StPeters_1_20200903HT_250UM", 
                            "AMMP_Gulf_StPeters_1_20200904HT_250UM", 
                            "AMMP_Gulf_StPeters_3_20200903LT_250UM"))), "Gulf 2020", "Gulf 2020")

gulfOtherWeird = nmdsMaker(ordPrepWideRelAbund %>%
  filter(!(FlowCamID %in% c("AMMP_Gulf_StPeters_1_20200903HT_250UM", 
                            "AMMP_Gulf_StPeters_1_20200904HT_250UM", 
                            "AMMP_Gulf_StPeters_3_20200903LT_250UM"))), "Gulf 2020", "Gulf 2020")

# Plot these side by side
ggarrange(gulf3Weird, gulfOtherWeird, ncol = 2)

# Try and see if I can figure out why 2 FC samples are overlapping?
girafe(ggobj = gulf3Weird)


################################################################################
# Make NMDS with all the data

nmdsMakerAllDat = function(df, plotTitle){
  
  beginNMDS = which(colnames(df)== "Acartia spp.")
  endNMDS = ncol(df)
  
  # Do NMDS ordination but only include species data
  ord = metaMDS(df[,c(beginNMDS:endNMDS)], distance = "bray", autotransform=FALSE)
  
  # Get NMDS coordinates from plot
  ordCoords = as.data.frame(scores(ord, display="sites")) %>%
    mutate(type = df$type,
           FlowCamID = df$FlowCamID,
           regionYear = df$regionYear) 
  
  # Add NMDS stress
  # Note that round() includes UP TO 2 decimal places. Does not include 0s 
  ordStress = paste("2D Stress: ", format(round(ord$stress, digits=2), nsmall=2))
  
  
  # Make the ggplot item for each DFO region
  ggplot() + 
    geom_line(data = ordCoords, aes(x = NMDS1, y = NMDS2, group = FlowCamID), col = "gray20")+
    geom_point_interactive(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = type, pch = regionYear), size = 5, data_id = ordCoords$FlowCamID, tooltip = ordCoords$FlowCamID, onclick =ordCoords$FlowCamID)+ # Use pch=21 to get black outline circles
    ggtitle(plotTitle)+
    scale_shape_manual(values=c(21:24), name = "Bay")+
    annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4, hjust = 1)+
    scale_fill_discrete(labels=c('FC', 'MC'))+
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
          plot.title = element_text(size=16))+
    guides(fill = guide_legend(override.aes = list(shape=21)))
  
}

# Run the function (for all data) using abundance in seawater
nmdsAll = nmdsMakerAllDat(ordPrepWideAbund, "Abundance in seawater: all regions")
# Run using relative abundance
nmdsAll.RelAbund = nmdsMakerAllDat(ordPrepWideRelAbund, "Relative abundance: all regions")

ggarrange(nmdsAll.RelAbund, nmdsAll, ncol =1)

################################################################################
## Make interactive plots to visualize "problem" taxa

# Make it interactive so you can point & click and see the sampleCode
girafe(ggobj = nmdsAll)
girafe(ggobj = nmdsAll.RelAbund)



### Track down some data
# Two of the plots had distinct groupings
# This is: Gulf 2020 (left hand side has 3 samples vs RHS has 7)
# For Newfoundland, there is an Upper cluster (4 points) and Lower cluster (6 points) on the NMDS

# For Gulf, need to go back to the original data that specifies if they're inner/mid/outer etc
# Too many sample labelling issues to get that from just the code 
# For NL, just point and click on the interactive plot. Can get info from FlowCamCodes.

# This creates an interactive ggplot that I can click on to see the FlowCamCodes
girafe(ggobj = gulf20.Ord)
girafe(ggobj = nl21.Ord)

girafe(ggobj = gulf20.OrdRel)

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


# Ok I think these three just have WAY more bivalvia larvae than the rest











