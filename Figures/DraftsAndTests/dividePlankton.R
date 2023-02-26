# Test dividing up copepods and unid zooplankton a bit better




# This has all the plankton data with counts for each file
source("DataProcessing/zooplanktonCounts.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("Figures/colourPchSchemes.R")


#################################################################################
#################################################################################
## Create NMDS for all data
# Including Pacific and Atlantic data

# Plot is a bit more complicated because I need 2 legend items: Atlantic Ocean (and DFO regions listed underneath)
# and Pacific Ocean (with DFO region listed underneath)
# This has to be done by making 3 ggplots to:
# 1. Get the legend (only, not the actual plot) from Atlantic data
# 2. Get the Legend (only) from Pacific data
# 3. Plot (with no legend) of both Pacific and Atlantic data
# All three will then be combined with grid.arrange()


# This will display each DFO region (Gulf, Maritimes, Newfoundland, Pacific) with a different colour
# Possibly different symbols for each ocean (Pacific, Atlantic). TBD how I handle this.

# Combine all the data together
allRegions = rbind(marMerge, nlMerge, pacMerge, gulfMerge)

#### THIS ONE WORKS FOR ONE SAMPLE. DON'T RUIN IT
# testData = allRegions %>%
#   filter(flowcamCode == "21_08_24_Mar_S04_Z01_1053_250") %>%
#   group_by(flowcamCode, copepodType) %>%
#   mutate(relAbund = ifelse(copepodType == "Calanoida", abund/sum(abund[copepodType == "Calanoida"]), NA_real_)) %>%
#   mutate(calanoidaAmt = .[.$class== "Calanoida (ci-cvi)",]$abund) %>% # Get the abundance of just Calanoida to be distributed
#   mutate(newAmount = ifelse(copepodType == "Calanoida", abund + relAbund*calanoidaAmt, NA_real_)) %>%
#   ungroup() %>%
#   filter(class != "Calanoida (ci-cvi)") # Now get rid of it


#### TRY FOR MORE THAN ONE SAMPLE


#### Divide up Calanoida among the genera

# Get just the calanoid abundances for each sample
# This is what needs to be redistributed among each of the Calanoid subtypes
# Result will be a dataframe for all regions, with 1 column of sample name with other column as calanoid abund in that sample
calSamAbund = allRegions %>%
  filter(class == "Calanoida (ci-cvi)") %>%
  group_by(sampleCode) %>%
  summarize(calan_abund = sum(abund))

# Distribute those calanoid abundances based on the relative abundance of each calanoid genera 
# Do this by relative abundance within each sample
calan_distribute = allRegions %>%
  group_by(sampleCode, copepodType) %>%
  mutate(relAbundCal = ifelse(copepodType == "Calanoida", abund/sum(abund[copepodType == "Calanoida"]), NA_real_)) %>%
  full_join(calSamAbund) %>%
  mutate(abundPlusCal = ifelse(copepodType == "Calanoida" & !is.na(calan_abund), abund + relAbundCal*calan_abund, abund)) %>%
  ungroup() %>%
  filter(class != "Calanoida (ci-cvi)") # Now get rid of it


#### Divide up the Cyclopoida among the genera

cyclSamAbund = allRegions %>%
  filter(class == "Cyclopoida (ci-vi or n.s.)") %>%
  group_by(sampleCode) %>%
  summarize(cycl_abund = sum(abund))


cycl_distribute = calan_distribute %>%
  group_by(sampleCode, copepodType) %>%
  mutate(relAbundCycl = ifelse(copepodType == "Cyclopoida", abundPlusCal/sum(abundPlusCal[copepodType == "Cyclopoida"]), NA_real_)) %>%
  full_join(cyclSamAbund) %>%
  mutate(abundPlusCycl = ifelse(copepodType == "Cyclopoida" & !is.na(cycl_abund), abundPlusCal + relAbundCycl*cycl_abund, abundPlusCal)) %>%
  ungroup() %>%
  filter(class != "Calanoida (ci-cvi)") # Now get rid of it


# Get just the copepod abundances for each sample
# This is what needs to be redistributed among each of the Calanoid subtypes
# Result will be a dataframe for all regions, with 1 column of sample name with other column as calanoid abund in that sample
copSamAbund = allRegions %>%
  filter(class == "Copepoda") %>%
  group_by(sampleCode) %>%
  summarize(copep_abund = sum(abund))

# Distribute those calanoid abundances based on the relative abundance of each calanoid genera 
# Do this by relative abundance within each sample
cop_distribute = cycl_distribute %>%
  group_by(sampleCode, isCopepod) %>%
  mutate(relAbundCop = ifelse(isCopepod == "Yes", abundPlusCycl/sum(abundPlusCycl[isCopepod == "Yes"]), NA_real_)) %>%
  full_join(copSamAbund) %>%
  mutate(abundPlusCop = ifelse(isCopepod == "Yes" & !is.na(copep_abund), abundPlusCycl + relAbundCop*copep_abund, abundPlusCycl)) %>%
  ungroup() %>%
  filter(class != "Copepoda") # Now get rid of it



#### Get the Zooplankton (unid) abundances for each sample
zooSamAbund = cop_distribute %>%
  filter(class == "Zooplankton (unid)") %>%
  group_by(sampleCode) %>%
  summarize(zooUnid_abund = sum(abund))


# Distribute those zooplankton abundances based on the relative abundance of all taxa 
# Do this by relative abundance within each sample
zoo_distribute = cop_distribute %>%
  # Remove the previous column called relAbund so it can be used again
  group_by(sampleCode) %>%
  # I think this will overwrite the original relAbund column?
  mutate(relAbundZoo = abundPlusCop/sum(abundPlusCop)) %>%
  full_join(zooSamAbund) %>%
  # Add the zooplankton abundances to each taxa. But only do this if there were actually unidentified zooplankton in each sample!
  mutate(abundPlusZoo = ifelse(!is.na(zooUnid_abund), abundPlusCop + relAbundZoo*zooUnid_abund, abundPlusCop)) %>%
  ungroup() %>%
  filter(class != "Zooplankton (unid)") # Now get rid of it


rem.cols = c("isCopepod", "copepodType", "relAbundCal", "relAbundCycl", "relAbundCop", "relAbundZoo", "abund", "calan_abund", "cycl_abund", "copep_abund", "zooUnid_abund",
             "abundPlusCal", "abundPlusCycl", "abundPlusCop")


mar = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Maritimes") %>%
  ungroup()

gulf = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Gulf") %>%
  #filter(sampleCode != "20_09_01_Gulf_S04_Z39_0858_250") %>%
  ungroup()

pac = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Pacific") %>%
  filter(sampleCode != c("AMMP_PA_S04W15_20210610HT_250um")) %>%
  filter(sampleCode != c("AMMP_PA_S04W01_20210611HT_250um")) %>%
  ungroup() 

nl = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Newfoundland") %>%
  ungroup()

  
nmdsBay = function(regionData, stationCol) {
    
    # alter the dataframe so it is in appropriate format for NMDS
    # names_from: The column whose values will be used as column names
    # values_from: The column whose values will be used as cell values
    regionData = regionData %>% 
      pivot_wider(names_from = class, values_from = abundPlusZoo) %>%
      mutate_all(~replace(., is.na(.), 0))  # replace NAs with 0
    
    
    # Initialize a list to store multiple ggplots
    ggList = list()
    
    # Loop through all the bays within each region
    for(i in 1:length(unique(regionData$facetFactor))){
      
      # Get the data for each bay. Sort them alphabetically
      bayData = regionData %>%
        filter(facetFactor == sort(unique(regionData$facetFactor))[i])
      
      # For NMDS calculations, must only include species data from dataframe
      # I will constantly be removing columns, adding columns etc. 
      # Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
      beginNMDS = which(colnames(bayData)== "Acartia spp. (civ-vi)")
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
      
      # There is probably a better way to deal with this. But I need to adjust the Newfoundland ggtitles to be in chronological order, not alphabetical
      # When sorted alphabetically, the facetFactor "Oct 2021" comes before "Sept 2020". Fix this.
      facetLabel = ifelse(bayData$facetFactor[2] == "Sept 2020", "(A) Sept 2020", # if it's the Sept 2020 data, set the facetLabel as "(A) Sept 2020"
                          ifelse(bayData$facetFactor[1] == "Oct 2021", "(B) Oct 2021", # if it's the Oct 2021 data, set the facetLabel as "(B) Oct 2021"
                                 # For all other data, NMDS ordinations will be split up alphabetically, and lettered A, B, C etc., followed by the bay name
                                 paste("(", LETTERS[i], ") ", bayData$facetFactor, sep ="")))  
      
      # Create the ggplot
      ggBay =
        ggplot() + 
        geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, pch = tidePhase, fill = myLabel), size = 5)+ # Use pch=21 to get black outline circles
        # Remember that ifelse can be nested in multiple ways. Here I'm using:
        # ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))
        # I want Cocagne to have month labels as text since data was collected during months (it's the only site like this)
        # Low tides are from July, Mid-Rising are from August
        geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= ifelse(facetFactor == "Cocagne" & tidePhase == "Low", "Jul",
                                                                              ifelse(facetFactor == "Cocagne" & tidePhase == "Mid-Rising", "Aug", ""))), colour = "gray30")+ # Use pch=21 to get black outline circles
        # geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= sampleCode), colour = "gray30")+ # Use this to check my legends in nmdsBaysWithLegend.R are correct
        # geom_text_repel(data = ordCoords, aes(x=NMDS1, y=NMDS2, label= tidePhase), colour = "gray30")+
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
              panel.border=element_rect(color="black", linewidth= 0.8), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              plot.background = element_blank(),
              # This affects the amount of space around each plot
              # If there is not enough space, plot_grid will make them too close together
              # Written as (top, right, bottom, left). I want less empty space to the right because the legend will go there
              plot.margin=unit(c(0.3, 0.1, 0.3, 0.3),"cm"),
              plot.title = element_text(size=16))
      
      # Add each ggplot to a list. List will be created for each region, and each list item will be ordination for each bay
      ggList[[i]] = ggBay
      
    }
    
    # No longer need this. But if I want to arrange all plots, it would be like this:
    # gridOfPlots = do.call("plot_grid", c(ggList, align = "v", ncol = 2, nrow = 2))
    
    return(ggList)
    
  }

gulf2 = gulf %>%
  filter(sampleCode != "20_09_01_Gulf_S04_Z39_0858_250")


marNMDSbays = nmdsBay(mar, stationCol)
gulfNMDSbays = nmdsBay(gulf, stationCol)


pac2 = pac %>%
  filter(facetFactor != "March 2021") %>%
  filter(sampleCode != c("AMMP_PA_S04W15_20210610HT_250um")) %>%
  filter(sampleCode != c("AMMP_PA_S04W01_20210611HT_250um"))


nl2 = nl %>%
  filter(monthStart == 10 & yearStart == 2021)


nl3 = nl %>%
  filter(monthStart == 09 & yearStart == 2020)



nmdsBay(nl2, stationColNL)

pac3 = pac2 %>%
  filter(facetFactor == "June 2021")

pacNMDSbays = nmdsBay(pac2, stationCol)
pacNMDSbays[[2]]





test = mar %>%
  filter(sampleCode == "21_08_24_Mar_S04_Z01_1053_250" | sampleCode == "21_08_24_Mar_S04_Z02_1117_250")


