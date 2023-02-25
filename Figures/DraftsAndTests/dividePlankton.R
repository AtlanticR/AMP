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
testData = allRegions %>%
  filter(flowcamCode == "21_08_24_Mar_S04_Z01_1053_250") %>%
  group_by(flowcamCode, copepodType) %>%
  mutate(relAbund = ifelse(copepodType == "Calanoida", abund/sum(abund[copepodType == "Calanoida"]), NA_real_)) %>%
  mutate(calanoidaAmt = .[.$class== "Calanoida (ci-cvi)",]$abund) %>% # Get the abundance of just Calanoida to be distributed
  mutate(newAmount = ifelse(copepodType == "Calanoida", abund + relAbund*calanoidaAmt, NA_real_)) %>%
  ungroup() %>%
  filter(class != "Calanoida (ci-cvi)") # Now get rid of it


#### TRY FOR MORE THAN ONE SAMPLE

# Get just the calanoid abundances for each sample
testData3 = allRegions %>%
  filter(class == "Calanoida (ci-cvi)") %>%
  group_by(flowcamCode) %>%
  summarize(calan_abund = sum(abund))


testData2 = allRegions %>%
  group_by(flowcamCode, copepodType) %>%
  mutate(relAbund = ifelse(copepodType == "Calanoida", abund/sum(abund[copepodType == "Calanoida"]), NA_real_)) %>%
  left_join(testData3) %>%
  mutate(newAmount = ifelse(copepodType == "Calanoida", abund + relAbund*calan_abund, abund)) %>%
  ungroup() %>%
  filter(class != "Calanoida (ci-cvi)") # Now get rid of it












+ allRegions[allRegions$class== "Calanoida (ci-cvi)",]$abund * relAbund

testData = allRegions %>%
  filter(flowcamCode == "21_08_24_Mar_S04_Z01_1053_250") 

allRegions[allRegions$class== "Calanoida (ci-cvi)",]$abund

allReg

testDivide = allRegions %>%
  group_by()














# Convert it to wide format
allRegionsWide = allRegions %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))  # replace NAs with 0 