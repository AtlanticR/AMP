#################################################################################
# Divide up ambiguous taxa
# Test dividing up copepods and unid zooplankton a bit better


# Important papers to read for taxonomic combining/harmonization:
# https://www.journals.uchicago.edu/doi/full/10.1086/680962
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6550328/
# https://onlinelibrary.wiley.com/doi/full/10.1111/fwb.13031


#################################################################################

# This has all the plankton data with counts for each file
source("TechReport/DataProcessing/zooplanktonCounts.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("TechReport/Figures/colourPchSchemes.R")

#################################################################################


# Combine all the data together from the different regions
allRegions = rbind(marMerge, nlMerge, pacMerge, gulfMerge)

#################################################################################
#### Divide up Calanoida among the genera

# First, I need to check if there are any samples that do not contain any children Calanoids
# e.g., if there were Calanoida that need to be distributed, but there are no taxa to distribute these to. This would cause problems
samples_noCalanoida = allRegions %>%
  group_by(sampleCode) %>%
  summarize(has_Calanoida = any(copepodType == "Calanoida child")) %>%
  filter(!has_Calanoida) %>%
  select(sampleCode)
# Size of data frame is 0! I'm all good to distribute these!


# Get just the calanoid abundances for each sample
# This is what needs to be redistributed among each of the Calanoid subtypes
# Result will be a dataframe for all regions, with 1 column of sample name with other column as calanoid abund in that sample
calSamAbund = allRegions %>%
  filter(class == "Calanoida parent") %>%
  group_by(sampleCode) %>%
  summarize(calan_abund = sum(abund))

# Distribute those calanoid abundances based on the relative abundance of each calanoid genera 
# Do this by relative abundance within each sample
calan_distribute = allRegions %>%
  group_by(sampleCode, copepodType) %>%
  mutate(relAbundCal = ifelse(copepodType == "Calanoida child", abund/sum(abund[copepodType == "Calanoida child"]), NA_real_)) %>%
  full_join(calSamAbund) %>%
  mutate(abundPlusCal = ifelse(copepodType == "Calanoida child" & !is.na(calan_abund), abund + relAbundCal*calan_abund, abund)) %>%
  ungroup() %>%
  filter(class != "Calanoida parent") # Now get rid of it


####################################################################################################################################
#### Divide up the Cyclopoida among the genera

# However, first, need to do some exploring

# Find the samples that do not contain genera of Cyclopoida
samples_noCyclchildren = calan_distribute %>%
  group_by(sampleCode) %>%
  summarize(has_CyclChildren = any(copepodType == "Cyclopoida child"))

# Find the ones that DO contain Cyclopoida
# there are 38
sample_CyclParent = calan_distribute %>%
  group_by(sampleCode) %>%
  summarize(has_CyclParent = any(class == "Cyclopoida parent"))

# The ones that are concerning DO have cylopoida parent, but do not have cyclopoida children
# The bad ones: has_CyclChildren == FALSE but also has_CyclParent == TRUE
cyclProblems = full_join(samples_noCyclchildren, sample_CyclParent) %>%
  filter(has_CyclParent == TRUE & has_CyclChildren == FALSE)

# Here are my problem taxa
# 20_09_01_Gulf_S04_Z38_1434_250 # St. Peters
# 20_09_03_Gulf_S04_Z40_0942_250 # St. Peters
# AMMP_PA_S04_W01_20200830LT_236UM # Pac Aug 2020

# See what the other Cyclopoida children are in St. Peters samples
stPcyclProbs = calan_distribute %>%
  filter(facetFactor == "St. Peters") %>%
  filter(copepodType == "Cyclopoida child" | 
           (sampleCode == "20_09_01_Gulf_S04_Z38_1434_250" & class == "Cyclopoida parent"))
# They are ALL Oithona spp.. Therefore I will convert Cyclopoida to Oithona (later step below)

# Find what the other Cyclopoida children are for the Pacific sample
pacCycl = calan_distribute %>%
  filter(facetFactor == "August 2020") %>%
  filter(copepodType == "Cyclopoida child" | 
           (sampleCode == "AMMP_PA_S04_W01_20200830LT_236UM" & class == "Cyclopoida parent"))

# It turns out there are 3 types of Cyclopoida in the Pac August 2020 dataset: Corycaeidae, Oithona spp. and Oncaeidae
# So, we should find the relative abundance of each, and then redistribute the Cyclopoida among those
relPacCycl = calan_distribute %>%
  filter(facetFactor == "August 2020" & copepodType == "Cyclopoida child") %>%
  group_by(class) %>%
  summarize(abundPerClass = sum(abundPlusCal)) %>%
  mutate(relAbundPacCyl = abundPerClass / sum(abundPerClass))

# Redistribute the Cyclopoida parent among the children
pacCyclReplace = pacCycl %>%
  filter(sampleCode == "AMMP_PA_S04_W01_20200830LT_236UM" & class == "Cyclopoida parent") %>%
  bind_rows(slice(., rep(1,2))) %>%
  mutate(class = relPacCycl$class) %>%
  mutate(abundPlusCal = abundPlusCal*relPacCycl$relAbundPacCyl)


####################################################################################################################################
# Now make the adjustments to the dataframe

# Take calan_distribute, 
# For St. Peters, rename that one Cyclopoida to Oithona
# For Pacific, remove that cyclopoida entry, and then add the new rows just discovered, where things have been redistributed

calan_distAdj = calan_distribute %>%
  mutate(class = ifelse(sampleCode == "20_09_01_Gulf_S04_Z38_1434_250" & class == "Cyclopoida parent", "Oithona spp.", class)) %>%
  filter(sampleCode != "AMMP_PA_S04_W01_20200830LT_236UM" | class != "Cyclopoida parent") %>% # remove the Cyclopoida from that one sample
  bind_rows(pacCyclReplace) # Now add the rows that had the redistributed Cyclopoida


#################################
### OK NOW REDISTRUBTE

cyclSamAbund = allRegions %>%
  filter(class == "Cyclopoida parent") %>%
  group_by(sampleCode) %>%
  summarize(cycl_abund = sum(abund))


cycl_distribute = calan_distAdj %>%
  group_by(sampleCode, copepodType) %>%
  mutate(relAbundCycl = ifelse(copepodType == "Cyclopoida child", abundPlusCal/sum(abundPlusCal[copepodType == "Cyclopoida child"]), NA_real_)) %>%
  full_join(cyclSamAbund) %>%
  mutate(abundPlusCycl = ifelse(copepodType == "Cyclopoida child" & !is.na(cycl_abund), abundPlusCal + relAbundCycl*cycl_abund, abundPlusCal)) %>%
  ungroup() %>%
  filter(class != "Cyclopoida parent") # Now get rid of it






# Get just the copepod abundances for each sample
# This is what needs to be redistributed among each of the Calanoid subtypes
# Result will be a dataframe for all regions, with 1 column of sample name with other column as calanoid abund in that sample
copSamAbund = allRegions %>%
  filter(class == "Copepoda parent") %>%
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
  filter(class != "Copepoda parent") # Now get rid of it


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
  filter(class != "Zooplankton (unid)") %>% # Now get rid of it 
  # Remove epibenthic taxa. These should not be part of the analysis
  
  # Make a few  adjustments to the epibenthic taxa
  # Caprellidae: Renamed to Amphipoda-epibenthic (in the taxa excel sheet)
  # Cyclopoida_epibenthic_Civ-vi from Maritimes 2021: renamed to Cyclopoida- epibenthic in the taxa excel sheet
  # Harpacticoida: renamed to Harpacticoida- epibenthic in the taxa Excel sheet
  # For the next two, I knew they were benthic from looking at the taxa list prepared by the taxonomists (in the "Data & Classes" tab of the Flowcam spreadsheets)
  # Amphipoda from Gulf 2020 (from taxa list) and
  # Isopoda Gulf 2020
  mutate(class = ifelse(class ==  "Amphipoda" & region == "Gulf" & yearStart == 2020, "Amphipoda- epibenthic", class)) %>%
  mutate(class = ifelse(class == "Isopoda (larvae)" & region == "Gulf" & yearStart == 2020, "Isopoda- epibenthic", class))

# All of this added a lot of extra columns. I need to remove these. 
rem.cols = c("isCopepod", "copepodType", "relAbundCal", "relAbundCycl", "relAbundCop", "relAbundZoo", "abund", "calan_abund", "cycl_abund", "copep_abund", "zooUnid_abund",
             "abundPlusCal", "abundPlusCycl", "abundPlusCop")

mar = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Maritimes") %>%
  rename(abund = abundPlusZoo) %>%
  ungroup()

gulf = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Gulf") %>%
  #filter(sampleCode != "20_09_01_Gulf_S04_Z39_0858_250") %>%
  rename(abund = abundPlusZoo) %>%
  ungroup()

pac = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Pacific") %>%
  filter(sampleCode != c("AMMP_PA_S04W15_20210610HT_250um")) %>%
  filter(sampleCode != c("AMMP_PA_S04W01_20210611HT_250um")) %>%
  rename(abund = abundPlusZoo) %>%
  ungroup() 

nl = zoo_distribute %>%
  select(-all_of(rem.cols)) %>%
  filter(region == "Newfoundland") %>%
  rename(abund = abundPlusZoo) %>%
  ungroup()

# Rename these variables too, in case they come up
gulfMerge = gulf 
marMerge = mar
nlMerge = nl 
pacMerge = pac 
