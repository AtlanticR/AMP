################################################################################
################################################################################
##### QUANTITATIVE ASSESSMENT

# Created by Stephen Finnis
# May 2023

################################################################################
#### GOALS AND BACKGROUND

# For AMP, taxonomists have also ID'd zooplankton by traditional microscopy
# This is so we can make comparisons between FlowCAM and microscopy

# We call call these comparisons the "Quantitative Analysis" (QA)
# Alternatively, samples for microscopy are called the QA samples

# Recall that in the field, a zooplankton tow was obtained in many regions,
# sites, stations, etc.
# Each tow was then split in 4: 
# 1 subsample run through FlowCam
# 1 subsample for quantitative analysis (microscopy)
# 1 subsample as a backup
# 1 subsample as... I forget!

# Note that not every single FlowCAM sample has a corresponding microscopy sample
# But there are for most!!
# In a few cases, several samples were combined (i.e., microscopy sample includes
# the plankton from 3 different samples combined together) 

# This code is meant to read in (just) the IDs from the QA and then match them 
# To the FlowCAM codes (i.e., how the samples are labelled in the files from the 
# FlowCAM outputs). 
# Then, these FlowCAM codes will be matched to the sampleCodes from the metadata
# (which are also sometimes different)

################################################################################
#### NOTES ABOUT THE QA SAMPLES

### Newfoundland

## 2020
# No issues. All 10 Sept 2020 samples have matches

## 2021
# There are 20 QA samples
# Not sure how they decided which samples to choose (they have >20 for the year)

# One QA sample has the wrong time in the formatting of the ID
# Written as 21-7-7-NL-S1-Z17-1506-250. Time is 1056 (not 1506)

### Gulf

## 2020 
# All 26 St Peters samples have matches
# All 3 Malpeque samples have matches

## 2021
# All 6 Cocagne samples have matches

### Maritimes

## 2021 (no data collected in 2020)
# S01: Sober Island, S02: Argyle, S03: Whitehead, S04: Country Harbour

# Some S03/S04 had direct matches (i.e., one QA sample for one FlowCam sample)
# These were easy to decipher from listed times and QA IDs
# For S01 stations, they combined 3 samples different stations
# e.g., combined Z01/Z02/Z03 into a single QA sample, Z04/05/06, Z07/08/09
# There was also a "21-08-MAR-S02-LOW" sample and "21-08-MAR-S02-HIGH"
# I guessed that these contained Z01-03 samples and matched based on those and S02. 
# This meant there are 9 S02 (Argyle) samples from the Maritimes not in the QA. After elimination, they do not have matches because:
# 6 are from September (not August)
# 2 are mid-rising tides (not high or low)
# 1 is Aug 30 (not Aug 31 like the others)

# Lastly, there are 6 S03 (Whitehead) stations from Maritimes without matches. All other S03s had direct matches.
# There are 4 S04 (Country Harbour) stations without matches. All other S04s had direct matches

### Pacific

## 2020
# There are only 2 samples with QA data
# Written as "High Tide 20-073-001" and "Low Tide 20-073-002"
# I am still trying to figure out which samples those refer to. TBD

## 2021
# Only have QA data for June 2021. Only for June 9 and 10 (not 11)
# Be careful when matching these, because tide phase and date were not included in QA IDs
# They were added as separate columns. Need to be included

## NOTE: As of June 9, 2023 we will only be using a select few samples
# 10 from St. Peters, 10 from NL (2020), 10 from NL (2021), and 10 from Pacific
# To make analysis more consistent
# These are identified in the 'selectForAnalysis' column in QA Pairings spreadsheet

################################################################################
#### SETUP

# Read in all required R packages
source("DataProcessing/metadataProcessing.R") 

# Read in data of FlowCam Counts
# Note that this is basically the same as ZooplanktonCount.R from previous project
# But I've just made a few minor adjustments to only give COUNTS, not abundances
# Also I don't want to mess up that old script! 
source("QuantitativeAssessment/flowcamCountQA.R")

# Remove scientific notation
options(scipen=999)

# Read in the Excel spreadsheet with the QA IDs
# Read in spreadsheet with adjustments to taxa names

# Files are read relative to my R Project (within "AMP"), which is separate from where the data files are stored (AMPDataFiles)
# ".." means "go back a directory"

# Read in the file I created that matches the QA samples (and the IDs used by the taxonomists) with the FlowCam IDs
# Note the FlowCAM IDs are NOT the same as the sampleCodes in the metadata
# Those will have to be obtained by joining/merging the IDs again with separate files
# THIS IS WHERE WE SELECT WHICH SAMPLES WE ARE LOOKING AT
qaID = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/selectionsQuantitativeAssessmentPairings.xlsx")

# Read in the file that has the adjustments to the QA taxa names (to make sure they're consistent)
qaTaxaChanges = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/quantAssess_taxaChanges.xlsx") %>%
  # Only keep the important column names (not all my draft labelling)
  select(taxStage, newName)

# Read in the file that has the adjustments to the FLOWCAM taxa names
# Note, these are very very slightly different than what I was using for the previous tech report
fcTaxaChanges = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/flowcam_taxaChanges.xlsx")

# Note that I have renamed the QA Excel file names from the originals
# File names are self explanatory. I did not change the sheet names

# Gulf, Maritimes and NL samples were done by Huntsman. Their format is slightly different
# I am using the term "countTot" to refer to the total count in the entire sample!
# I will use "abundance" when it is converted to individuals in seawater
qaGulf2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/Gulf_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  # renaming is "new" = "old"
  rename("countTot" = `Abundance in total sample`,
         "countSample" = `Count in subsample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaMar2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/Mar_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  rename("countTot" = `Abundance in total sample`,
         "countSample" = `Count in subsample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaNL2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/NL_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  rename("countTot" = `Abundance in total sample`,
         "countSample" = `Count in subsample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaNLGulf2020 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/NL_Gulf_2020_QA_Zooplankton_AMP.xlsx", sheet = "ID raw data") %>%
  rename("countTot" = `Abundance in total sample`,
         "countSample" = `Count in split`,
         "qaSampleID" = `Unique sample name`,
         "Taxon" = "Species") %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))


# Pacific samples (done by Biologica) have a slightly different format
qaPac2020 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/PAC_2020_QA_Zooplankton_AMP.xlsx", sheet = "3. Data-Long") %>%
  rename("countTot" = "Count") %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
  
qaPac2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/PAC_2021_QA_Zooplankton_AMP.xlsx", sheet = "4. Biologica Data-Long", skip = 5) %>% # ignore first 5 lines with background info
  rename("countTot" = `Total Abundance`,
         "countSample" = `Raw Abundance`) %>%
  filter(countTot != "n/a" | countSample !="n/a") %>% # remove this NA where ctenophora fragments were found, but no counts provided
  mutate(countTot = as.numeric(countTot),
         countSample = as.numeric(countSample), # now I can make the value a numeric
         # IDs for Pacific samples have to be concatenated with Tide and Date info, or they can't be distinguished
    qaSampleID =  paste(`Client Sample ID`, Tide, `Date Sampled`, sep = "_"), .before = Fraction) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))


# Get the FlowCam data. Join my df with the updated taxa names to it
flowCamData = qaID %>%
  left_join(fcDataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  # Only select the samples we're interested in
  filter(selectForAnalysis == "Yes") %>%
  left_join(fcTaxaChanges) %>% 
  mutate(newName = ifelse(is.na(newName) & originalNames == "Chaetognatha (juvenile or n.s.)", "Chaetognatha", newName)) %>%
  mutate(newName = ifelse(is.na(newName) & originalNames == "Pseudocalanus spp Civ-vi ", "Pseudocalanus spp.", newName)) %>%
  # Only select the relevant columns otherwise there are too many
  select(newName, regionYear, FlowCamID, qaSampleID, count, abund, waterVolume) %>%
  # Need to make adjustments: a few taxa names were combined within each sample. Make sure these are added together.
  group_by(newName, regionYear, FlowCamID, qaSampleID, waterVolume) %>%
  summarize(count = sum(count), 
            abund = sum(abund), # this is abundance in seawater i.e., ind per ml
            countTot = abund * waterVolume / 4) %>%  # this is total counts in 1 subsample
  mutate(type = "FC") %>%
  distinct() # remove duplicates that were created from renaming and joining taxa
  
# Extract the water volume for each sample from the FlowCam data because I need to add this to the QA data below
waterVolSamples = flowCamData %>%
  ungroup() %>%
  select(waterVolume, FlowCamID) %>%
  distinct()

# Combine all Quantitative Assessment dataframes (except qaPac2020)
allQAData = bind_rows(qaGulf2021, qaMar2021, qaNL2021, qaNLGulf2020, qaPac2021, qaPac2020) %>%
  select(qaSampleID, countTot, Taxon, taxStage, countSample) %>%
  # Join with the file that matches FlowCam IDs to Quantitative Assessment IDs
  left_join(qaID, by = "qaSampleID") %>%
  # Only select the samples we are interested in
  filter(selectForAnalysis == "Yes") %>%
  # Now replace the old names with my new updated names
  left_join(qaTaxaChanges) %>%
  # Select only the relevant columns again
  select(FlowCamID, countSample, regionYear, qaSampleID, newName, countTot) %>%
  # Need to make adjustments: a few taxa names were combined within each sample. Make sure these are added together.
  group_by(newName, FlowCamID, regionYear, qaSampleID) %>%
  summarize(count = sum(countSample),
            countTot = sum(countTot)) %>%
  left_join(waterVolSamples, by = "FlowCamID") %>%
  mutate(abund = countTot * 4 /waterVolume,
         type = "QA")
  
# Combine the FlowCam and QA data into 1 data frame
fcQaDf = rbind(allQAData, flowCamData)



########################################################
### Adjustments to the taxa list

# Things to fix:
# From FLOWCAM: Possibly redistribute 'Zooplankton (unid)' and 'Calanoida (unid)'
# From QA: redistribute damaged Acartia spp., Pseudocalanus spp., Temora spp.
# Redistribute decapoda indet.
# Remove egg mass

# redistribute_abundances = function(df, damaged_taxa, target_taxa, target_taxa2){
# 
#   # filter the data frame for damaged Acartia
#   df_damaged = df %>%
#     filter(newName == damaged_taxa)
#   
#   # Calculate the total abunndance of damaged Acartia for each sample
#   df_damaged_total = df_damaged %>%
#     group_by(FlowCamID, type) %>%
#     summarize(total_abund_damaged = sum(abund))
#     
#   # Join the total abundances back tot he original dataframe
#   df = df %>%
#     left_join(df_damaged_total, by = c("FlowCamID", "type"))
#   
#   # Calculate the proportion of Acartia spp. and Calanoida ci-ciii within each sample
#   df_total = df %>%
#     filter(newName %in% c(target_taxa, target_taxa2)) %>%
#     group_by(FlowCamID, type) %>%
#     summarize(total_abund_total = sum(abund))
#     
#   # Join the total abundance back to the original dataframe
#   df = df %>%
#     left_join(df_total, by = c("FlowCamID","type"))
#   
#   # Calculate the relative abundance of regular Acartia and juvenile Calanoids
#   df = df %>%
#     mutate(prop_acartia = ifelse(newName == target_taxa, abund/total_abund_total, 0),
#            prop_calanoida_juv = if_else(newName == target_taxa2, abund/total_abund_total, 0))
#   
#   
#   # Calculate the redistributed abundances
#   df = df %>%
#     mutate(abund2 = if_else(newName == target_taxa & !is.na(total_abund_damaged), abund + prop_acartia*total_abund_damaged, 
#                             if_else(newName == target_taxa2 & !is.na(total_abund_damaged), abund + prop_calanoida_juv*total_abund_damaged, abund)))
# 
# 
# }
# 
# 
# df2= redistribute_abundances(fcQaDf, "Acartia spp. DAMAGED", "Acartia spp.", "Calanoida ci-ciii")
# df2= redistribute_abundances(fcQaDf, "Temora spp. DAMAGED", "Temora spp.")
# 



#######
# TEST

redistribute_abundances <- function(df, damaged_taxa, target_taxa){
  
  # Filter the data frame for the damaged taxa
  df_damaged = df %>%
    filter(newName == damaged_taxa)
  
  # Calculate the total abundance of the damaged taxa for each sample
  df_damaged_total = df_damaged %>%
    group_by(FlowCamID, type) %>%
    summarize(total_abund_damaged = sum(abund))
  
  # Join the total abundance back to the original dataframe
  df = df %>%
    left_join(df_damaged_total, by = c("FlowCamID", "type"))
  
  # Calculate the total abundance of the target taxa for each sample
  df_total <- df %>%
    filter(newName %in% target_taxa) %>%
    group_by(FlowCamID, type) %>%
    summarize(total_abund_target = sum(abund))
  
  # Join the total abundance back to the original dataframe
  df = df %>%
    left_join(df_total, by = c("FlowCamID", "type"))
  
  # Calculate the relative abundance of the target taxa within each sample
  for (taxon in target_taxa) {
    df = df %>%
      mutate(!!paste0("prop_", taxon) := if_else(newName == taxon, abund / total_abund_target, 0))
  }
  
  # Calculate the redistributed abundances
  for (taxon in target_taxa) {
    df = df %>%
      mutate(abund = if_else(newName == taxon & !is.na(total_abund_damaged),
                             abund + get(paste0("prop_", taxon)) * total_abund_damaged, abund))
  }
  
  # Remove the damaged taxa from the dataframe
  df = df %>% filter(newName != damaged_taxa)
  
  # Drop the unnecessary columns
  df = df %>% select(-c(total_abund_damaged, total_abund_target, starts_with("prop_")))
  
  return(df)
}

# Example usage for multiple target taxa
df2 = redistribute_abundances(fcQaDf, "Acartia spp. DAMAGED", c("Acartia spp.", "Calanoida ci-ciii"))


# Redistribute abundances of unidentified zooplankton between all other taxa present in the dataset
df2 = redistribute_abundances(fcQaDf, "Zooplankton (unid)", unique(fcQaDf$newName[fcQaDf$newName != "Zooplankton (unid)"]))


df2 = redistribute_abundances(df2, "Temora spp. DAMAGED", c("Temora spp.", "Calanoida ci-ciii"))


fcQaDf = df2

