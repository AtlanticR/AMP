################################################################################
################################################################################
### ZOOPLANKTON ABUNDANCE

# I have taken this code from zooplanktonCounts.R in Desktop/AMP/DataProcessing
# I am adapting it here to give counts per sample for the Quantitative Assessment



################################################################################
################################################################################
## Read in other scripts

source("DataProcessing/FlowCamPercentAnalyzed.R") # get adjustments for % of sample analyzed
source("DataProcessing/metadataProcessing.R") # get metadata

# Read in spreadsheet with adjustments to taxa names
taxaFixes = read.csv("../AMPDataFiles/extraFiles/taxaCorrections.csv")

################################################################################
## GET THE DATA FILE NAMES
# First, get the names of the dataset folders to search through. 
# This is just how the data were provided to me. They include:
# Gulf 2020, Gulf 2021, Maritimes 2021, NL 2020, NL 2021, Pacific 2020, Pacific 
# June 2021, Pacific March 2021, Pacific Sept 2021.
# Then, get all of the file names in all of those folders. 
# Just because it's useful, I get the full directory name, and just the file name

# Define the directory where data need to be read from 
allFolders = "../AMPDataFiles/AMMP FlowCam Zooplankton Data/"

# Get a list of all the folders that are within that directory
# These are what I refer to as the "datasets" (Gulf 2020, Gulf 2021, etc)
allDataNames =
  list.files(
    allFolders,
    full.names = T, # don't want full directory names
  )

# Now, create a list of the csv files within each folder that contain the zooplankton counts
# There are slightly different file structures based on how the taxonomists did their work
# Some datasets do not have a "Zooplankton Identification Data" folder (they were "cleaned" while ID'ing)
# Therefore we need to check for files in two different directories (but both end in "Classification Summary")

# For reference, these datasets do NOT have a "Zooplankton Identification Data" folder:
# AAMP NL 2020 Zooplankton Data
# AAMP Pacific June 2021 Zooplankton Data
# AAMP Pacific March 2021 Zooplankton Data
# AAMP Pacific Sept 2021 Zooplankton Data

# In some cases I need the full directory names and sometimes I don't
# I found it easiest to just create these separately

## Get full directory names
# These will be stored in a "list of lists". Each dataset will be stored as a list.
# Within that, the files found within each dataset will be included as a list.
dirFull = lapply(allDataNames, function(dataName){
  outDir = list.files(
    # By setting two paths, this will search through both options of whether there is a Zoo... folder
    path = c(paste(dataName, "/Classification Summary", sep = ""),
             paste(dataName, "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = T, # Get full directory names
    pattern = ".csv")
  return(outDir)
})

## Get just the file names (i.e., not the full directory names)
# This is helpful for naming things and matching to the metadata spreadsheet
dirShort = lapply(allDataNames, function(dataName){
  outDir = list.files(
    path = c(paste(dataName, "/Classification Summary", sep = ""),
             paste(dataName, "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = F, # Just get the file names
    pattern = ".csv")
  # Remove the file extension
  outDir = sub('\\.csv$', '', outDir)
  return(outDir)
  
})

################################################################################
## MAKE FUNCTION TO READ IN ZOOPLANKTON COUNT DATA
# This function reads in the data file names, extracts the relevant information 
# from each spreadsheet.
# Function will also remove unnecessary classes (i.e., not zooplankton) and fix
# typos to ensure consistency between datasets.

# Pass in the list of files with full directory name (xl_dataFull) and just file names (xl_dataShort)
speciesDF = function(xlDataFull, xlDataShort) {
  
  # Make an empty list to store the data
  datalist = list()
  
  # The data for Newfoundland 2020 has a different layout and needs to be processed separately!
  # if statement checks if the list of data being passed in matches the Newfoundland 2020 data (the 4th element in the list)
  # Does not compare the ENTIRE list of elements. Just checks if the FIRST element is the same (so there is only one TRUE instead of many)
  if (compare.list(xlDataShort[1], dirShort[[4]][1])){
    
    # Loop through all the data and extract the class (plankton taxa), count (# of 
    # cells in the sample), and particles (# cells/ml)
    for(i in 1:length(xlDataFull)) {  
      df = read.csv(xlDataFull[i], skip = 2) %>% 
        # Keep everything before the "End Metadata Statistics" part
        filter(row_number() < which(Name =='======== End Metadata Statistics ========')) %>%
        # Rename the columns to match the other data files. Format: new = cold
        rename(class = Name, count = Count, particles = Particles...ml)
      # Add sample name as a column
      df$sample = xlDataShort[i]
      # add each element to a new list
      datalist[[i]] = df
    }
    
    # For all other data (not NL 2020)
  } else {
    
    # Loop through all the data and extract the class (plankton taxa), count (# of 
    # cells in the sample), and particles (# cells/ml)
    for(i in 1:length(xlDataFull)){
      
      data = read.csv(xlDataFull[i]) 
      
      # Extract full row of data which contain class, count and particle information 
      class = data[which(str_detect(data[,1], "Class$")), ] 
      count = data[which(str_detect(data[,1], "Count")), ]
      particles = data[which(str_detect(data[,1], "Particles")), ]
      
      # Combine this all into a dataframe
      # Don't want the full row, we only want the 2nd column with the actual data
      # Label it with the file name (without directory/extension)
      df = as.data.frame(cbind("sample" = xlDataShort[i],
                               "class" = class[,2], "count" = count[,2], "particles" = particles[,2]))
      datalist[[i]] = df
    }
  }
  
  # Bind together this list of dataframes into one big data frame
  siteDf = dplyr::bind_rows(datalist) %>%
    # Keep the original class names with no adjustments
    mutate(originalNames = class) %>%
    # Convert counts to numeric
    mutate(count = as.numeric(count)) %>%
    # Merge values to the spreadsheet that has the edited taxa names
    full_join(taxaFixes) %>%
    
    
    # Need to sum the values to remove the duplicates
    # Note: this also removes the Particles and originalName columns
    group_by(sample, newName, originalNames, isCopepod, copepodType) %>%
    # group_by(sample, newName, originalNames) %>%
    dplyr::summarize(count = sum(count)) %>%
    filter(count >0) %>% # Remove ones with a count of 0
    filter(newName != "Remove") # Remove non-zoo particles e.g., "Debris", "Bubbles", etc.
  
  # Return the final corrected dataframe!
  # Will return a df with the sample name, class (taxa), count, particle (count/ml) as columns 
  return(siteDf)
}

################################################################################
## Create dataframes of each dataset

# Run the speciesDF function and create the dataframes for dataset
# This returns a dataframe with columns for sample, class, count, particles
gulf20 = speciesDF(dirFull[[1]], dirShort[[1]]) 
gulf21 = speciesDF(dirFull[[2]], dirShort[[2]])
mar21 = speciesDF(dirFull[[3]], dirShort[[3]])
# This is the one in a different format
nl20 = speciesDF(dirFull[[4]], dirShort[[4]])# %>% 
# Also, there was one file (AAMP_NL_S01_41_20200916PM_250) that had one extra blank line.
# This was above "===END METADATA STATISTICS===". Remove this or else there will be a blank class with a count of zero
#subset(class != "")
nl21 = speciesDF(dirFull[[5]], dirShort[[5]])
nl22 = speciesDF(dirFull[[6]], dirShort[[6]])
pac20 = speciesDF(dirFull[[7]], dirShort[[7]])
pacJun21 = speciesDF(dirFull[[8]], dirShort[[8]])
pacMar21 = speciesDF(dirFull[[9]], dirShort[[9]])
pacSep21 = speciesDF(dirFull[[10]], dirShort[[10]])

################################################################################
# TESTING FOR UNUSUAL SPECIES NAMES
# Just checking if I got all the typos. 

# I also need to find out which dataset they're from to see if it's due to differences 
# in naming conventions between each region
gulf20$dataset = "Gulf 2020"
gulf21$dataset = "Gulf 2021"
mar21$dataset = "Maritimes"
nl20$dataset = "Newfoundland 2020"
nl21$dataset = "Newfoundland 2021"
nl22$dataset = "Newfoundland 2022"
pac20$dataset = "Pacific August 2020"
pacJun21$dataset = "Pacific June 2021"
pacMar21$dataset = "Pacific March 2021"
pacSep21$dataset = "Pacific September 2021"

# Get of each species in whole dataset
taxaCountsEntire = rbind(gulf20, gulf21, mar21, nl20, nl21, nl22, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(originalNames) %>%
  dplyr::summarize(countPerClass = sum(count))

# Get counts BY DATASET 
taxaCountsBay = rbind(gulf20, gulf21, mar21, nl20, nl21, nl22, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(originalNames, dataset) %>%
  dplyr::summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0) # remove any zeroes

# Get counts BY SAMPLE 
taxaCountsSample = rbind(gulf20, gulf21, mar21, nl20, nl21, nl22, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(newName, dataset, sample) %>%
  dplyr::summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0) # remove any zeroes

taxaCountsSampleOrigNames = rbind(gulf20, gulf21, mar21, nl20, nl21, nl22, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(originalNames, newName, sample) %>%
  dplyr::summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0) # remove any zeroes


# write.csv(taxaCountsBay, "taxaCountsBay2.csv")

################################################################################
################################################################################
### Add  hand-counted Chaetognath data for 2021 and 2022

# Chaetognath data for Newfoundland 2021 and 2022 were hand counted
# Some were also run through the FlowCam, but many broke. So these hand-counted values need 
# to be added to the 5mm counts of each file
nl21ChaetData = suppress_warnings(read_excel("../AMPDataFiles/extraFiles/NL_Chaetognath_2021.xlsx")) %>%
  # For everything except the first row: if a value is NA, put 0. If it's not, put 1.
  # This is because chaetognath lengths were recorded. But I am only interested in counts. So if there is any text, it's a count of 1
  mutate(across(-1, ~ifelse(!is.na(.), 1, 0))) %>%
  # tally up how many counts there were per sample
  mutate(count = rowSums(.[-1]),
         newName = "Chaetognatha (juvenile or n.s.)") %>%
  rename(sample = "FlowCam Sample Name") %>%
  # Only select the flowcam names (sample) and the counts
  select(sample, newName, count) %>%
  mutate(PercSampleCleaned = 1,
         PercZooIdentified = 1,
         adjCount = count,
         dataset = "Newfoundland 2021",
         originalNames = "Chaetognatha (juvenile or n.s.)",
         isCopepod = "",
         copepodType = "",
         sample = str_replace(sample, "_5mm", "_250")) # Need to replace last bit of the sample ID so 250 um and 5mm fractions add together properly

nl22ChaetData = suppress_warnings(read_excel("../AMPDataFiles/extraFiles/NL_Chaetognath_2022.xlsx")) %>%
  # For everything except the first row: if a value is NA, put 0. If it's not, put 1.
  # This is because chaetognath lengths were recorded. But I am only interested in counts. So if there is any text, it's a count of 1
  mutate(across(-1, ~ifelse(!is.na(.), 1, 0))) %>%
  # tally up how many counts there were per sample
  mutate(count = rowSums(.[-1]),
         newName = "Chaetognatha (juvenile or n.s.)") %>%
  rename(sample = "FlowCam Sample Name") %>%
  # Only select the flowcam names (sample) and the counts
  select(sample, newName, count) %>%
  mutate(PercSampleCleaned = 1,
         PercZooIdentified = 1,
         adjCount = count,
         originalNames = "Chaetognatha (juvenile or n.s.)",
         isCopepod = "",
         copepodType = "",
         dataset = "Newfoundland 2022")

################################################################################
################################################################################
## Make adjustments to the zooplankton counts
# The counts in the data spreadsheets do NOT represent counts from the entire sample
# The sample was divided into 10 portions (usually) and then a portion was "cleaned" (separated into different components)
# From that, a % of the zooplankton in the subsamples were ID'd (they would only spend __ hours ID'ing)

# AT SOME POINT I SHOULD REPLACE THIS ALL WITH A FUNCTION 
# BUT The naming conventions are inconsistent between samples.
# For some sites, the data files DO match the "Zooplankton Samples" spreadsheet. For others, they do not.
# I will do this manually first and then see what is consistent between them all

######## Maritimes 2021 ########

# Remove the R2 from the file name
# This represents a second run of the sample because the first one had some sort of problem
# For some datasets, "R2" has been removed from data file names. For some it's not. 
Mar21Perc$FlowCamSampleName = str_replace(Mar21Perc$FlowCamSampleName,"_R2", "")

# Join the dataframes of counts with the dataframe of the adjustments
mar21Adj =full_join(mar21, Mar21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  # Add column with adjusted counts
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified)

######## Gulf 2020 ######## 

# For some of these data files, there is no HT/LT in the names. These can still be matched to metadata based on what Jeff Barrell provided

# Join the dataframes of counts with the dataframe of the adjustments
gulf20Adj = full_join(gulf20, Gulf20Perc, by=c("sample" = "FlowCamSampleName")) %>%
  # The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
  # nothing to join to. However, for these, 100% of the sample was analyzed.
  # Therefore, replace these NAs with 1 (100% as a fraction)
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  # This had no associated data
  filter(sample!="AMMP_Gulf_StPeters_2_20200901HT_250UM_2") %>%
  mutate(sample = str_replace(sample, "_R2", "")) %>%
  
  # Gulf 2020 have underscores in the middle of the string AND at the end
  # I need to remove the one at the end 
  mutate(sample = ifelse(endsWith(sample, "_1"), # check if it ends with "_1"
                         gsub('.{2}$', '', sample), # if it does, remove last 2 characters (i.e., "_1")
                         sample)) %>% # if not, just leave it
  mutate(sample = str_replace(sample, "_5mm", ""))

######## Gulf 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
gulf21Adj =full_join(gulf21, Gulf21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  mutate(sample = str_replace(sample, "_5mm", "_250"))

######## Newfoundland 2020 ######## 
nl20Adj =full_join(nl20, Nl20Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified)

########  Newfoundland 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
nl21Adj =full_join(nl21, Nl21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  mutate(sample = str_replace(sample, "_5mm", "_250")) %>%
  rbind(nl21ChaetData) # Now add the hand-counted chaeotognath data

########  Newfoundland 2022 ########

nl22Adj =full_join(nl22, Nl22Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  rbind(nl22ChaetData)


########  Pacific 2020 ########  

# Need to capitalize um so they match
pac20 = pac20 %>%
  mutate(sample = str_replace(sample,"um", "UM"))

pac20Adj =full_join(pac20, Pac20Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified)

######## Pacific June 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
pacJun21Adj =full_join(pacJun21, PacJun21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  mutate(sample = str_replace(sample, "_5mm", "_250um")) %>%
  mutate(sample = str_replace(sample, "_run", ""))

########  Pacific March 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
# NOTE TO SELF: COME BACK TO THIS. I DON'T THINK ALL SAMPLES WERE COMBINED
pacMar21Adj =full_join(pacMar21, PacMar21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified)

######## Pacific Sept 2021 ########

# Join the dataframes of counts with the dataframe of the adjustments
pacSept21Adj =full_join(pacSep21, PacSept21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  mutate(sample = str_replace(sample, "_5mm", ""))

################################################################################
## Merge FlowCam data with the metadata

## First, need to combine Gulf and Pacific datasets together.
# Combine 2020 and 2021 flowcam data
gulfAll = rbind(gulf20Adj, gulf21Adj)
# Combine 2020, 2021, 2022 Newfoundland data together
nlAll = rbind(nl20Adj, nl21Adj, nl22Adj)
# all Pacific datasets
pacAll = rbind(pac20Adj, pacMar21Adj, pacJun21Adj, pacSept21Adj)

# Function to only get the metadata columns that are important to merge (raw metadata files have ~30-40 columns)
reducedMeta = function(metadata) {
  # Get the columns that are actually important to merge
  metadata = metadata %>%
    select(sampleCode, waterVolume, yearStart, monthStart, dayStart, facilityName, myLabel, tidePhase, flowcamCode, depthWaterM, productionType, target, samplingDesign, equipmentType, TowType, netMesh)
  return(metadata)
}

# Run function to get reduced metadata files for each region
gulfMetaRed = reducedMeta(gulfMeta) 

marMetaRed = reducedMeta(marMeta) %>%
  # Maritimes didn't need a flowcamCode since the flowcam files were correctly named.
  # However, for consistency between all datasets (so function below works), I will assign flowcamCode as sampleCode
  # flowcamCode is what will be used to link the species datasets to metadata
  mutate(flowcamCode = sampleCode)

nlMetaRed = reducedMeta(nlMeta)

# Pacific needs a bit of extra processing! Otherwise the species dataframe won't merge correctly with metadata
pacMetaRed = reducedMeta(pacMeta) %>%
  # There are a bunch of samples with NAs where they didn't collect data. Not sure why even included. Remove these
  filter(!is.na(flowcamCode)) %>%
  # Pooled data from March 2021 are from all over the inlet. Replace with NA.
  mutate(myLabel = replace(myLabel, flowcamCode == "AMMP_PA_S04Pooled_202103HT_250UM", NA)) %>%
  mutate(myLabel = replace(myLabel, flowcamCode == "AMMP_PA_S04Pooled_202103LT_250UM", NA)) %>%
  # Make an adjustment for March: if I group by dayStart, I will end up with 4 entries (instead of 2) because HT and LT were collected on multiple days
  # Therefore, for the Pacific March data, just replace the "dayStart" to 3 (instead of 3 and 5)
  mutate(dayStart = ifelse(monthStart == 3, 3, dayStart)) %>%
  # Remember that each sample in the Pacific is made up of two (or more) tows that they combined together. Need to group these into one
  # by NOT including "sampleCode" in the grouping, the waterVolumes per flowcamCode can be summed
  group_by(flowcamCode, myLabel, yearStart, monthStart, dayStart, facilityName, tidePhase, productionType, target, samplingDesign, equipmentType, TowType, netMesh) %>%
  # Adjust the water volume that is the sum of the water volume from tow of both samples
  dplyr::summarize(waterVolume = sum(as.numeric(waterVolume)),
                   depthWaterM = mean(as.numeric(depthWaterM))) %>% # Need to add depthWaterM. Take the average from both tows
  # NOTE: This is not technically correct. But I use 'sampleCode' in future functions/scripts. 
  # I want to keep this as a column name. Therefore create a sampleCode column and set it equal to flowcamCode
  # Pacific often combined data from multiple tows and therefore multiple sampleCodes are combined within one flowcamCode
  mutate(sampleCode = flowcamCode)

# Create function to merge the metadata with the species data from the flowcam
mergeSpeciesMeta = function(metadata, speciesDataset) {
  mergedData = full_join(metadata, speciesDataset, by = c("flowcamCode" = "sample")) %>%
    # Note: waterVolume is already in m^3 not litres like I had previously thought!! Do not divide by 1000.
    # multiply by 4 because tow was split in 4 and this just represents 1/4 of total
    mutate(abund = adjCount / waterVolume * 4) %>%
    # Remove unnecessary columns
    select(-c(PercSampleCleaned, PercZooIdentified, adjCount)) %>%
    # Make depthWaterM numeric
    mutate(depthWaterM = as.numeric(depthWaterM)) %>%
    # Group the stations so 5mm species are added to the regular counts 
    group_by(flowcamCode, newName, originalNames, facilityName, waterVolume, dataset, yearStart, monthStart, dayStart, myLabel, tidePhase, sampleCode, depthWaterM, productionType, target, samplingDesign, equipmentType, TowType, netMesh) %>% 
    # group_by(flowcamCode, newName, facilityName, waterVolume, dataset, yearStart, monthStart, dayStart, myLabel, tidePhase, sampleCode, depthWaterM, productionType, target, samplingDesign, equipmentType, TowType, netMesh) %>% 
    
    # This is needed to combine the 250 fraction with the 5mm fraction
    dplyr::summarize(count = sum(count), abund = sum(abund))
}


# Call the functions to merge the zooplankton counts with the (reduced) metadata
# Need to also add extra columns! These are important for the plotting scripts later on
# facetFactor is used in some scripts since regions in the Atlantic are faceted based on bay (facilityName)
# but Pacific is faceted based on field season (dataset)
gulfMerge = mergeSpeciesMeta(gulfMetaRed, gulfAll) %>%
  mutate(facetFactor = facilityName,
         region = "Gulf",
         ocean = "Atlantic") %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "StPeters", "St. Peters")) %>%
  rename(class = newName) 
# mutate(sampleCode = if_else(sampleCode == "20_09_03_Gulf_S04_Z40_1526_250", "TEST",
#        if_else(sampleCode == "20_09_03_Gulf_S04_Z38_1613_250", "OTHER", sampleCode)))

nlMerge = mergeSpeciesMeta(nlMetaRed, nlAll) %>%
  # For Newfoundland data, I will only be running stats for month/year combos with enough data
  # Only testing tide and station effects for September 2020 data and October 2021. 
  # Just call the rest "Other" and ignore them for most of the stats/plots
  mutate(facetFactor = ifelse(monthStart == 9 & yearStart == 2020, "Sept 2020", 
                              ifelse(monthStart == 10 & yearStart == 2021, "Oct 2021", "Other")),
         region = "Newfoundland") %>%
  filter(flowcamCode != "NO MATCH") %>%
  rename(class = newName)


marMerge = mergeSpeciesMeta(marMetaRed, mar21Adj) %>%
  mutate(facetFactor = facilityName,
         region = "Maritimes",
         ocean = "Atlantic") %>%
  # I think I want to change the name of Sober Island
  mutate(facetFactor = replace(facetFactor, facetFactor == "Sober Island Oyster", "Sober Island")) %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "WhiteHead", "Whitehead")) %>%
  rename(class = newName)

pacMerge = mergeSpeciesMeta(pacMetaRed, pacAll) %>%
  mutate(facetFactor = dataset,
         region = "Pacific",
         ocean = "Pacific") %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific August 2020", "August 2020"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific June 2021", "June 2021"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific March 2021", "March 2021"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific September 2021", "September 2021")) %>%
  rename(class = newName)

# Combine data from all datasets
fcDataForQA = rbind(nlMerge, marMerge, pacMerge, gulfMerge) %>%
  # Revert back to counts in one sample
  # But this also includes the 5mm fraction
  mutate(abundSample = abund / 4 * waterVolume) 














