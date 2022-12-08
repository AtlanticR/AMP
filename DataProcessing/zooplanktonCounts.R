################################################################################
################################################################################
### ZOOPLANKTON ABUNDANCE

## BACKGROUND:
# Zooplankton from the Aquaculture Monitoring Program (AMP) have been sampled from
# four different DFO regions (Pacific, Gulf, Maritimes, Newfoundland) in several
# different years. 
# These samples were run through the FlowCam and taxonomists identified the species.
# These counts need to be read in, and then corrected to represent the # of 
# individuals per cubic meter of seawater

## PURPOSE OF CODE:
# This code is intended to:
# -Read in the necessary information from the data files
# -Ensure consistency between data file entries (e.g., consistent spelling)
# -Adjust the counts by the amount of sample analyzed (from FlowCamPercentAnalyzed.R)
# -Adjust the counts again by volume of water sampled (from metadataProcessing.R)
# -Adjust the counts again by dividing by 4 (since the samples were divided in 
# 4 and only one of the samples was run through the FlowCam)
# -Final units for each taxa are individuals per cubic meter of water (abundance)
# The output will be a dataframe for each region with the abundance of each taxa
# within each sample
# These will be used as the data for statistical analyses/making graphs, etc.

## ADDITIONAL INFO:
# Created by Stephen Finnis 2022
# Data files are not public
################################################################################
################################################################################
## Read in other scripts

source("DataProcessing/FlowCamPercentAnalyzed.R") # get adjustments for % of sample analyzed
source("DataProcessing/metadataProcessing.R") # get metadata

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
    # Remove all underscores and replace them with spaces
    mutate(class = us_to_space(class)) %>%
    # Make entire string lowercase
    mutate(class = str_to_lower(class)) %>%
    # Then capitalize the first letter
    mutate(class = str_to_sentence(class)) %>%
    # Remove instances of multiple spaces between words
    mutate(class = str_squish(class)) %>%
    mutate(class = str_replace(class, "spp", "spp.")) %>%
    # Remove whitespace from end of word
    mutate(class = str_trim(class, side = c("right"))) 
    
  # Remove unwanted classes
  # These do not contain relevant zooplankton data
  # Explanations of these terms are in "Zooplankton Samples" xlsx for each site (in the "Data and Classes" sheet)
  
  # These are the things to remove
  excludeList = c("Benthic", "Bubbles", "Clumped zooplankton", "Clumped zooplankton/debris", "Clumped zooplanlton/debris", "Clumped zooplankton debris", 
                   "Cut images", "Debris", "Debris of zooplankton", "Debris or zooplankton", "Diatom", "Duplicate images", "Extra taxa", "Fragments of zooplankton",
                  "Leftover", "Leftovers")

  typoFixes = list(
    # cypris, actinula, nauplii/nauplius, metanauplius, calyptopsis, furcilia are all larval stages
    
    "Aglantha digitale medusa" = "Aglantha spp. medusa", # some to higher level than others
    "Calanoid" = "Calanoida (unid)", 
    "Calananoida (unid)" = "Calanoida (unid)",
    "Calanoid civ-vi" = "Calanoida (unid)",
    "Calanoid cv-vi" = "Calanoida (unid)",
    "Centropages spp. civ-vi" = "Centropages spp.",
    "Chaetognatha" = "Chaetognatha (juvenile or not specified)", # stage not always specified
    "Chaetognatha juvenile" = "Chaetognatha (juvenile or not specified)",
    
    "Cirripedia cypris" = "Cirripedia cypris/nauplii", 
    "Cirripedia cypris nauplii" = "Cirripedia cypris/nauplii",
    "Cirripedia nauplii" = "Cirripedia cypris/nauplii",
    "Cirripedia nauplius" = "Cirripedia cypris/nauplii",
    
    "Cnidaria actinula" = "Cnidaria actinula/larvae",
    "Cnidaria actinula larvae" = "Cnidaria actinula/larvae",
    "Cnidaria larvae" = "Cnidaria actinula/larvae",
    
    "Ctenophora larva" = "Ctenophora larvae",
    "Cyclopoida spp." = "Cyclopoida",
    "Cyclopoida epibenthic" = "Cyclopoida", # epibenthic not a stage
    
    "Cumacea juvenileadult" = "Cumacea juvenile adult",
    "Decapoda brachyura zoea larvae larvae" = "Decapoda brachyura zoea",
    "Decapoda nonbrachyura zoea" = "Decapoda non-brachyura zoea",
    "Decapoda nonbrachyura zoea larvae" = "Decapoda non-brachyura zoea",
    "Decpoda brachyura zoea" = "Decapoda brachyura zoea",
    
    # But actually I want to get reid of the 'larvae' part!!
    "Decapoda brachyura zoea larvae" = "Decapoda brachyura zoea",
    "Decapoda non-brachyura zoea larvae" = "Decapoda non-brachyura zoea",
    
    # Larval stages of euphausiacea are nauplius, metanauplius, calyptopis, furcilia
    "Euphausiacea calyptopis" = "Euphausiacea larvae (calyptosis/furcilia/nauplii)",
    "Euphausiacea furcillia" = "Euphausiacea larvae (calyptosis/furcilia/nauplii)",
    "Euphausiacea furcilia" = "Euphausiacea larvae (calyptosis/furcilia/nauplii)",
    "Euphausiacea nauplii" = "Euphausiacea larvae (calyptosis/furcilia/nauplii)",
    
    # They were not consistent with these. Combine into Gastropoda/Limacina
    "Gastropoda larvae/lamacina spp." = "Gastropoda/Limacina",
    "Gastropoda limacina spp. larvaeadult" = "Gastropoda/Limacina",
    "Gastropoda larvae" = "Gastropoda/Limacina",
    "Gastropoda limacina spp." = "Gastropoda/Limacina",
    "Gastropoda limacina spp. larvae adult" = "Gastropoda/Limacina",
    
    "Hydrozoa juvenile medusa" = "Hydrozoa juvenile/medusa",
    "Hydrozoa medusa" = "Hydrozoa juvenile/medusa",
    
    # Could not differentiate between trochophore and egg in some cases
    "Invertebrate trochophore larvae" = "Invertebrate egg/trochophore", 
    "Invertebrate trochophore/larvae" = "Invertebrate egg/trochophore",
    "Invertebrate egg" = "Invertebrate egg/trochophore",
    "Invertebrate trochophore" = "Invertebrate egg/trochophore",
    "Invertebrate egg trochophore" = "Invertebrate egg/trochophore",
    
    "Isopoda" = "Isopoda larvae or not specified",
    "Isopoda larvae" = "Isopoda larvae or not specified",
    
    "Monstrilloida" = "Monstrillidae", # Monstrilloida is an order with single family Monstrillidae
    "Monstrillidae spp." = "Monstrillidae",
    
    "Mysidacea juvenileadult" = "Mysidacea",
    "Mysidacea juvenile adult" = "Mysidacea",
    
    " Osteichthyes egg" = "Osteichthyes egg",
    "Osteichthys egg" = "Osteichthyes egg",
    "Osteichthys eggs" = "Osteichthyes egg",
    "Osteichthys larvae" = "Osteichthyes larvae",
    "Osteichthyes eggs" = " Osteichthyes egg",
    "Ostheichthys eggs" = "Osteichthyes egg",
    "Ostracoda spp." = "Ostracoda",
    "Platyhelmenthes nemertea larva" = "Platyhelmenthes nemertea larvae",
    "Platyhelmenthes nemertrea larvae" = "Platyhelmenthes nemertea larvae",
    "Platyhelminthes nemertea larvae" = "Platyhelmenthes nemertea larvae",
    
    "Podon pleopis spp." = "Podon spp./Pleopsis spp.",
    "Podon/pleopis spp." = "Podon spp./Pleopsis spp.",
    "Temopteris spp. juvenile" = "Temopteris spp.",
    
    "Unid zooplankton" = "Zooplankton (unid)",
    "Unidentified calanoida" = "Calanoida (unid)",
    "Unidentified copepoda" = "Copepoda (unid)",
    "Unidentified zooplankton" = "Zooplankton (unid)",
    # "Zooplankton" = "Zooplankton (unid)", # CAREFUL WITH THIS ONE. DOUBLE CHECK.
    "Zooplankton (unid))" = "Zooplankton (unid)")
  
  siteDf = siteDf %>%
    
    # Remove stage information: Might have to adjust this based on the type of analysis!!!
    # For most biodiversity stuff (not size fraction things) I do not want the "spp." stuff
    # Stages are either written as ci-something or civ-something. .* implies "every character after that"
    mutate(class = str_replace(class, "ci-.*", "")) %>%
    mutate(class = str_replace(class, "civ-.*", "")) %>%
    mutate(class = str_replace(class, "cv-.*", "")) %>%
    mutate(class = str_replace(class, "cvi-.*", "")) %>%   
    
    
    subset(!grepl("[0-9]", class)) %>% # remove the "Class 1-9" data
    subset(!(class %in% excludeList)) %>%
    
    # Quentin's recommendation of how to fix species typos
    # Ask what "!!!" means
    # This replaces my old way which was: (every change was a new line)
    # mutate(class = replace(class, class == "Calananoida (unid)", "Calanoida (unid)")) %>%
    mutate(class = recode(class, !!!typoFixes)) %>%
    
    # Removing these classes will mean there is trailing whitespace again. Remove this
    mutate(class = str_trim(class, side = c("right"))) %>%
    
    # I'm not sure why these ones aren't "caught". But manually change a few extras.
    mutate(class = replace(class, class == " Osteichthyes egg", "Osteichthyes egg")) %>%
    mutate(class = replace(class, class == "Calanoida", "Calanoida (unid)")) %>%
    mutate(class = replace(class, class == "Calanoida ", "Calanoida (unid)")) %>%
    mutate(class = replace(class, class == "Calanoid ", "Calanoida (unid)")) %>%
    mutate(class = replace(class, class == "Calanoid", "Calanoida (unid)")) %>%
    mutate(class = replace(class, class == "Centropages", "Centropages spp.")) %>%
    mutate(class = replace(class, class == "Cyclopoida epibenthic", "Cyclopoida")) %>%
    mutate(class = replace(class, class == "Metridia spp. ciii-vi", "Metridia spp.")) %>%
    mutate(class = replace(class, class == "Monstrilloida", "Monstrillidae")) %>%
    
    # Because stage information was removed, there now are duplicates (e.g., Calanus ci-iii and ci-iv are both just "Calanus")
    # Need to sum the values to remove the duplicates
    # Note: this also removes the Particles and originalName columns
    group_by(sample, class) %>%
    summarize(count = sum(count))

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
nl20 = speciesDF(dirFull[[4]], dirShort[[4]]) %>% 
  # Also, there was one file (AAMP_NL_S01_41_20200916PM_250) that had one extra blank line.
  # This was above "===END METADATA STATISTICS===". Remove this or else there will be a blank class with a count of zero
  subset(class != "")
nl21 = speciesDF(dirFull[[5]], dirShort[[5]])
pac20 = speciesDF(dirFull[[6]], dirShort[[6]])
pacJun21 = speciesDF(dirFull[[7]], dirShort[[7]])
pacMar21 = speciesDF(dirFull[[8]], dirShort[[8]])
pacSep21 = speciesDF(dirFull[[9]], dirShort[[9]])

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
pac20$dataset = "Pacific August 2020"
pacJun21$dataset = "Pacific June 2021"
pacMar21$dataset = "Pacific March 2021"
pacSep21$dataset = "Pacific September 2021"

# Get of each species in whole dataset
taxaCountsEntire = rbind(gulf20, gulf21, mar21, nl20, nl21, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(class) %>%
  summarize(countPerClass = sum(count))

# Get counts BY DATASET 
taxaCountsBay = rbind(gulf20, gulf21, mar21, nl20, nl21, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(class, dataset) %>%
  summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0) # remove any zeroes

# Get counts BY SAMPLE 
taxaCountsSample = rbind(gulf20, gulf21, mar21, nl20, nl21, pac20, pacJun21, pacMar21, pacSep21) %>%
  group_by(class, dataset, sample) %>%
  summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0) # remove any zeroes

# write.csv(taxaCountsBay, "taxaCountsBay2.csv")

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
  mutate(sample = str_replace(sample, "_5mm", "_250"))


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
# all Pacific datasets
pacAll = rbind(pac20Adj, pacMar21Adj, pacJun21Adj, pacSept21Adj)

# Function to only get the metadata columns that are important to merge (raw metadata files have ~30-40 columns)
reducedMeta = function(metadata) {
  # Get the columns that are actually important to merge
  metadata = metadata %>%
    select(sampleCode, waterVolume, tideRange, yearStart, facilityName, myLabel, tidePhase, flowcamCode)
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
  # Remember that each sample in the Pacific is made up of two (or more) tows that they combined together. Need to group these into one
  # by NOT including "sampleCode" in the grouping, the waterVolumes per flowcamCode can be summed
  group_by(flowcamCode, myLabel, yearStart, facilityName, tidePhase) %>%
  # Adjust the water volume that is the sum of the water volume from tow of both samples
  summarize(waterVolume = sum(as.numeric(waterVolume))) %>%
  # NOTE: This is not technically correct. But I use 'sampleCode' in future functions/scripts. 
  # I want to keep this as a column name. Therefore create a sampleCode column and set it equal to flowcamCode
  # Pacific often combined data from multiple tows and therefore multiple sampleCodes are combined within one flowcamCode
  mutate(sampleCode = flowcamCode)

# Create function to merge the metadata with the species data from the flowcam
mergeSpeciesMeta = function(metadata, speciesDataset) {
  mergedData = full_join(metadata, speciesDataset,  by = c("flowcamCode" = "sample")) %>%
    # Note: waterVolume is already in m^3 not litres like I had previously thought!! Do not divide by 1000.
    # multiply by 4 because tow was split in 4 and this just represents 1/4 of total
    mutate(abund = adjCount / waterVolume * 4) %>%
    # Remove unnecessary columns
    select(-c(count, PercSampleCleaned, PercZooIdentified, adjCount)) %>%
    # Group the stations so 5mm species are added to the regular counts 
    group_by(flowcamCode, class, facilityName, waterVolume, dataset, yearStart, myLabel, tidePhase, sampleCode) %>% 
    # This is needed to combine the 250 fraction with the 5mm fraction
    summarize(abund = sum(abund))
}


# Call the functions to merge the zooplankton counts with the (reduced) metadata
# Need to also add extra columns! These are important for the plotting scripts later on
# facetFactor is used in some scripts since regions in the Atlantic are faceted based on bay (facilityName)
# but Pacific is faceted based on field season (dataset)
gulfMerge = mergeSpeciesMeta(gulfMetaRed, gulfAll) %>%
  mutate(facetFactor = facilityName,
         region = "Gulf",
         ocean = "Atlantic") %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "StPeters", "St. Peters"))

nlMerge = mergeSpeciesMeta(nlMetaRed, nl20Adj) %>%
  mutate(facetFactor = dataset,
         region = "Newfoundland",
         ocean = "Atlantic") %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Newfoundland 2020", "Southeast Arm 2020"))

marMerge = mergeSpeciesMeta(marMetaRed, mar21Adj) %>%
  mutate(facetFactor = facilityName,
         region = "Maritimes",
         ocean = "Atlantic") %>%
  # I think I want to change the name of Sober Island
  mutate(facetFactor = replace(facetFactor, facetFactor == "Sober Island Oyster", "Sober Island")) %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "WhiteHead", "Whitehead"))

pacMerge = mergeSpeciesMeta(pacMetaRed, pacAll) %>%
  mutate(facetFactor = dataset,
         region = "Pacific",
         ocean = "Pacific") %>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific August 2020", "August 2020"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific June 2021", "June 2021"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific March 2021", "March 2021"))%>%
  mutate(facetFactor = replace(facetFactor, facetFactor == "Pacific September 2021", "September 2021"))