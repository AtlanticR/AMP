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
## GET THE RELEVANT R PACKAGES

# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("ggplot2", "ggrepel", "ggthemes", "jcolors", "leaflet", "mapr", "mapview", "readxl", "stringr",
             "tidyr", "tools", "useful", "vegan", "wpa")
ipak(packages)

################################################################################
## Read in other scripts

setwd("C:/Users/FINNISS/Desktop/AMPCode")

# See source files for a full explanation of the data
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
allFolders = "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data/"

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
dirFull = list()

for(i in 1:length(allDataNames)){
  dirFull[[i]] = list.files(
    # By setting two paths, this will search through both options of whether there is a Zoo... folder
    path = c(paste(allDataNames[i], "/Classification Summary", sep = ""),
      paste(allDataNames[i], "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = T, # Get full directory names
    pattern = ".csv")
}

## Get just the file names (i.e., not the full directory names)
# This is helpful for naming things and matching to the metadata spreadsheet
dirShort = list()

for(i in 1:length(allDataNames)){
  dirShort[[i]] = list.files(
    path = c(paste(allDataNames[i], "/Classification Summary", sep = ""),
             paste(allDataNames[i], "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = F, # Just get the file names
    pattern = ".csv")
  # Remove the file extension
  dirShort[[i]] = sub('\\.csv$', '', dirShort[[i]])
}

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
  mutate(class = str_replace(class, "spp", "spp."))
  
# Remove unwanted classes
# These do not contain relevant zooplankton data
# Explanations of these terms are in "Zooplankton Samples" xlsx for each site (in the "Data and Classes" sheet)
siteDf = siteDf %>%
  subset(!grepl("[0-9]", class) & # remove the "Class 1-9" data
           class != "Benthic" &
           class != "Bubbles" &
           class != "Clumped zooplankton" &
           class != "Clumped zooplankton/debris" &
           class != "Clumped zooplankton debris" &
           class != "Cut images" &
           class != "Debris" &
           class != "Debris or zooplankton" & 
           class != "Diatom" &
           class != "Duplicate images" &
           class != "Extra taxa" &
           class != "Fragments of zooplankton" &
           class != "Leftover" &
           class != "Leftovers") %>%
  
  # Fix typos in classes
  # Also ensure the classes are consistent between all locations
  mutate(class = replace(class, class == "Calananoida (unid)", "Calanoida (unid)")) %>%
  mutate(class = replace(class, class == "Calanoid civ-vi", "Calanoida civ-vi")) %>%
  mutate(class = replace(class, class == "Calanoid cv-vi", "Calanoida cv-vi")) %>%
  mutate(class = replace(class, class == "Centropages spp civ-vi", "Centropages civ-vi")) %>%
  mutate(class = replace(class, class == "Cirripedia nauplius", "Cirripedia nauplii")) %>%
  mutate(class = replace(class, class == "Ctenophora larva", "Ctenophora larvae")) %>%
  mutate(class = replace(class, class == "Cyclopoida spp.", "Cyclopoida")) %>%
  mutate(class = replace(class, class == "Cumacea juvenileadult", "Cumacea juvenile adult")) %>%
  mutate(class = replace(class, class == "Decapoda brachyura zoea larvae larvae", "Decapoda brachyura zoea larvae")) %>%
  mutate(class = replace(class, class == "Decapoda nonbrachyura zoea", "Decapoda non-brachyura zoea")) %>%
  mutate(class = replace(class, class == "Decapoda nonbrachyura zoea larvae", "Decapoda non-brachyura zoea larvae")) %>%
  mutate(class = replace(class, class == "Decpoda brachyura zoea", "Decapoda brachyura zoea")) %>%
  mutate(class = replace(class, class == "Gastropoda limacina spp. larvaeadult", "Gastropoda limacina spp. larvae adult")) %>%
  mutate(class = replace(class, class == "Monstrilloida", "Monstrilloida spp.")) %>%
  mutate(class = replace(class, class == "Mysidacea juvenileadult", "Mysidacea juvenile adult")) %>%
  mutate(class = replace(class, class == "Osteichthys egg", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Osteichthys eggs", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Osteichthys larvae", "Osteichthyes larvae")) %>%
  mutate(class = replace(class, class == "Osteichthyes eggs", " Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Ostheichthys eggs", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Ostracoda spp.", "Ostracoda")) %>%
  mutate(class = replace(class, class == "Platyhelmenthes nemertea larva", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Platyhelmenthes nemertrea larvae", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Platyhelminthes nemertea larvae", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Unid zooplankton", "Zooplankton (unid)")) %>%
  mutate(class = replace(class, class == "Zooplankton", "Zooplankton (unid)")) %>%
  mutate(class = replace(class, class == "Zooplankton (unid))", "Zooplankton (unid)"))
  
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
nl20 = speciesDF(dirFull[[4]], dirShort[[4]]) # This is the one in a different format
nl21 = speciesDF(dirFull[[5]], dirShort[[5]])
pac20 = speciesDF(dirFull[[6]], dirShort[[6]])
pacJun21 = speciesDF(dirFull[[7]], dirShort[[7]])
pacMar21 = speciesDF(dirFull[[8]], dirShort[[8]])
pacSep21 = speciesDF(dirFull[[9]], dirShort[[9]])

# Check for consistency between classes
checkClass = data.frame(unique(sort(c(gulf20$class, gulf21$class, mar21$class, nl21$class, pac20$class, pacJun21$class, pacMar21$class, pacSep21$class))))

################################################################################
## Make adjustments to the zooplankton counts
# The counts in the data spreadsheets do NOT represent counts from the entire sample
# The sample was divided into 10 portions (usually) and then a portion was "cleaned" (separated into different components)
# From that, a % of the zooplankton in the subsamples were ID'd (they would only spend __ hours ID'ing)

# The naming conventions are inconsistent between samples.
# For some sites, the data files DO match the "Zooplankton Samples" spreadsheet. For others, they do not.
# It is not easy to create a function to do all of these edits since some things are
# not the same between files (e.g., ending with "250UM" vs "250" vs "250um") and this is instead done manually.

######## Maritimes 2021 ########

# Remove the R2 from the file name
# This represents a second run of the sample because the first one had some sort of problem
# For some datasets, "R2" has been removed from data file names. For some it's not. 
Mar21Perc$FlowCamSampleName = str_replace(Mar21Perc$FlowCamSampleName,"_R2", "")

# Join the dataframes of counts with the dataframe of the adjustments
mar21adj =full_join(mar21, Mar21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  # Add column with adjusted counts
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified)

######## Gulf 2020 ######## 

# For some of these data files, there is no HT/LT in the names. These can still be matched to metadata based on what Jeff Barrell provided

# Join the dataframes of counts with the dataframe of the adjustments
gulf20Adj = full_join(gulf20, Gulf20Perc, by=c("sample" = "FlowCamSampleName"))

# The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
# nothing to join to. However, for these, 100% of the sample was analyzed.
# Therefore, replace these NAs with 1 (100% as a fraction)
gulf20Adj$PercSampleCleaned[is.na(gulf20Adj$PercSampleCleaned)] = 1
gulf20Adj$PercZooIdentified[is.na(gulf20Adj$PercZooIdentified)] = 1

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
gulf20Adj$adjCount = gulf20Adj$count / gulf20Adj$PercSampleCleaned / gulf20Adj$PercZooIdentified

# Remove this one sample because there's no associated data
gulf20Adj = subset(gulf20Adj, sample!="AMMP_Gulf_StPeters_2_20200901HT_250UM_2")

# Fix up sample names. These endings can now be removed.
gulf20Adj$sample = str_replace(gulf20Adj$sample,"_R2", "")
gulf20Adj$sample = str_replace(gulf20Adj$sample,"_1", "")
gulf20Adj$sample = str_replace(gulf20Adj$sample,"_5mm", "")

######## Gulf 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
gulf21Adj =full_join(gulf21, Gulf2021Perc, by=c("sample" = "FlowCamSampleName"))

# The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
# nothing to join to. However, for these, 100% of the sample was analyzed.
# Therefore, replace these NAs with 1 (100% as a fraction)
gulf21Adj$PercSampleCleaned[is.na(gulf21Adj$PercSampleCleaned)] = 1
gulf21Adj$PercZooIdentified[is.na(gulf21Adj$PercZooIdentified)] = 1

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
gulf21Adj$adjCount = gulf21Adj$count / gulf21Adj$PercSampleCleaned / gulf21Adj$PercZooIdentified

# Remove _5mm file name ending and replace with _250. This will associate the 5mm size fraction with the rest
# of the sample from that location
gulf21Adj$sample = str_replace(gulf21Adj$sample,"_5mm", "_250")

######## Newfoundland 2020 ######## 
nl20Adj =full_join(nl20, Nl2020Perc, by=c("sample" = "FlowCamSampleName"))

# Add new column with adjusted counts
nl20Adj$adjCount = nl20Adj$count / nl20Adj$PercSampleCleaned / nl20Adj$PercZooIdentified

########  Newfoundland 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
nl21Adj =full_join(nl21, Nl21Perc, by=c("sample" = "FlowCamSampleName")) %>%
  # The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
  # nothing to join to. However, for these, 100% of the sample was analyzed.
  # Therefore, replace these NAs with 1 (100% as a fraction)
  mutate(PercSampleCleaned = replace_na(PercSampleCleaned, 1)) %>%
  mutate(PercZooIdentified = replace_na(PercZooIdentified, 1)) %>%
  # Create a column with adjusted counts
  mutate(adjCount = count / PercSampleCleaned / PercZooIdentified) %>%
  # Remove _5mm file name ending and replace with _250. This will associate the 5mm size fraction with the rest
  # of the sample from that location
  mutate(sample = str_replace(sample, "_5mm", "_250"))


########  Pacific 2020 ########  

# Need to capitalize um so they match
pac20$sample = str_replace(pac20$sample,"um", "UM")

# Join the dataframes of counts with the dataframe of the adjustments
pac20Adj =full_join(pac20, Pac2020Perc, by=c("sample" = "FlowCamSampleName"))

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
pac20Adj$adjCount = pac20Adj$count / pac20Adj$PercSampleCleaned / pac20Adj$PercZooIdentified

######## Pacific June 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
pacJun21Adj =full_join(pacJun21, PacJun2021Perc, by=c("sample" = "FlowCamSampleName"))

# The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
# nothing to join to. However, for these, 100% of the sample was analyzed.
# Therefore, replace these NAs with 1 (100% as a fraction)
pacJun21Adj$PercSampleCleaned[is.na(pacJun21Adj$PercSampleCleaned)] = 1
pacJun21Adj$PercZooIdentified[is.na(pacJun21Adj$PercZooIdentified)] = 1

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
pacJun21Adj$adjCount = pacJun21Adj$count / pacJun21Adj$PercSampleCleaned / pacJun21Adj$PercZooIdentified

# Remove _5mm_run file name ending and replace with _250. This will associate the 5mm size fraction with the rest
# of the sample from that location
pacJun21Adj$sample = str_replace(pacJun21Adj$sample,"_5mm", "_250um")
pacJun21Adj$sample = str_replace(pacJun21Adj$sample,"_run", "")

########  Pacific March 2021 ########  

# Join the dataframes of counts with the dataframe of the adjustments
pacMar21Adj =full_join(pacMar21, PacMar2021Perc, by=c("sample" = "FlowCamSampleName"))

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
pacMar21Adj$adjCount = pacMar21Adj$count / pacMar21Adj$PercSampleCleaned / pacMar21Adj$PercZooIdentified

######## Pacific Sept 2021 ########

# Join the dataframes of counts with the dataframe of the adjustments
pac21SeptAdj =full_join(pacSep21, PacSept2021Perc, by=c("sample" = "FlowCamSampleName"))

# The 5mm data will show up as NA because these are not included in the "Zooplankton Samples xlsx" and have
# nothing to join to. However, for these, 100% of the sample was analyzed.
# Therefore, replace these NAs with 1 (100% as a fraction)
pac21SeptAdj$PercSampleCleaned[is.na(pac21SeptAdj$PercSampleCleaned)] = 1
pac21SeptAdj$PercZooIdentified[is.na(pac21SeptAdj$PercZooIdentified)] = 1

# Create a column with adjusted counts. Original count is only a percentage of entire sample
# because only a percentage has been cleaned and then subsequently identified
pac21SeptAdj$adjCount = pac21SeptAdj$count / pac21SeptAdj$PercSampleCleaned / pac21SeptAdj$PercZooIdentified

# Remove _5mm file name ending. This will associate the 5mm size fraction with the rest of the sample from that location
pac21SeptAdj$sample = str_replace(pac21SeptAdj$sample,"_5mm", "")

################################################################################
## TEST SECTION: MERGE WITH THE METADATA

# The data do match:
# Gulf 2021
#	Maritimes 2021
#	NL 2021
#	Pacific Sept 2021 (these are okay, but the last 2 digits with the sampling time got cut off)

### REMEMBER TO ALSO DIVIDE BY 4 BECAUSE THE SAMPLE WAS SPLIT IN 4



# gulf 2021
metaGulfFix = 
  gulfZoo %>%
  select(facilityName, sampleCode, waterVolume, tideRange, yearStart) %>%
  filter(yearStart==2021)


# nl 2021 DONT HAVE METADATA FOR THIS YET
metaNlFix = 
  nlZoo %>%
  select(facilityName, sampleCode, waterVolume, tideRange, yearStart)


# Pacific Sept 2021
metaPacSept21Fix = 
  pacZoo %>%
  select(facilityName, sampleCode, waterVolume, tideRange, yearStart, monthStart) %>%
  filter(yearStart==2021 & monthStart == 09)



# Maritimes 2021
metaMarFix = 
  marZoo %>%
  select(facilityName, sampleCode, waterVolume, tideRange, yearStart) %>%
  # Rename one of the samples from the metadata where the file name is different
  mutate(sampleCode=str_replace(sampleCode, "21_08_25_Mar_S03_Z01_1548_250", "21_08_25_Mar_S03_Z01_1538_250"))




# Maritimes 2021
marMerge = full_join(mar21Adj, metaMarFix, by=c("sample" = "sampleCode"))



pacSept21Merge = full_join(pac21SeptAdj, metaPacSept21Fix, by=c("sample" = "sampleCode"))



mar21NewTest= 
  mar21New %>%
  select(sample)

x =full_join(mar21Adj, metaMarTest, by=c("sample" = "sampleCode"))

