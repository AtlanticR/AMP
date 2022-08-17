################################################################################
# Making Aquaculture Monitoring Program maps of sampling locations
# Make leaflet maps for the AMP sampling sites out of the metadata files
# Create generic functions to process/clean the data, then map these locations
# Sampling locations are both points (discrete i.e., punctual stations) and
# lines (transects)
# Metadata files are not public
# Code by Stephen Finnis July 2022
################################################################################

## Get things set up

# Clear console
rm(list=ls())
graphics.off()

# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("dplyr", "ggplot2", "leaflet", "mapr", "mapview", "readxl")
ipak(packages)

################################################################################
## Load the data data

# Set directory
# Look into here package and probably replace this 
setwd("C:/Users/FINNISS/Desktop")

# Load in metadata data files
# mar = Maritimes
marMeta = read_excel("FlowCamMetadata\\AMP Metadata Plankton_2021_Maritimes_21Dec2021.xlsx", sheet = "zoo") 
nlMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo")
pacMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_Pacific_Jan262022.xlsx", sheet = "zoo")
gulfMeta = read_excel("FlowCamMetadata\\AMP_Metadata_Plankton_2021_GULF_Feb22022_JB.xlsx", sheet = "zoo")

################################################################################
## Data cleaning

# Create a function to only select relevant data from the metadata
processMeta = function(xlData) {
  dfProc = subset(xlData, sampleType == "Z" & # only get Zooplankton data
                    (netMesh == 250 | # only want net size of 236 or 250 um
                       netMesh == 236) & 
                    yearStart != 2019) # do not want 2019 data
  
  ## Fix waterVolume (volume of water filtered by plankton net)
  # Calculation comes from the zoo_inst sheet in the metadata
  # volume of cylinder = pi * r^2 * depth. Here, a flowmeter conversion factor is included instead of depth
  # r is the net radius which may not always be the same!
  
  # GULF: there are several NAs. They just forgot to do the waterVolume calculation
  dfProc$waterVolume = ifelse(is.na(dfProc$waterVolume) & dfProc$region == "Gulf", # check for NAs in Gulf region metadata
                              0.25^2 * dfProc$flowmeter * 26873 / 999999, # if true, fill in with volume calculation
                              dfProc$waterVolume) # if false, just leave the original value as is
  
  # PACIFIC: 
  # For some of the sites (ones with 50 x 150 cm Net), they forgot to convert radius in cm --> meters
  # Therefore divide by 100^2 (cm --> meters conversion, then squared)
  # Note: there will still be some NAs left over. I think these are from tows with no data. Will need to double check
  dfProc$waterVolume = ifelse((dfProc$region == "Pacific" | dfProc$region == "Pac") & dfProc$equipmentType == "50 x 150 cm Net", # check for NAs in Gulf region metadata
                              as.numeric(dfProc$waterVolume) / 100^2, # if true, fill in with volume calculation
                              dfProc$waterVolume) # if false, just leave the original value as is
  
  # MARITIMES and NEWFOUNDLAND: no waterVolume issues
  
  return(dfProc) # return processed data frame
}

# Process the data
marZoo = processMeta(marMeta) # Maritimes zooplankton data
nlZoo = processMeta(nlMeta) # Newfoundland
pacZoo = processMeta(pacMeta) # Pacific
gulfZoo = processMeta(gulfMeta) # Gulf
