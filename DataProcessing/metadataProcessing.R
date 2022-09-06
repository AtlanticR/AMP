################################################################################
## PROCESSING METADATA

## BACKGROUND:
# The metadata files describe the sampling for each Aquaculture Monitoring 
# Program (AMP) region (Maritimes, Gulf, Newfoundland, Pacific)
# This includes lat/lon of tow, sample name, net type, volume of water towed, etc.
# Metadata files include data for both zooplankton analysis (FlowCam) and flow 
# cytometry (separate analysis).

## PURPOSE OF CODE:
# This code reads in the metadata files, selects on relevant data for zooplankton
# analysis, and returns the processed data
# This processed data is needed for other analyses (e.g., creating study area
# maps and linking FlowCam data files to each tow)

## ADDITIONAL INFO:
# Metadata files are not public
# Code by Stephen Finnis 2022
################################################################################

# read in all required R packages
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/rPackages.R") 

################################################################################
## Load the data data

# Set directory
# Look into here package and probably replace this 
# setwd("C:/Users/FINNISS/Desktop")

# Load in metadata data files
# mar = Maritimes
marMeta = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP Metadata Plankton_2021_Maritimes_21Dec2021.xlsx", sheet = "zoo") 
nlMeta = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo")
pacMeta = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_Pacific_Jan262022.xlsx", sheet = "zoo")
gulfMeta = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_GULF_Feb22022_JB.xlsx", sheet = "zoo")

# Read in my spreadsheet with the location names, i.e., "north", "mid", or "south" in bay
# I've deleted a bunch of extra columns from the metadata
marLoc = read.csv("C:/Users/FINNISS/Desktop/LocationAndNameMatches/marLocation.csv")
# This is where I also included the matches from the FlowCam sample names to the metadata (col: flowcamCode)
# These were provided from Jeff Barrell. I need these for the Maritimes 2020 data which were different
gulfLoc = read.csv("C:/Users/FINNISS/Desktop/LocationAndNameMatches/gulfLocation.csv")
# Newfoundland: matches provided from Olivia Gibb
nlLoc = read.csv("C:/Users/FINNISS/Desktop/LocationAndNameMatches/nl20Location.csv")
# Pacific: broken into 3, just like FlowCam adata
pac20Loc = read_excel("C:/Users/FINNISS/Desktop/LocationAndNameMatches/pacific2020Location.xlsx")
pacMar21Loc = read_excel("C:/Users/FINNISS/Desktop/LocationAndNameMatches/pacificMarch2021Location.xlsx")
pacJun21Loc = read_excel("C:/Users/FINNISS/Desktop/LocationAndNameMatches/pacificJune2021Location.xlsx")
pacSept21Loc = read_excel("C:/Users/FINNISS/Desktop/LocationAndNameMatches/pacificSept2021Location.xlsx")


################################################################################
## Make data processing function

# Create a function to only select relevant data from the metadata
processMeta = function(xlData) {
  dfProc = subset(xlData, sampleType == "Z" & # only get Zooplankton data
                    (netMesh == 250 | # only want net size of 236 or 250 um
                       netMesh == 236) & 
                    yearStart != 2019) # do not want 2019 data
  
  ## Fix waterVolume (volume of water filtered by plankton net)
  # Calculation comes from the zoo_inst sheet in the metadata spreadsheets
  # volume of cylinder = pi * r^2 * depth. Here, a flowmeter conversion factor is included instead of depth
  # The conversion factor is specific to the flowmeter model
  # r is the net radius. It should be in meters but in a few instances it is in cm (these are fixed below)
  
  # GULF: there are several NAs. They just forgot to do the waterVolume calculation
  dfProc$waterVolume = ifelse(is.na(dfProc$waterVolume) & dfProc$region == "Gulf", # check for NAs in Gulf region metadata
                              0.25^2 * dfProc$flowmeter * 26873 / 999999, # if true, fill in with volume calculation
                              dfProc$waterVolume) # if false, just leave the original value as is
  
  # PACIFIC: 
  # For some of the sites (ones with 50 x 150 cm Net), they forgot to convert radius from cm --> meters
  # Therefore divide by 100^2 (cm --> meters conversion, then squared)
  # Note: there will still be some NAs left over. I think these are from tows with no data. Will need to double check
  dfProc$waterVolume = ifelse((dfProc$region == "Pacific" | dfProc$region == "Pac") & dfProc$equipmentType == "50 x 150 cm Net", # check for NAs in Gulf region metadata
                              as.numeric(dfProc$waterVolume) / 100^2, # if true, fill in with volume calculation
                              dfProc$waterVolume) # if false, just leave the original value as is
  
  # MARITIMES and NEWFOUNDLAND: no waterVolume issues

   
  ## Need an identifier for where within each bay the stations are located
  # I displayed these in Google Earth and then determined if they were in the South, Mid, North part of the bay
  # Note: ifelse can be nested in many different ways. If it's Maritimes or Gulf, put the myLabel, otherwise put as NA
  dfProc = dfProc %>%
    mutate(location = ifelse(region == "Mar", marLoc$myLabel,
                             ifelse(region == "Gulf", gulfLoc$myLabel, NA))) %>%
    mutate(flowCamMatch = ifelse(region == "Gulf", gulfLoc$flowcamCode, NA)) # add code from Jeff Barrell

  return(dfProc) # return processed data frame
}

################################################################################
## Process the data

# Pass the raw metadata xlsx files into the processMeta function above
# Will return dataframe of processed metadata for each region
# I really should rename these variables!!
marZoo = processMeta(marMeta) # Maritimes zooplankton data
nlZoo = processMeta(nlMeta) # Newfoundland
pacZoo = processMeta(pacMeta) # Pacific
gulfZoo = processMeta(gulfMeta) # Gulf

# write.csv(gulfZoo, "GulfTides.csv")

