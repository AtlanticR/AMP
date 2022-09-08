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
marMetaRaw = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP Metadata Plankton_2021_Maritimes_21Dec2021.xlsx", sheet = "zoo") 
nlMetaRaw = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo")
pacMetaRaw = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_Pacific_Jan262022.xlsx", sheet = "zoo")
gulfMetaRaw = read_excel("C:/Users/FINNISS/Desktop/FlowCamMetadata/AMP_Metadata_Plankton_2021_GULF_Feb22022_JB.xlsx", sheet = "zoo")

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

# Combine all the info from the Pacific into one dataframe
# Convert latitude/longitude to numeric otherwise there are problems when merging
# Remove duplicates. There are 2 duplicates: 20_08_29_Pac_S04_Z15_1105_236 and 21_03_05_Pac_S04_Z20_NA_250
pacAllLoc = purrr::reduce(list(pacMar21Loc, pacJun21Loc, pacSept21Loc, pac20Loc), dplyr::full_join) %>%
  mutate(latitude = as.numeric(replace(latitude, latitude == "NA", NA))) %>%
  mutate(longitude = as.numeric(replace(longitude, longitude == "NA", NA))) %>%
  distinct(sampleCode, .keep_all =T) %>%
  # I also have problems matching things if there are too many fields (idk why!! maybe extra spaces)
  # Just choose the ones that are actually important that need to be merged
  select(sampleCode, myLabel, tideGuess, tidePhase, flowcamCode)

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

  # Changing characters --> numeric for some entries
  # Some data values were entered as "NA" which means the rest of the numbers aren't showing up as numeric. This causes problems when merging
  # datasets since the column type needs to be the same!
  # Change "NA" to NA and change type to numeric.
  dfProc = dfProc %>%
    mutate(latitude = as.numeric(replace(latitude, latitude == "NA", NA))) %>%
    mutate(longitude = as.numeric(replace(longitude, longitude == "NA", NA))) %>%
    mutate(latitudeEnd = as.numeric(replace(latitudeEnd, latitudeEnd == "NA", NA))) %>%
    mutate(longitudeEnd = as.numeric(replace(longitudeEnd, longitudeEnd == "NA", NA))) %>%
    mutate(tideLevel = as.numeric(replace(tideLevel, tideLevel == "NA", NA))) 

  return(dfProc) 
}

################################################################################
## Process the data and merge with the Location/Tide

# Pass the raw metadata xlsx files into the processMeta function above
# Will return dataframe of processed metadata for each region

marMeta = processMeta(marMetaRaw) %>%
  left_join(marLoc)

nlMeta = processMeta(nlMetaRaw) %>%
  left_join(nlLoc)

pacMeta = processMeta(pacMetaRaw) %>%
  # Note, there are 2 duplicates: 20_08_29_Pac_S04_Z15_1105_236 and 21_03_05_Pac_S04_Z20_NA_250
  # I'm just removing the second one. For the first one, waterVolumes (what really matters) are approx the same
  # For the second one, I don't think it matters because these don't have any data anyway
  # If these aren't removed, the dfs won't merge properly and there will be duplicates
  # Setting .keep_all = T means it keeps all the data but removes the duplicate (second option)  
  distinct(sampleCode, .keep_all = T) %>%
  full_join(pacAllLoc)

gulfMeta = processMeta(gulfMetaRaw) %>%
  left_join(gulfLoc)
