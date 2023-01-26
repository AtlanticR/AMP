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
source("DataProcessing/rPackages.R") 

################################################################################
## Load the data
# Files are read relative to my R Project (within "AMP"), which is separate from where the data files are stored (AMPDataFiles)
# ".." means "go back a directory"
# Suppress warnings ("expecting numeric but got  date") 
marMetaRaw = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP Metadata Plankton_2021_Maritimes_21Dec2021.xlsx", sheet = "zoo"))
nlMetaRaw20 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo"))

nlMetaRaw20$regionalSampleID = as.character(nlMetaRaw20$regionalSampleID)


pacMetaRaw = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_Pacific_Jan262022.xlsx", sheet = "zoo"))
gulfMetaRaw = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_GULF_Feb22022_JB.xlsx", sheet = "zoo"))

nlMetaRaw2122 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_2022_NL_Jan252023.xlsx"))
nlMetaRaw2122$tideLevel = as.character(nlMetaRaw2122$tideLevel)
nlMetaRaw2122$flowmeter = as.character(nlMetaRaw2122$flowmeter)

nlMetaRaw = nlMetaRaw20 %>%
  full_join(nlMetaRaw2122)

### Read in Extra Information: Tide Phase and Location in Bay

## Location
# In general, sampling sites were selected to be in "Inner", "Mid", and "Outer" portions of the bay
# I visualized these in Google Earth and assigned them labels. Sometimes they do not follow these categories exactly
# I am still deciding how these should be labelled

## Tides
# We want tide phase categories, i.e., High, Mid-Rising, Mid-Falling, Low which was not always directly defined in the metadata
# Tide info in metadata does not always follow what it is supposed to (i.e., tideRange = high, mid, low. tideRiseFall = rising, falling)
# I have therefore added a TidePhase column

# Maritimes: comes from a combination of tideRange (high, mid, low) and tideRiseFall (rising, falling)

# Gulf: provided by Thomas Guyondet. However, some were included in the FlowCam names. Tides were from Thomas' classes, then compared against flowcam labels
# NOTE: sample 20_09_01_Gulf_S04_Z38_0938_250	aka AMMP_Gulf_StPeters_3_20200902LT_250UM was labelled incorrectly. So it is actually a HT sample.
# Also, there are 4 "mid-falling". When looking at the tideLevel and Thomas' classes, these are definitely in the middle. I am ignoring the HT from the labels, and asking what is correct
# 20_09_02_Gulf_S04_Z39_1106_250	AMMP_Gulf_StPeters_2A_20200902HT_250UM
# 20_09_02_Gulf_S04_Z39_1126_250	AMMP_Gulf_StPeters_2B_20200902HT_250UM
# 20_09_02_Gulf_S04_Z39_1150_250	AMMP_Gulf_StPeters_2C_20200902_250UM
# 20_09_02_Gulf_S04_Z39_1213_250	AMMP_Gulf_StPeters_2D_20200902_250UM

# Newfoundland: in 2020, samples were not taken from specific high/mid/low periods. However, Thomas Gyundet has also provided.

# Pacific: tides were either Low or High. Get this from the FlowcamSample name OR from nearby Tofino tide station (https://www.isdm-gdsi.gc.ca/isdm-gdsi/twl-mne/maps-cartes/inventory-inventaire-eng.asp?user=isdm-gdsi&region=ATL&tst=1&perm=0)
# For some Pacific sites, when comparing sampling times to Tofino tide data, some could be classified as 'mid-falling' or 'mid-rising'
# However, to reduce the # of classes, keep as just high/low. But tidePhaseOld has mid-rising/falling if it's ever needed

# Note to self: I have the correct High Tide/Low Tide data from Jackson Chu (emailed Sept 27, 2022). Need to use/refer to this for correcting the metadata

# I've deleted a bunch of extra columns from the metadata
marLoc = read.csv("../AMPDataFiles/LocationAndNameMatches/marLocation.csv")
# This is where I also included the matches from the FlowCam sample names to the metadata (col: flowcamCode)
# These were provided from Jeff Barrell. I need these for the Maritimes 2020 data which were different
gulfLoc = read.csv("../AMPDataFiles/LocationAndNameMatches/gulfLocation.csv")

# Newfoundland: matches provided from Olivia Gibb
nl20Loc = read.csv("../AMPDataFiles/LocationAndNameMatches/nl20Location.csv") # 2020 data
nl2122Loc = read.csv("../AMPDataFiles/LocationAndNameMatches/nl2122Location.csv")  # 2021 and 2022 data

# Join the 2 nNewfoundland location information datasets
# The columns that truly matter are in both spreadsheets. So they will join properly. 
# There are a few extra, unimportant columns that aren't shared between the two
nlAllLoc = full_join(nl20Loc, nl2122Loc)

# Pacific: broken into 3, just like FlowCam adata
pac20Loc = read_excel("../AMPDataFiles/LocationAndNameMatches/pacific2020Location.xlsx")
pacMar21Loc = read_excel("../AMPDataFiles/LocationAndNameMatches/pacificMarch2021Location.xlsx")
pacJun21Loc = read_excel("../AMPDataFiles/LocationAndNameMatches/pacificJune2021Location.xlsx")
pacSept21Loc = read_excel("../AMPDataFiles/LocationAndNameMatches/pacificSept2021Location.xlsx")

# Combine all the info from the Pacific into one dataframe
# Convert latitude/longitude to numeric otherwise there are problems when merging
# Remove duplicates. There are 2 duplicates: 20_08_29_Pac_S04_Z15_1105_236 and 21_03_05_Pac_S04_Z20_NA_250
pacAllLoc = purrr::reduce(list(pacMar21Loc, pacJun21Loc, pacSept21Loc, pac20Loc), dplyr::full_join) %>%
  mutate(latitude = as.numeric(replace(latitude, latitude == "NA", NA))) %>%
  mutate(longitude = as.numeric(replace(longitude, longitude == "NA", NA))) %>%
  distinct(sampleCode, .keep_all =T) %>%
  # I also have problems matching things if there are too many fields (idk why!! maybe extra spaces)
  # Just choose the ones that are actually important that need to be merged
  select(sampleCode, myLabel, tidePhase, flowcamCode)

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
  
  # Fix monthStart: should be integer
  #dfProc$monthStart = as.numeric(dfProc$monthStart)
  #dfProc$dayStart = as.numeric(dfProc$dayStar)
  
  # GULF: there are several NAs. They just forgot to do the waterVolume calculation
  dfProc$waterVolume = ifelse(is.na(dfProc$waterVolume) & dfProc$region == "Gulf", # check for NAs in Gulf region metadata
                              0.25^2 * dfProc$flowmeter * 26873 / 999999, # if true, fill in with volume calculation
                              dfProc$waterVolume) # if false, just leave the original value as is
  
  # PACIFIC: 
  # For some of the sites (ones with 50 x 150 cm Net), they forgot to convert radius from cm --> meters
  # Therefore divide by 100^2 (cm --> meters conversion, then squared)
  # Note: there will still be some NAs left over. I think these are from tows with no data. Will need to double check
  # Suppress warnings to get rid of "NAs introduced..." message
  dfProc$waterVolume = suppress_warnings(ifelse((dfProc$region == "Pacific" | dfProc$region == "Pac") & dfProc$equipmentType == "50 x 150 cm Net", # check for NAs in Pacific region metadata
                              as.numeric(dfProc$waterVolume) / 100^2, # if true, fill in with volume calculation
                              dfProc$waterVolume)) # if false, just leave the original value as is
  
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
  left_join(marLoc) %>%
  # Rename one of the samples from the metadata where the file name is different
  mutate(sampleCode=str_replace(sampleCode, "21_08_25_Mar_S03_Z01_1548_250", "21_08_25_Mar_S03_Z01_1538_250")) %>%
  # When mapped, latitudeEnd and longitudeEnd of 21_08_27_Mar_S01_Z07_1115_250 sample is clearly wrong. I think mistakenly
  # entered as values from station 21_08_27_Mar_S01_Z09_1802_250 (almost the same)
  # Replace with latitudeEnd/longitudeEnd from the other S07 station (21_08_27_Mar_S01_Z07_1743_250)
  mutate(latitudeEnd = replace(latitudeEnd, sampleCode == "21_08_27_Mar_S01_Z07_1115_250", 44.83887)) %>%
  mutate(longitudeEnd = replace(longitudeEnd, sampleCode == "21_08_27_Mar_S01_Z07_1115_250", -62.47138))

# Not sure why I had to specify "by = " in this case, but it made it work?
nlMeta = processMeta(nlMetaRaw) %>%
  left_join(nlAllLoc, by = "sampleCode")

pacMeta = processMeta(pacMetaRaw) %>%
  # Note, there are 2 duplicates: 20_08_29_Pac_S04_Z15_1105_236 and 21_03_05_Pac_S04_Z20_NA_250
  # I'm just removing the second one. For the first one, waterVolumes (what really matters) are approx the same
  # For the second one, I don't think it matters because these don't have any data anyway
  # If these aren't removed, the dfs won't merge properly and there will be duplicates
  # Setting .keep_all = T means it keeps all the data but removes the duplicate (second option)  
  distinct(sampleCode, .keep_all = T) %>%
  full_join(pacAllLoc) %>%
  # One site missing a water volume. Fill in with the other with the spot (for Pacific, 2 tows combined in one sample)
  # Will not affect results much since this is one of the "Pooled" samples
  mutate(waterVolume = replace(waterVolume, sampleCode == "21_03_03_Pac_S04_Z01_1503_250", 10.42636801))

gulfMeta = processMeta(gulfMetaRaw) %>%
  left_join(gulfLoc)

