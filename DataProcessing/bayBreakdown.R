################################################################################
## DATA BY BAYS

# I want to create dataframes with all the data for each bay
# It will take the dataframes created at the end of zooplanktonCounts.R and break them up by bay

# This therefore contains the following info:
# sampleCode, waterVolume, tideRange, yearStart, facilityName, myLabel, tidePhase, flowcamCode
# and also the taxa counts for each sample

# It will also adjust the format of the data: tows as rows, species as columns, counts as cells (with the metadata also as columns)

################################################################################

# Read in the processed zooplankton counts
# This will provide the dataframes with counts for each REGION (it's not broken up by bay yet)
# source("DataProcessing/zooplanktonCounts.R") 

################################################################################

options(scipen = 999)

# These are the stats 
waterVolStats = read.csv("../AMPDataFiles/rarefactionWaterVol.csv") %>%
  select(-c("waterVolume"))



## Create a function to alter the data format
breakupBayBay = function(regionData, bayName) {
  
  # Change data with tows as rows, species as columns, counts as cells (with the metadata also as columns)
  regionData %>%
    left_join(waterVolStats, by = "sampleCode") %>%
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    subset(facetFactor == bayName) %>%
    mutate(waterVolAnalyzed = (waterVolume * PercZooIdentified * PercSampleCleaned)/4)
}


################################################################################
## Now run the function for each bay
# There is probably a more elegant way to do this so it's not repeated
# But that is a task for another day!

## MARITIMES
argyle = breakupBayBay(mar, "Argyle")
country = breakupBayBay(mar, "Country Harbour")
sober = breakupBayBay(mar, "Sober Island")
whitehead = breakupBayBay(mar, "Whitehead")


## GULF
cocagne = breakupBayBay(gulf, "Cocagne")
malpeque = breakupBayBay(gulf, "Malpeque")
stPeters = breakupBayBay(gulf, "St. Peters")

## PACIFIC
# I have to be very careful with these names since there are similar variable names
# created in zooplanktonCounts.R
# Here, I will use the full year in the name
pacAug2020 = breakupBayBay(pac, "August 2020")
pacMar2021 = breakupBayBay(pac, "March 2021")
pacJun2021 = breakupBayBay(pac, "June 2021")
pacSept2021 = breakupBayBay(pac, "September 2021")

## NEWFOUNDLAND
# There's only one bay and time period (so far) but it still needs to have the layout adjusted
seArm2020 = breakupBayBay(nl, "Sept 2020")
seArm2021 = breakupBayBay(nl, "Oct 2021")
