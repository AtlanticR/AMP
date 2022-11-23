################################################################################
## DATA BY BAYS

# I want to create dataframes with all the data for each bay
# It will take the dataframes created at the end of zooplanktonCounts.R and break
# them up by bay

# This therefore contains the following info:
# sampleCode, waterVolume, tideRange, yearStart, facilityName, myLabel, tidePhase, flowcamCode
# and also the taxa counts for each sample

# It will also adjust the format of the data: tows as rows, species as columns, counts as cells (with the metadata also as columns)

# Created by Stephen Finnis 2022

################################################################################

# read in all required R packages
source("DataProcessing/zooplanktonCounts.R") 

################################################################################