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

































