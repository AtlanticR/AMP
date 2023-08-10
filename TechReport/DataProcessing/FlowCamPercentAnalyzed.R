################################################################################
################################################################################
### FLOWCAM SAMPLE ADJUSTMENTS

## BACKGROUND:
# For each plankton sample, the entire sample is run through the FlowCam and
# basic properties like particle size are measured. 
# HOWEVER, not all of the zooplankton are ID'd!! It is very time consuming.
# Instead, only a portion of the sample is actually ID'd and this must be 
# correctly adjusted for. These adjustments may vary from sample to sample.
# More detail is provided below in the "EXPLANATION OF PROCESS" section.

## PURPOSE OF CODE:
# This script will read in the adjustment factors that need to be accounted for
# for each sample. 
# Separate dataframes with the necessary adjustments will be created for each 
# dataset. Here, dataset refers to how the FlowCam data were provided to me 
# (Gulf 2020, Gulf 2021, Maritimes 2021, NL 2020, NL 2021, Pacific 2020, Pacific 
# June 2021, Pacific March 2021, Pacific Sept 2021).
# This script does NOT make the adjustments to the counts. That is done separately
# in the "ZooplanktonCounts.R" processing file.

## EXPLANATION OF PROCESS (what the FlowCam Tech/taxonomists did):
# Identifying the zooplankton is a laborious process and takes too long for one 
# entire sample. Instead, (for MOST datasets) the sample is first split into 10 equal 
# parts. From that, only a few of these subsamples are "cleaned" i.e, zooplankton 
# are separated out. The taxonomists only look at a few of those subsamples 
# (usually 3, but sometimes fewer if there are lots of plankton).
# From that, taxonomists have a designated amount of time they can spend per 
# sample. Sometimes they do not have time to get through entire sample.

# Therefore, the counts in each data file must be:
# Divided by the % of Sample Cleaned and then divided again by the % of (post-
# cleaned) Zooplankton Identified.
# This will turn the counts into (approx.) the total # of plankton per sample.
# Note, the counts also need to be divided by the volume of water sampled to get
# ind m^-3 but that information is in the metadata spreadsheets and the 
# calculation will be done separately.
# ALSO: counts will  need to be divided by 4, because after the sample
# was obtained, it was divided in 4 (1 sample for Flowcam, 1 as a backup, 
# the others to ... I forget). This is also done in ZooplanktonCounts.R

## DEFINITIONS:
# "Cleaning": the sorting of images into the following classes: Zooplankton, 0-250μm Length, Cut Images, 
# Debris, Fragments of Zooplankton, Benthic, Clumped Zooplankton, Debris or Zooplankton, Bubbles.
# "% of Sample Cleaned": the part of the fraction analyzed by taxonomists (i.e., % of subsamples they looked at)
# "% of (post cleaned) Zooplankton Identified": the amount of plankton from these subsamples they had time to analyze.

## STEPS I TOOK:
# I combined each "Zooplankton Samples xlsx" into one spreadsheet, where each
# sheet represents a different dataset (e.g., Gulf 2020, Gulf 2021, etc.)
# This was to clean up various formatting issues in Excel (a time saver).

# The "Zooplankton Samples" spreadsheets were found within each dataset directory.
# These Contain information regarding each FlowCam sample.
# I took the first table from the "Samples" sheet in each spreadsheet
# The columns I copied out were:
# 1. FlowCam Sample Name (to match with the data files)
# 2. % of Sample Cleaned
# 3. % of (post cleaned) Zooplankton Identified

# Data files are not public
# Code by Stephen Finnis 2022

################################################################################
################################################################################

# read in all required R packages
source("DataProcessing/rPackages.R") 

################################################################################
## Read in data and create dataframes for each dataset

# Set the file path to the spreadsheet I created
xl_data = file.path("../AMPDataFiles/extraFiles/FlowCamPercent.xlsx")

# Get the sheet names from the spreadsheet (there is one sheet per dataset
sheets = excel_sheets(path = xl_data)

# Read in the data from each sheet in the Excel file. Each sheet will be its own list
list_all = lapply(sheets, function (x) read_excel (xl_data, sheet = x))

# Very handy!!! This is how to label each list with the sheet name
names(list_all) = sheets

# This is how to change cell entries within each list of list
# I thought I could remove the R2 ending in ALL sample names, but I shouldn't become file names have 
# inconsistencies between datasets.
# list_all = map(list_all, ~ mutate(.x, FlowCamSampleName = str_replace(FlowCamSampleName, "_R2", "")))

# Turn each sheet (list entry) into individual dataframes. Each dataframe will be named the sheet name
list2env(list_all,envir=.GlobalEnv)
