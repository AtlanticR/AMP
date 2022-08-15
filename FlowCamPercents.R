################################################################################
# Script to read in FlowCam data
# This script will read the percent of sample analyzed by the FlowCam


################################################################################
## Background notes
# I combined each "Zooplankton Samples xlsx" into one spreadsheet, where each
# sheet represents a different dataset (e.g., Gulf 2020, Gulf 2021, etc.)
# This was to clean up various formatting issues in Excel (a time saver)
# These were found in each directory, but combined into one to make reading in 
# the files a bit easier
# Took the first table from the "Samples" sheet in each spreadsheet
# The columns we want are:
# FlowCam Sample Name (to match with the data files)
# % of Sample Cleaned: Percentage of the sample that has been cleaned. This does not include 5mm portions of the sample. Cleaning refers to the sorting of images into the following classes: Zooplankton, 0-250μm Length, Cut Images, Debris, Fragments of Zooplankton, Benthic, Clumped Zooplankton, Debris or Zooplankton, Bubbles.
# % of (post cleaned) Zooplankton Identified: The percentage of zooplankton from the Post Cleaning Zooplankton Count that have been identified. This does not include 5mm portions of the sample.

# Cleaning: refers to the sorting of images into the following classes: Zooplankton, 0-250μm Length, Cut Images, Debris, Fragments of Zooplankton, Benthic, Clumped Zooplankton, Debris or Zooplankton, Bubbles.

#############################################
## Explanation of process:
# Note: the entire sample is analyzed (i.e., basic properties like particle size are measured) BUT
# not all of the zooplankton ID'd!! It is very time consuming since they have to be manually ID'd
# Instead, (for MOST datasets) the sample is first split into 10 equal parts
# From that, only a few of the samples are "cleaned" i.e, zooplankton are separated out
# Therefore, % of Sample Cleaned represents part of the fraction analyzed by taxonomists
# From that, taxonomists have a designated amount of time they can spend per sample. Sometimes they do not have time to get through entire sample

# Therefore the counts must be:
# Multiplied by the % of Sample Cleaned and then multiplied again by the % of (post cleaned) Zooplankton Identified

# Note, the counts also need to be divided by the volume of water sampled to gets ind m^-3 but that information is in the metadata
# spreadsheet and the calculation will be done separately
###################################




# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("dplyr", "ggplot2", "ggrepel", "ggthemes", "jcolors", "leaflet", "mapr", "mapview", "readxl", "stringr",
             "tidyr", "tools", "useful", "vegan", "wpa")
ipak(packages)





# Determine file path for permit data (created by Charlotte Smith)
xl_data = file.path(fileLoadPath, "NaturalResources/Species/Permits/SARAdata_withCoordinates.xlsx")

# Get the sheet names from the spreadsheet (there is one sheet per year, from 2010 to 2020)
sheets = excel_sheets(path = xl_data)

# Read in the data from each sheet in the Excel file. Each sheet will be its own list
list_all = lapply(sheets, function (x) read_excel (xl_data, sheet = x))

# Combine each list into a single dataframe
perm_df = rbind.fill(list_all)