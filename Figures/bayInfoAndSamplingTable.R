################################################################################
## Create table of information for Tech Report that characterized

# This code has all the bay data (combined) in the format that I want
# The dataframe I want is "allRegionsWide" which has all the relevant info for each sample (and the species counts!)
source("Figures/nmdsRegions.R")

# Get the columns that are actually important to merge
bayTable = allRegionsWide %>%
  # If I don't ungroup I get an "add missing grouping variables:..." warning
  # See here for more details: https://datacornering.com/dplyr-adding-missing-grouping-variable/
  ungroup() %>%
  # Turn months into 3 letter text (e.g., 8 --> Aug)
  mutate(monthLetters = month.abb[monthStart]) %>%
  # Create the date range for each field campaign
  # Start by getting the min and max date within each month for each field campaign (facetFactor)
  # Need to get the start and end dates for each field campaign
  # Note this is weird for Argyle which was split over 2 months (Aug 30 - Sep 1), so just manually fill that one in
  # Contrast to Cocagne, which was sampled on Jul 21 and Aug 26 (dif field campaigns, so I want this to be specified 
  group_by(facetFactor, monthStart) %>%
  mutate(minDate = min(dayStart),
         maxDate = max(dayStart),
         # ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))
         dateRange = ifelse(facetFactor == "Argyle", "Aug 30 - Sep 1", # for Argyle, just manually enter the date range to prevent errors
                            ifelse(minDate == maxDate, paste(monthLetters, minDate), # If data only collected on one day, enter that day (not as a range)
           paste(monthLetters, minDate, "-", monthLetters, maxDate)))) %>% # Otherwise, specify the range
  group_by(region, facetFactor, myLabel) %>%
  # This takes the average depth for each station within each bay
  mutate(avgDepthStn = round(mean(depthWaterM), 2)) %>% 
  group_by(region, facetFactor, yearStart,dateRange, myLabel, tidePhase, avgDepthStn, productionType, target, samplingDesign, equipmentType, TowType, netMesh) %>%
  # In each bay, count the number of samples with this station and tide phase combination
  summarise(stnTideCount = n()) %>%
  # March 2021 had multiple samples combined from all stations
  mutate(myLabel = ifelse(facetFactor == "March 2021", "Combined (Inner, Mid, Outer)", myLabel)) %>%
  # Shellfish production type was missing for St. Peters and Malpeque
  # I looked at the leases from https://www.arcgis.com/home/webmap/viewer.html?webmap=16aa8830c7084a8a92ce066b525978b4 (in QGIS)
  # And looked at the shellfish types within both areas. I am not sure the difference between quahaug and clam?
  mutate(productionType = ifelse(facetFactor == "St. Peters", "Mussel, oyster, scallop, quahaug, clam",
                                 ifelse(facetFactor == "Malpeque", "Mussel, oyster, quahaug, clam", productionType))) %>%
  # Make first letter in each cell upper case. Idk why this didn't work for "target" column. Maybe because of NAs?
  mutate_if(is.character, str_to_upper) %>%
  # Fix for consistency. Some Pacific field seasons were blank (but I know it should be Punctual Stations). Country Harbour and Whitehead did not have an "s" at the end
  mutate(samplingDesign = ifelse(region == "Pacific" | facetFactor == "Country Harbour" | facetFactor == "Whitehead", "Punctual stations", samplingDesign)) %>%
  mutate(TowType = ifelse(TowType == "obliq", "Oblique", TowType))


  
  





