################################################################################
## Create table of information for Tech Report that characterized

# This code has all the bay data (combined) in the format that I want
# The dataframe I want is "allRegionsWide" which has all the relevant info for each sample (and the species counts!)
source("Figures/nmdsRegions.R")

# Get the columns that are actually important to merge
bayTable = allRegionsWide %>%
  
  ## Figure out the date ranges for each bay/field season
  # If I don't ungroup I get an "add missing grouping variables:..." warning
  # See here for more details: https://datacornering.com/dplyr-adding-missing-grouping-variable/
  ungroup() %>%
  # Turn months into 3 letter text (e.g., 8 --> Aug)
  mutate(monthLetters = month.abb[monthStart]) %>%
  
  # Try creating new variable instead of facetFactor due to challenges with NL data.
  mutate(fieldCampaign = ifelse(region == "Newfoundland", paste("NL", monthLetters, yearStart), facetFactor)) %>%
  
  
  # Create the date range for each field campaign
  # Start by getting the min and max date within each month for each field campaign (facetFactor)
  # Need to get the start and end dates for each field campaign
  # Note this is weird for Argyle which was split over 2 months (Aug 30 - Sep 1), so just manually fill that one in
  # Contrast to Cocagne, which was sampled on Jul 21 and Aug 26 (dif field campaigns, so I want this to be specified 
  group_by(fieldCampaign, monthStart) %>%
  mutate(minDate = min(dayStart),
         maxDate = max(dayStart),
         # ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))
         dateRange = ifelse(fieldCampaign == "Argyle", "Aug 30 - Sep 1", # for Argyle, just manually enter the date range to prevent errors
                            ifelse(fieldCampaign == "March 2021", "Mar 3-5", # I had to initially adjust to only say "March 3" so samples would group properly in zooplanktonCounts.R. But it's actually Mar 3-5
                              ifelse(minDate == maxDate, paste(monthLetters, minDate), # If data only collected on one day, enter that day (not as a range)
           paste(monthLetters, minDate, "-", monthLetters, maxDate))))) %>% # Otherwise, specify the range
  
  ## Fix typos and make adjustments to the entries
  
  # Because I have to sort and group entries by monthStart, and I don't want Argyle entries to show up separately. So just pretend that the samples in September (9) occurred in August (8)
  mutate(monthStart = ifelse(fieldCampaign == "Argyle", 8, monthStart)) %>%
  
  # Change equipment type for all St. Peters to "250/150". (Some were just "250" but it must be the same)
  mutate(equipmentType = ifelse(fieldCampaign == "St. Peters", "250/150", equipmentType)) %>%
  
  # Change station name for March 2021. Samples combined from all stations
  mutate(myLabel = ifelse(fieldCampaign == "March 2021", "Combined (Inner, Mid, Outer)", myLabel)) %>%
  
  # Shellfish production type was missing for St. Peters and Malpeque
  # I looked at the leases from https://www.arcgis.com/home/webmap/viewer.html?webmap=16aa8830c7084a8a92ce066b525978b4 (some archive files are from Jeff)
  # And looked at the shellfish types within both areas. I am not sure the difference between quahaug and clam?
  mutate(productionType = ifelse(fieldCampaign == "St. Peters", "Mussel, oyster, scallop, quahaug, clam",
                                 ifelse(fieldCampaign == "Malpeque", "Mussel, oyster, quahaug, clam", productionType))) %>%
  
  
  # Make first letter in each cell upper case. Idk why this didn't work for "target" column. Maybe because of NAs?
  # mutate_if(is.character, str_to_title) %>%
  # Try again for the ones it didn't work for
  mutate(target = str_to_title(target),
         productionType = str_to_title(productionType)) %>%
  
  # Rename the transect type. I did this by comparing the entries for equipmentType and TowType and combining into one column (they were a bit inconsistent)
  mutate(mySampleType = ifelse(region == "Gulf" | fieldCampaign == "Sober Island", "Transect (horizontal)",
                               ifelse(fieldCampaign == "Argyle", "Transect (oblique)",
                                      "Punctual station (vertical)"))) %>%
  
  ## Start doing some calculations:
  
  # Need to take the average value for every station within each bay/field season
  group_by(region, fieldCampaign, myLabel) %>%
  # This takes the average depth for each station within each bay
  mutate(avgDepthStn = round(mean(depthWaterM), 2)) %>% 
  
  # In each bay, count the number of samples with this station and tide phase combination. Need to "group_by" everything else so I don't lose the columns
  group_by(region, fieldCampaign, yearStart,dateRange, myLabel, tidePhase, avgDepthStn, productionType, target, equipmentType, netMesh, mySampleType, monthStart) %>%
  summarise(stnTideCount = n()) %>%
  
  ## Last steps! Select the columns I actually need and rename them.
  ungroup() %>% # I have to do this again or it will "add missing grouping variables" lol
  select(region, fieldCampaign, yearStart, dateRange, productionType, target, mySampleType, equipmentType, netMesh, myLabel, avgDepthStn, tidePhase, stnTideCount, monthStart) %>%
  
  arrange(region, yearStart, ifelse(region == "Maritimes" | region == "Gulf", fieldCampaign, monthStart)) %>%
  
  rename("Region" = region, 
         "Bay or field season" = fieldCampaign, 
         "Year" = yearStart, 
         "Date range" = dateRange,
         "Production type" = productionType,
         "Tow type" = mySampleType, # note this is my category
         "Net mesh size" = netMesh,
         "Station name" = myLabel,
         "Av. station depth (m)" = avgDepthStn,
         "Tide phase" = tidePhase,
         "Station-Tide count" = stnTideCount)

# write.csv(bayTable, "bayTable.csv")
  



