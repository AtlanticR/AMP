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
  group_by(region, facetFactor, yearStart,dateRange, myLabel, tidePhase, avgDepthStn) %>%
  # In each bay, count the number of samples with this station and tide phase combination
  summarise(stnTideCount = n())


  
  





