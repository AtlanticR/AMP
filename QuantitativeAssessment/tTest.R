################################################################################
################################################################################
##### T-Tests

# Conduct t-tests (?) between relative abundance of taxa from flowcam vs microscopy

################################################################################

# Read in the script that creates bar charts for the different comparisons
# This has already created a dataframe of relative abundance for each sample
source("QuantitativeAssessment/barChartsQA.R")

################################################################################
## Prep the data 

# Background: I need to be doing paired tests (wilcoxon or t-tests, TBD) to test for 
# differences in relative abundance between FlowCam (FC) and Microscopy (QA) methods
# Using paired tests (vs independent tests) because they are from the same sample and 
# therefore not independent.

# Paired tests require the same # of samples per group since there should be an equivalent
# value for each sample in FC/QA 
# Therefore, if a taxa was observed in one sample for QA/FC but not the other, I need to say the 
# relative abundance was 0 in the sample where it was not observed.
# Adding zeroes means that I need to use the complete() function to create all pairs of data by FlowCamID and taxa

# I also get errors when I'm comparing 0 relative abundance between 0 relative abundance (i.e., absent in both samples)
# So I need to remove those. BUT only when they are absent in NONE of the samples in that regionYear.


# From the bar chart figures code
# This tells me if taxa were observed in both of the regionYears, neither of the regionYears, FC Only, or MC only
# Note, this is for the overall regionYear, not by individual sample. e.g., if Acartia was found in 1 FC sample, but none
# of the others (and not in any of the microscopy samples), the sample would be ID'd as "FC Only"
sumWant = sampleSummaryAdj %>%
  select(newName, regionYear, type, presence) # only keep a few of the columns

# Get FlowCamID and regionYear (for filling in NAs later)
qaID.edit = qaID %>%
  select(FlowCamID, regionYear)

# Create the data frame described above with zeroes when there are no taxa
dfAllPairs = fcQaDf %>%
  # Get the relative abundance of each taxa for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = countTot / sum(countTot)*100) %>%
  # Need to ungroup after using group_by or functions won't work
  ungroup() %>%
  # Create all comparisons. When there's no data, put relAbund as zero
  complete(newName, FlowCamID, type, fill = list(relAbund =0)) %>%
  # Now, edit the df: I think there's a better way to do this. But above function will put NAs in some of the columns.
  # Need to fill in the regionYears that became NAs. Do this by joining with a dataframe of just FlowCamID/regionYear
  left_join(qaID.edit, by = c("FlowCamID")) %>%
  mutate(regionYear.x = coalesce(regionYear.x, regionYear.y)) %>%
  rename(regionYear = regionYear.x) %>%
  # Join with df that specifies whether taxa are observed in "both" FC/MC, just FC, or just MC for each regionYear
  full_join(sumWant) 

################################################################################
# Start running the stats


shWilk = dfAllPairs %>%
  filter(presence == "Both") %>%
  
# Shapiro Wilk test
  
  # I want to conduct t-tests between all taxa for each regionYear 
  group_by(newName, regionYear) %>%
  
  mutate(sample_size = n()) %>%
  filter(sample_size >2) %>%
  summarize(p_value = shapiro.test(relAbund[type %in% c("FC", "QA")])$p.value, .groups = "drop")
  
    




# T-TEST 
test = fcQaDf %>%
  # Get the relative abundance for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = countTot / sum(countTot)*100) %>%
  # Join with df that specifies whether taxa are observed in "both" FC/MC, just FC, or just MC for each regionYear
  left_join(sumWant) %>%
  # Need to ungroup or functions won't work
  ungroup() %>%
  # Create all comparisons. when there's no data, put relAbund as zero
  complete(newName, regionYear, FlowCamID, type, fill = list(relAbund =0)) %>%
  # I want to conduct t-tests between all taxa for each regionYear 
  group_by(newName, regionYear) %>%
  
  mutate(avg_relAbund = mean(relAbund, na.rm = T))
  
  # Run the t-tests for each grouping listed above
  do(t_test_result = tidy(t.test(relAbund~type, data = ., paired = T))) %>%
  # If this isn't added, the t-test results get stored as lists instead of separate columns
  unnest_wider(t_test_result) %>%
  mutate(avg_relAbund = mean(relAbund, na.rm = T))


# Wilcoxon test
test = fcQaDf %>%
  # Get the relative abundance for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = countTot / sum(countTot)*100) %>%
  # Join with df that specifies whether taxa are observed in "both" FC/MC, just FC, or just MC for each regionYear
  left_join(sumWant) %>%
  # Need to ungroup or functions won't work
  ungroup() %>%
  # Create all comparisons. when there's no data, put relAbund as zero
  complete(newName, regionYear, FlowCamID, type, fill = list(relAbund =0)) %>%
  
  filter(presence == "Both") %>%
  
  # I want to conduct t-tests between all taxa for each regionYear 
  group_by(newName, regionYear) %>%
  
  # Run the t-tests for each grouping listed above
  do(t_test_result = tidy(wilcox.test(relAbund~type, data = ., paired = T))) %>%
  # If this isn't added, the t-test results get stored as lists instead of separate columns
  unnest_wider(t_test_result)
     
  