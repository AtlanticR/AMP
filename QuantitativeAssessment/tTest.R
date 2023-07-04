################################################################################
################################################################################
##### T-Tests

# Conduct t-tests (?) between relative abundance of taxa from flowcam vs microscopy

################################################################################

# Read in the script that creates bar charts for the different comparisons
# This has already created a dataframe of relative abundance for each sample
source("QuantitativeAssessment/barChartsQA.R")



sumWant = sampleSummaryAdj %>%
  select(newName, regionYear, type, presence)

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
  
  # Run the t-tests for each grouping listed above
  do(t_test_result = tidy(t.test(relAbund~type, data = ., paired = T))) %>%
  # If this isn't added, the t-test results get stored as lists instead of separate columns
  unnest_wider(t_test_result)
     
  