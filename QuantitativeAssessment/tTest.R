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

  left_join(sumWant) %>%
  filter(presence == "Both") %>%
  
  ungroup() %>%
  complete(newName, regionYear, FlowCamID, type, fill = list(relAbund =0)) %>%

  group_by(newName, regionYear) %>%
  do(t_test_result = tidy(t.test(relAbund~type, data = ., paired = T)))
     
  