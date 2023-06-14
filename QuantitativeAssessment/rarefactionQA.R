##############################################################################
### Rarefaction figures 

# Create plots to show what happens to richness if we resample to the lowest # of individuals

# Stephen Finnis

################################################################################
## Setup

# Read in the script that puts together the QA (microscopy) and Flowcam data
source("QuantitativeAssessment/QAcodeMatches.R")

################################################################################
## Get basic stats for # of plankton per sample in the QA data

# Get total counts per sample
rawCountsPerSample = allQAData %>%
  group_by(qaSampleID) %>%
  summarize(rawSampleCount = sum(count))

# Get stats for each sample
minInd = min(rawCountsPerSample$rawSampleCount) # minimum number of inds in any sample
max(rawCountsPerSample$rawSampleCount)
mean(rawCountsPerSample$rawSampleCount)
sd(rawCountsPerSample$rawSampleCount)

################################################################################
### Quantitative Assessment data

## Restructure data into appropriate format

# Convert data to wide format so I can use the rarecurve() vegan function
qaWide = allQAData %>%
  select(FlowCamID, count, newName) %>%
  pivot_wider(names_from = newName, values_from = count) %>%
  mutate_all(~replace(., is.na(.), 0))  # replace NAs with 0 

# Gulf 2020 (St. Peters)
qaGulf2020Wide = qaWide %>%
  filter(regionYear == "Gulf 2020")

# Pac 2021
qaPac2021Wide = qaWide %>%
  filter(regionYear == "Pac 21")

# NL 2020
qaNL2020Wide = qaWide %>%
  filter(regionYear == "NL 2020")

# NL 2021
qaNL2021Wide = qaWide %>%
  filter(regionYear == "NL 2021")


## Start making the plots

# Set up plotting window (2 rows, 2 columns)
par(mfrow = c(2,2))

rarecurve(qaGulf2020Wide[3:length(qaGulf2020Wide)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("St. Peters (2020)")

rarecurve(qaPac2021Wide[3:length(qaPac2021Wide)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("Lemmens (2021)")

rarecurve(qaNL2020Wide[3:length(qaNL2020Wide)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("South Arm (2020)")

rarecurve(qaNL2021Wide[3:length(qaNL2021Wide)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("South Arm (2021)")

# Clear plotting window specifications
dev.off()

################################################################################
### FlowCam data

## Restructure data into appropriate format

# Convert data to wide format so I can use the rarecurve() vegan function
fcWide = flowCamData %>%
  select(FlowCamID, count, newName) %>%
  pivot_wider(names_from = newName, values_from = count) %>%
  mutate_all(~replace(., is.na(.), 0))  # replace NAs with 0 

# Gulf 2020 (St. Peters)
fcGulf2020 = fcWide %>%
  filter(regionYear == "Gulf 2020")

# Pac 2021
fcPac2021 = fcWide %>%
  filter(regionYear == "Pac 21")

# NL 2020
fcNL2020 = fcWide %>%
  filter(regionYear == "NL 2020")

# NL 2021
fcNL2021 = fcWide %>%
  filter(regionYear == "NL 2021")


## Start making the plots

# Set up plotting window (2 rows, 2 columns)
par(mfrow = c(2,2))

# Species list starts at column 3
rarecurve(fcGulf2020[3:length(fcGulf2020)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red") # add red line for what would happen if resample to 203 individuals
title("St. Peters (2020)")

rarecurve(fcPac2021[3:length(fcPac2021)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("Lemmens (2021)")

rarecurve(fcNL2020[3:length(fcNL2020)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("South Arm (2020)")

rarecurve(fcNL2021[3:length(fcNL2021)], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v = minInd, col="red")
title("South Arm (2021)")

# Clear plotting window specifications
dev.off()
