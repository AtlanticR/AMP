##############################################################################
### RAREFACTION TESTS 

# Stephen Finnis

################################################################################
# Get counts per sample in each region:

rawCountsPerSample = allQAData %>%
  group_by(qaSampleID) %>%
  summarize(rawSampleCount = sum(countSample))

min(rawCountsPerSample$rawSampleCount)
max(rawCountsPerSample$rawSampleCount)
mean(rawCountsPerSample$rawSampleCount)
sd(rawCountsPerSample$rawSampleCount)






# FIRST WITH THE FLOWCAM DATAs

# Convert data to wide format so I can use the rarecurve() vegan function
fcWide = flowCamData %>%
  select(FlowCamID, count, newNameTake2) %>%
  pivot_wider(names_from = newNameTake2, values_from = count) %>%
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

par(mfrow = c(2,2))

rarecurve(fcGulf2020[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("St. Peters (2020)")

rarecurve(fcPac2021[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("Lemmens (2021)")

rarecurve(fcNL2020[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("South Arm (2020)")

rarecurve(fcNL2021[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("South Arm (2021)")

dev.off()

## Now try with the QA data



qaWide = allQAData %>%
  select(FlowCamID, count, taxaUpdateAgain) %>%
  pivot_wider(names_from = taxaUpdateAgain, values_from = count) %>%
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

par(mfrow = c(2,2))

rarecurve(qaGulf2020Wide[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("St. Peters (2020)")

rarecurve(qaPac2021Wide[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("Lemmens (2021)")

rarecurve(qaNL2020Wide[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("South Arm (2020)")

rarecurve(qaNL2021Wide[3:58], xlab = "Number of individuals", ylab = "Taxa richness")  
abline(v=203, col="red")
title("South Arm (2021)")


