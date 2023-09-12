

source("TechReport/DataProcessing/rPackages.R")

# Also need to have QAcodeMatches.R to get waterVolSamples data



sampleData = read_excel("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/sampleSummaries.xlsx", sheet = "originalCopy") %>%
  # Replace NAs (or "n/a" written in) with zeroes for the 5mm counts
  mutate(fiveMmCount = as.numeric(ifelse(is.na(fiveMmCount) | fiveMmCount == "n/a", 0, fiveMmCount)),
         chaetognathaNotes = as.numeric(ifelse(is.na(chaetognathaNotes) | str_detect(chaetognathaNotes, "^Taxa was"), 0, chaetognathaNotes)),
         propPlankton = zooCountPostCleaning / cleanedParticleCount) %>%
  right_join(waterVolSamples, by = c("FlowCam Sample Name" = "FlowCamID")) # make sure to right join or I get all the extra data from samples not being used

sampleStats = sampleData %>%
  filter(usedForAnalysis == "Yes") %>%
  group_by(regionYear) %>%
  summarize(meanParticle = mean(sampleParticleCount),
            minParticle = min(sampleParticleCount),
            maxParticle = max(sampleParticleCount),
            sdParticle = sd(sampleParticleCount),
            meanZoo = mean(identifiedZooCount),
            minZoo = min(identifiedZooCount),
            maxZoo = max(identifiedZooCount),
            sdZoo = sd(identifiedZooCount),
            meanFiveCount = mean(fiveMmCount),
            minFiveCount = min(fiveMmCount),
            maxFiveCount = max(fiveMmCount),
            sdFiveCount = sd(fiveMmCount),
            meanWater = mean(waterVolume),
            minWater = min(waterVolume),
            maxWater = max(waterVolume),
            sdWater = sd(waterVolume),
            meanProp = mean(propPlankton),
            minProp = min(propPlankton),
            maxProp = max(propPlankton),
            sdProp = sd(propPlankton))

# write.csv(sampleStats, "sampleStats.csv")


statsAll = sampleData %>%
  filter(usedForAnalysis == "Yes") %>%
  #group_by(regionYear) %>%
  summarize(meanParticle = mean(sampleParticleCount),
            minParticle = min(sampleParticleCount),
            maxParticle = max(sampleParticleCount),
            sdParticle = sd(sampleParticleCount),
            meanZoo = mean(identifiedZooCount),
            minZoo = min(identifiedZooCount),
            maxZoo = max(identifiedZooCount),
            sdZoo = sd(identifiedZooCount),
            meanFiveCount = mean(fiveMmCount),
            minFiveCount = min(fiveMmCount),
            maxFiveCount = max(fiveMmCount),
            sdFiveCount = sd(fiveMmCount),
            meanWater = mean(waterVolume),
            minWater = min(waterVolume),
            maxWater = max(waterVolume),
            sdWater = sd(waterVolume),
            meanProp = mean(propPlankton),
            minProp = min(propPlankton),
            maxProp = max(propPlankton),
            sdProp = sd(propPlankton))

write.csv(statsAll, "statsAll.csv")


