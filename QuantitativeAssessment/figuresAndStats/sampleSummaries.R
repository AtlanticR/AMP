

source("TechReport/DataProcessing/rPackages.R")

sampleData = read_excel("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/sampleSummaries.xlsx", sheet = "originalCopy")

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
            sdZoo = sd(identifiedZooCount))



