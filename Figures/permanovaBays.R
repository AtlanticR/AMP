# of unique permutations 

# See here for sample questions
# #https://github.com/vegandevs/vegan/issues/308

source("DataProcessing/bayBreakdown.R")


################################################################################
## Argyle

# Test this without the Mid-Rising data (n=2) for now
argMinusMR = argyle %>%
  subset(tidePhase != "Mid-Rising")



dataTest = argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)]
brayArg = vegdist(sqrt(dataTest))

adonis2(brayArg~tidePhase*myLabel, data = argMinusMR, perm = 9999)

# Note that inputting the actual data and then selecting method = "bray" gives the same result
adonis2(sqrt(dataTest)~tidePhase*myLabel, data = argMinusMR, method = "bray", perm = 9999)









################################################################################
# St. Peters

adonis2(sqrt(stPeters[,which(colnames(stPeters)== "Acartia spp."):ncol(stPeters)])~tidePhase*myLabel, data = stPeters, method = "bray", nperm = 9999)

