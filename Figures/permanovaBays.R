################################################################################
################################################################################
#### Run the multivariate statistics within each bay

# For each bay I am testing the effects of tide and station (myLabel, i.e., "inner, mid, outer"
# locations) on zooplankton composition

# This includes running two tests:
# 1. betadisper aka PERMDISP (what's called in PRIMER)
# 2. PERMANOVA 

# I may also include SIMPER tests, but currently I am unsure which groups to make the comparisons between

# There are issues with these tests when the sample size is low 
# There are not enough unique permutations for a valid test. See p. 28 of http://updates.primer-e.com/primer7/manuals/PERMANOVA+_manual.pdf
# PRIMER recommends using Monte Carlo p-values in these instances
# These are not incorporated into the R adonis2 function
# I am therefore only running these tests on a few bays. More info will be provided in the Methods of the Tech Report
# I will also not be conducting post-hoc pairwise comparisons with the pairwise.adonis2 function
# In these instances, NMDS ordinations will be used to visualize if an effect of tide or station is present

# This code is very similar to that in permanova.R
# I have broken up the tests for each bay since the approach is very slightly different. And to avoid one huge script


# A few more notes for myself:
# For dispersion tests, I am using spatial median as the definition of centroid
# I wanted to use "type = centroid" but see the Warning on the R help: https://rdrr.io/cran/vegan/man/betadisper.html

# For dispersion tests, if there are 2 factors, here are the steps for dispersion test:
# https://stat.ethz.ch/pipermail/r-sig-ecology/2010-September/001524.html
# i.e., run them separately

# See here for calculating the number of unique permutations
# https://github.com/vegandevs/vegan/issues/308

################################################################################
################################################################################
# Set-up

# Read in script that has the plankton counts in each bay
source("DataProcessing/bayBreakdown.R")


################################################################################
### Argyle

# Test this without the Mid-Rising data (n=2) for now
argMinusMR = argyle %>%
  subset(tidePhase != "Mid-Rising")

## DISPERSION TESTS

# Run the test
# Needs to be done separately for each test
argTideDisp = betadisper(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)])), as.factor(argMinusMR$tidePhase), type = "median")
argStnDisp = betadisper(vegdist(sqrt(dataTest[,which(colnames(dataTest)== "Acartia spp."):ncol(dataTest)])), as.factor(argMinusMR$myLabel), type = "median")

# Need to get permuted p-values. Use this to extract all the relevant information. I don't actually need pairwise comparisons, just including them for fun
set.seed(13)
argTideDispResults = permutest(argTideDisp, pairwise = T, permutations = 9999)
set.seed(13)
argStnDispResults = permutest(argStnDisp, pairwise = T, permutations = 9999)

## PERMANVOA
set.seed(13)
argPerm = adonis2(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)])) ~ tidePhase*myLabel, data = argMinusMR, permutations = 9999)

# Note that this gives the same result as above. method = "bray is added instead of putting the data within vegdist() which creates a bray-curtis dissimilarity matrix
adonis2(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)]) ~ tidePhase*myLabel, data = argMinusMR, permutations = 9999, method = "bray")

################################################################################
### Whitehead

# I won't be testing this but just including it here for reference
# No residual component: therefore, won't test
adonis2(sqrt(whitehead[,which(colnames(whitehead)== "Acartia spp."):ncol(whitehead)])~myLabel*tidePhase, data = whitehead, method = "bray", permutations = 9999)

################################################################################
### St. Peters

## DISPERSION TESTS

# Run the test
# Needs to be done separately for each test
stPTideDisp = betadisper(vegdist(sqrt(stPeters[,which(colnames(stPeters)== "Acartia spp."):ncol(stPeters)])), as.factor(stPeters$tidePhase), type = "median")
stPStnDisp = betadisper(vegdist(sqrt(stPeters[,which(colnames(stPeters)== "Acartia spp."):ncol(stPeters)])), as.factor(stPeters$myLabel), type = "median")

set.seed(13)
stPTideDispResults = permutest(stPTideDisp, pairwise = T, permutations = 9999)
set.seed(13)
stPStnDispResults = permutest(stPStnDisp, pairwise = T, permutations = 9999)

set.seed(13)
stPperm = adonis2(sqrt(stPeters[,which(colnames(stPeters)== "Acartia spp."):ncol(stPeters)])~tidePhase*myLabel, data = stPeters, method = "bray", permutations = 9999)

################################################################################
# Newfoundland
# I DON'T THINK I SHOULD DO THIS
# "No residual component"
adonis2(sqrt(seArm2020[,which(colnames(seArm2020)== "Acartia spp."):ncol(seArm2020)])~tidePhase*myLabel, data = seArm2020, method = "bray", permutations = 9999)

################################################################################
### Pacific

## August 2020 

# Dispersion test
aug2020TideDisp = betadisper(vegdist(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp."):ncol(pacAug2020)])), as.factor(pacAug2020$tidePhase), type = "median")
aug2020StnDisp = betadisper(vegdist(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp."):ncol(pacAug2020)])), as.factor(pacAug2020$myLabel), type = "median")

# Get permuted results
set.seed(13)
aug2020TideDispResults = permutest(aug2020TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
aug2020StnDispResults = permutest(aug2020StnDisp, pairwise = T, permutations = 9999)

# Run the permanova
set.seed(13)
aug2020perm = adonis2(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp."):ncol(pacAug2020)])~tidePhase*myLabel, data = pacAug2020, method = "bray", permutations = 9999)


## June 2021
# Dispersion test
jun2021TideDisp = betadisper(vegdist(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp."):ncol(pacJun2021)])), as.factor(pacJun2021$tidePhase), type = "median")
jun2021StnDisp = betadisper(vegdist(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp."):ncol(pacJun2021)])), as.factor(pacJun2021$myLabel), type = "median")

# Get permuted results
set.seed(13)
jun2021TideDispResults = permutest(jun2021TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
jun2021StnDispResults = permutest(jun2021StnDisp, pairwise = T, permutations = 9999)

# Run the PERMANOVA
set.seed(13)
jun2021perm = adonis2(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp."):ncol(pacJun2021)])~tidePhase*myLabel, data = pacJun2021, method = "bray", permutations = 9999)


## September 2021

# Dispersion test
sept2021TideDisp = betadisper(vegdist(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp."):ncol(pacSept2021)])), as.factor(pacSept2021$tidePhase), type = "median")
sept2021StnDisp = betadisper(vegdist(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp."):ncol(pacSept2021)])), as.factor(pacSept2021$myLabel), type = "median")

# Get permuted results
set.seed(13)
sept2021TideDispResults = permutest(sept2021TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
sept2021StnDispResults = permutest(sept2021StnDisp, pairwise = T, permutations = 9999)

sept2021perm= adonis2(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp."):ncol(pacSept2021)])~tidePhase*myLabel, data = pacSept2021, method = "bray", permutations = 9999)



