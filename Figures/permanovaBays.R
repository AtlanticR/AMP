################################################################################
################################################################################
#### Run the multivariate statistics within each bay

# For each bay I am testing the effects of tide and station (myLabel, i.e., "inner, mid, outer"
# locations) on zooplankton composition

# This includes running three tests:
# 1. betadisper aka PERMDISP (what's called in PRIMER)
# 2. PERMANOVA 
# 3. SIMPER. Note this will be run between all factors, but I will later cut out the ones that between pairwise PERMANVOA cmoparisons
# that were not significant

# There are issues with these tests when the sample size is low 
# There are not enough unique permutations for a valid test. See p. 28 of http://updates.primer-e.com/primer7/manuals/PERMANOVA+_manual.pdf
# PRIMER recommends using Monte Carlo p-values in these instances
# These are not incorporated into the R adonis2 function
# I am therefore only running these tests on a few bays. More info will be provided in the Methods of the Tech Report
# I will also not be conducting post-hoc pairwise comparisons with the pairwise.adonis2 function
# In these instances, NMDS ordinations will be used to visualize if an effect of tide or station is present

# This code is very similar to that in permanova.R
# I have broken up the tests for each bay since the approach is very slightly different. And to avoid one huge script

# To get the results in tables that are easier to copy and paste into Google Docs/Word, use the code found in:
# Figures/permanovaTables.R

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
### Set-up

# Read in script that has the plankton counts in each bay
source("DataProcessing/bayBreakdown.R")

# Install this package to get the pairwise.adonis2 function
remotes::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

################################################################################
### Argyle

# Test this without the Mid-Rising data (n=2) for now
argMinusMR = argyle %>%
  subset(tidePhase != "Mid-Rising")

## DISPERSION TESTS

# Run the test
# Needs to be done separately for each test
argTideDisp = betadisper(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)])), as.factor(argMinusMR$tidePhase), type = "median")
argStnDisp = betadisper(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)])), as.factor(argMinusMR$myLabel), type = "median")

# Need to get permuted p-values. Use this to extract all the relevant information. I don't actually need pairwise comparisons, just including them for fun
set.seed(13)
argTideDispResults = permutest(argTideDisp, pairwise = T, permutations = 9999)
set.seed(13)
argStnDispResults = permutest(argStnDisp, pairwise = T, permutations = 9999)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disArgdf = data.frame(group = argTideDisp$group, distances = argTideDisp$distances, type = "Tide") %>%
  rbind(data.frame(group = argStnDisp$group, distances = argStnDisp$distances, type = "Station"))

ggplot(disArgdf, aes(x = group, y = distances), fill = "white")+
  facet_wrap(.~type, scales = "free")+
  geom_boxplot(col = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  #xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text.x = element_text(size = 13),
        legend.position = "none")


## PERMANVOA
set.seed(13)
argPerm = adonis2(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)])) ~ tidePhase*myLabel, data = argMinusMR, permutations = 9999)

# Note that this gives the same result as above. method = "bray is added instead of putting the data within vegdist() which creates a bray-curtis dissimilarity matrix
adonis2(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]) ~ tidePhase*myLabel, data = argMinusMR, permutations = 9999, method = "bray")

adonis2(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]) ~ tidePhase, data = argMinusMR, permutations = 9999, method = "bray")

adonis2(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)])) ~ tidePhase*myLabel, data = argMinusMR, permutations = 9999)

## PAIRWISE PERMANOVA
# set seed to get reproducible p-values
# Both tide and station were significant, so do pairwise comparisons for both
set.seed(13)
argTidePairwise = pairwise.adonis2(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]))~as.factor(tidePhase), data = argMinusMR, perm=9999, set.seed(13))
argStnPairwise = pairwise.adonis2(vegdist(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]))~as.factor(myLabel), data = argMinusMR, perm=9999, set.seed(13))


## SIMPER: will later cut out pairwise comparisons that aren't significant
simArgTide = simper(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]), 
                group=argMinusMR$tidePhase, permutations = 9999)

simArgStn = simper(sqrt(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp. (civ-vi)"):ncol(argMinusMR)]), 
                    group=argMinusMR$myLabel, permutations = 9999)

################################################################################
### Whitehead

# I won't be testing this but just including it here for reference
# No residual component: therefore, won't test
adonis2(sqrt(whitehead[,which(colnames(whitehead)== "Acartia spp. (civ-vi)"):ncol(whitehead)])~myLabel*tidePhase, data = whitehead, method = "bray", permutations = 9999)

################################################################################
### St. Peters

## DISPERSION TESTS

# Run the test
# Needs to be done separately for each test

stPMinusMF = stPeters %>%
  filter(tidePhase != "Mid-Falling")


stPTideDisp = betadisper(vegdist(sqrt(stPMinusMF[,which(colnames(stPMinusMF)== "Acartia spp. (civ-vi)"):ncol(stPMinusMF)])), as.factor(stPMinusMF$tidePhase), type = "median")
stPStnDisp = betadisper(vegdist(sqrt(stPMinusMF[,which(colnames(stPMinusMF)== "Acartia spp. (civ-vi)"):ncol(stPMinusMF)])), as.factor(stPMinusMF$myLabel), type = "median")

set.seed(13)
stPTideDispResults = permutest(stPTideDisp, pairwise = T, permutations = 9999)
set.seed(13)
stPStnDispResults = permutest(stPStnDisp, pairwise = T, permutations = 9999)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disStPdf = data.frame(group = stPTideDisp$group, distances = stPTideDisp$distances, type = "Tide") %>%
  rbind(data.frame(group = stPStnDisp$group, distances = stPStnDisp$distances, type = "Station"))

ggplot(disStPdf, aes(x = group, y = distances), fill = "white")+
  facet_wrap(.~type, scales = "free")+
  geom_boxplot(col = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  #xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text.x = element_text(size = 13),
        legend.position = "none")




## PERMANOVA
set.seed(13)
stPperm = adonis2(sqrt(stPMinusMF[,which(colnames(stPMinusMF)== "Acartia spp. (civ-vi)"):ncol(stPMinusMF)])~tidePhase*myLabel, data = stPMinusMF, method = "bray", permutations = 9999)

## PAIRWISE PERMANOVA
# Only station effects were significant, so run pairwise permanova for that
stPStnPairwise = pairwise.adonis2(vegdist(sqrt(stPMinusMF[,which(colnames(stPMinusMF)== "Acartia spp. (civ-vi)"):ncol(stPMinusMF)]))~as.factor(myLabel), data = stPMinusMF, perm=9999, set.seed(13))

## SIMPER
# Only between stations. Will later cut out comparisons that aren't significant
simStPStn = simper(sqrt(stPeters[,which(colnames(stPeters)== "Acartia spp. (civ-vi)"):ncol(stPeters)]), 
                   group=stPeters$myLabel, permutations = 9999)

################################################################################
### Newfoundland

nl21minusMf = seArm2021 %>%
  filter(tidePhase != "Mid-Falling")

nl21TideDisp = betadisper(vegdist(sqrt(nl21minusMf[,which(colnames(nl21minusMf)== "Acartia spp. (civ-vi)"):ncol(nl21minusMf)])), as.factor(nl21minusMf$tidePhase), type = "median")
nl21StnDisp = betadisper(vegdist(sqrt(nl21minusMf[,which(colnames(nl21minusMf)== "Acartia spp. (civ-vi)"):ncol(nl21minusMf)])), as.factor(nl21minusMf$myLabel), type = "median")

set.seed(13)
nl21TideDispResults = permutest(nl21TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
nl21StnDispResults = permutest(nl21StnDisp, pairwise = T, permutations = 9999)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disnl21df = data.frame(group = nl21TideDisp$group, distances = nl21TideDisp$distances, type = "Tide") %>%
  rbind(data.frame(group = nl21StnDisp$group, distances = nl21StnDisp$distances, type = "Station"))

ggplot(disnl21df, aes(x = group, y = distances), fill = "white")+
  facet_wrap(.~type, scales = "free")+
  geom_boxplot(col = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  #xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text.x = element_text(size = 13),
        legend.position = "none")


## PERMANOVA
set.seed(13)
nl21perm = adonis2(sqrt(nl21minusMf[,which(colnames(nl21minusMf)== "Acartia spp. (civ-vi)"):ncol(nl21minusMf)])~tidePhase*myLabel, data = nl21minusMf, method = "bray", permutations = 9999)

## PAIRWISE PERMANOVA: Don't need to do since only 2 groups

## SIMPER
# Only between stations. Will later cut out comparisons that aren't significant
simnl21Stn = simper(sqrt(nl21minusMf[,which(colnames(nl21minusMf)== "Acartia spp. (civ-vi)"):ncol(nl21minusMf)]), 
                   group=nl21minusMf$myLabel, permutations = 9999)


################################################################################
################################################################################
### Pacific

### August 2020 

## DISPERSION TESTS
aug2020TideDisp = betadisper(vegdist(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp. (civ-vi)"):ncol(pacAug2020)])), as.factor(pacAug2020$tidePhase), type = "median")
aug2020StnDisp = betadisper(vegdist(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp. (civ-vi)"):ncol(pacAug2020)])), as.factor(pacAug2020$myLabel), type = "median")

# Get permuted results
set.seed(13)
aug2020TideDispResults = permutest(aug2020TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
aug2020StnDispResults = permutest(aug2020StnDisp, pairwise = T, permutations = 9999)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disAug20df = data.frame(group = aug2020TideDisp$group, distances = aug2020TideDisp$distances, type = "Tide") %>%
  rbind(data.frame(group = aug2020StnDisp$group, distances = aug2020StnDisp$distances, type = "Station"))

ggplot(disAug20df, aes(x = group, y = distances), fill = "white")+
  facet_wrap(.~type, scales = "free")+
  geom_boxplot(col = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  #xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text.x = element_text(size = 13),
        legend.position = "none")


## PERMANOVA
set.seed(13)
aug2020perm = adonis2(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp. (civ-vi)"):ncol(pacAug2020)])~tidePhase*myLabel, data = pacAug2020, method = "bray", permutations = 9999)

# Only station was significant. So use that for pairwise tests
pacAug2020StnPairwise = pairwise.adonis2(vegdist(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp. (civ-vi)"):ncol(pacAug2020)]))~as.factor(myLabel), data = pacAug2020, perm=9999, set.seed(13))

## SIMPER
simAug2020Stn = simper(sqrt(pacAug2020[,which(colnames(pacAug2020)== "Acartia spp. (civ-vi)"):ncol(pacAug2020)]), 
                   group=pacAug2020$myLabel, permutations = 9999)

################################################################################
### June 2021

## DISPERSION
jun2021TideDisp = betadisper(vegdist(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp. (civ-vi)"):ncol(pacJun2021)])), as.factor(pacJun2021$tidePhase), type = "median")
jun2021StnDisp = betadisper(vegdist(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp. (civ-vi)"):ncol(pacJun2021)])), as.factor(pacJun2021$myLabel), type = "median")

# Get permuted results
set.seed(13)
jun2021TideDispResults = permutest(jun2021TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
jun2021StnDispResults = permutest(jun2021StnDisp, pairwise = T, permutations = 9999)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disJun21df = data.frame(group = jun2021TideDisp$group, distances = jun2021TideDisp$distances, type = "Tide") %>%
  rbind(data.frame(group = jun2021StnDisp$group, distances = jun2021StnDisp$distances, type = "Station"))

ggplot(disJun21df, aes(x = group, y = distances), fill = "white")+
  facet_wrap(.~type, scales = "free")+
  geom_boxplot(col = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  #xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text.x = element_text(size = 13),
        legend.position = "none")

## PERMANOVA
set.seed(13)
jun2021perm = adonis2(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp. (civ-vi)"):ncol(pacJun2021)])~tidePhase*myLabel, data = pacJun2021, method = "bray", permutations = 9999)

pacJun2021StnPairwise = pairwise.adonis2(vegdist(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp. (civ-vi)"):ncol(pacJun2021)]))~as.factor(myLabel), data = pacJun2021, perm=9999, set.seed(13))

## SIMPER
simJun2021Stn = simper(sqrt(pacJun2021[,which(colnames(pacJun2021)== "Acartia spp. (civ-vi)"):ncol(pacJun2021)]), 
                       group=pacJun2021$myLabel, permutations = 9999)

################################################################################
### September 2021

# DISPERSION TESTS
sept2021TideDisp = betadisper(vegdist(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp. (civ-vi)"):ncol(pacSept2021)])), as.factor(pacSept2021$tidePhase), type = "median")
sept2021StnDisp = betadisper(vegdist(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp. (civ-vi)"):ncol(pacSept2021)])), as.factor(pacSept2021$myLabel), type = "median")

## Get permuted results
set.seed(13)
sept2021TideDispResults = permutest(sept2021TideDisp, pairwise = T, permutations = 9999)
set.seed(13)
sept2021StnDispResults = permutest(sept2021StnDisp, pairwise = T, permutations = 9999)

## PERMANOVA
sept2021perm= adonis2(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp. (civ-vi)"):ncol(pacSept2021)])~tidePhase*myLabel, data = pacSept2021, method = "bray", permutations = 9999)

## PAIRWISE PERMANOVA
# NOTE: SHOULD PROBABLY ACTUALLY NOT DO PERMANOVAS, SINCE NUMBER OF UNIQUE PERMUTATIONS BETWEEN 2 GROUPS OF 4 IS ONLY 35
# Station was significant: run pairwise Permanova on that only
pacSept2021StnPairwise = pairwise.adonis2(vegdist(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp. (civ-vi)"):ncol(pacSept2021)]))~as.factor(myLabel), data = pacSept2021, perm=9999, set.seed(13))

## SIMPER
simSept2021Stn = simper(sqrt(pacSept2021[,which(colnames(pacSept2021)== "Acartia spp. (civ-vi)"):ncol(pacSept2021)]), 
                   group=pacSept2021$myLabel, permutations = 9999)

