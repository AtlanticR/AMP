################################################################################
################################################################################
#### PERMANOVAs
# For testing for significant differences between groups
# We are testing for differences at multiple spatial scales:
# Between oceans (Pacific vs Atlantic)
# Between DFO regions (Pacific, Maritimes, Gulf, Newfoundland)
# Between bays
# Between locations within bay (mid, inner, outer)
# Different days

## Approach:

### CHANGE THIS. FIRST DO DISPERSION. THEN PERMANOVA. THEN SIMPER
# Pairwise comparisons are sub-steps of each test


# 1. At each spatial scale, conduct a PERMANOVA to determine if significant differences exist
# 2. Conduct pairwise comparisons to find out which groups are significantly different 
# 3. Conduct SIMPER analysis to determine which species contribute most to the dissimilarities
# between groups

# Created by Stephen Finnis 2022

################################################################################
################################################################################
## Get things set up

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("Figures/nmdsSymbols.R")

# Install this package to get the pairwise.adonis2 function
remotes::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

################################################################################
################################################################################
#### Test for differences between oceans
# There are two factors: Atlantic and Pacific 

### DISPERSION
# betadisper is the R function. PERMDISP is what it's called in PRIMER
# This is a prerequisite (assumption) for PERMANOVA but also gives interesting results on its own
oceanDisp = betadisper(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)])), as.factor(allRegionsWide$ocean))
# This will get you ANOVA results but with non-permuted significance
anova(oceanDisp) 
# Get significance (overall) and also conduct pairwise tests
pairOceanDisp = permutest(oceanDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for permuted p-values. Also can get t-values for pairwise comparisons 
pairOceanDisp
pairOceanDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

# I am removing Tukey HSD test and just going to use t-statistics and permuted p-values
# But could use Tukey HSD to determine pairwise significance if I change my mind


### PERMANOVA
# First selects only the species data (i.e., starting at Acartia until the end)
adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$ocean), method="bray", sqrt.dist = T, perm = 9999)

# No need for pairwise tests since only 2 groups at this scale


### SIMPER 
# Use SIMPER to find out which species contribute most to the differences 
simOcean = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]), 
                                      group=allRegionsWide$ocean)

# View the SIMPER results                  
summary(simOcean)

################################################################################
################################################################################
#### Test for differences between DFO Regions
# There are 4 factors (in this study): Maritimes, Gulf, Newfoundland, Pacific

### DISPERSION
regDisp = betadisper(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)])), as.factor(allRegionsWide$region))
pairRegDisp = permutest(regDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairRegDisp
pairRegDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????


### PERMANOVA

# Test 3 different adonis methods to make sure they're all the same. They are!!
adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$region), method="bray", sqrt.dist = T, perm = 9999)

# Make sure I get same results using vegdist. I do!!
adonis2(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]))~
          as.factor(allRegionsWide$region), sqrt.dist = F, perm = 9999)

# Last check- adjust the square root distances. Also the same!
adonis2(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)])~
          as.factor(allRegionsWide$region), sqrt.dist = T, perm = 9999)

# Pairwise comparisons between each region
# I don't think I actually need the as.factor() but I've added it just in case
# Could take the square root of the F-value to get t-values (see PERMANOVA/PRIMER guide)
pairwise.adonis2(sqrt(vegdist(allRegionsWide[12:ncol(allRegionsWide)]))~as.factor(region), data = allRegionsWide)


### SIMPER
simRegion = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]), 
                  group=allRegionsWide$region)
summary(simRegion)

################################################################################
################################################################################
### Test if bays are different within each region

# First need to break up the data
# I had the regions in a for loop so now I have to create them all here 
# LOOK INTO MOVING THIS AND MAKING IT CONSISTENT BETWEEN ALL SCRIPTS
marPN = marMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

gulfPN = gulfMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

nlPN = nlMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

pacPN = pacMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0


################################################################################
## MARITIMES

### DISPERSION
marDisp = betadisper(sqrt(vegdist(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)])), as.factor(marPN$facetFactor))
pairMarDisp = permutest(marDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairMarDisp
pairMarDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????


### PERMANOVA
# Are there differences between bays (facetFactor)
adonis2(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]~
          as.factor(marPN$facetFactor), method="bray", sqrt.dist = T, perm = 9999)

# Pairwise comparisons between bays
pairwise.adonis2(sqrt(vegdist(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]))~as.factor(facetFactor), data = marPN)

### SIMPER
simMar = simper(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]), 
                   group=marPN$facetFactor)
summary(simMar)


################################################################################
## Gulf

### DISPERSION
gulfDisp = betadisper(sqrt(vegdist(gulfPN[,which(colnames(gulfPN)== "Acartia spp. "):ncol(gulfPN)])), as.factor(gulfPN$facetFactor))
pairGulfDisp = permutest(gulfDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairGulfDisp
pairMarDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????


### PERMANOVA
# Are there differences between bays (facetFactor)
adonis2(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]~
          as.factor(marPN$facetFactor), method="bray", sqrt.dist = T, perm = 9999)

# Pairwise comparisons between bays
pairwise.adonis2(sqrt(vegdist(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]))~as.factor(facetFactor), data = marPN)

### SIMPER
simMar = simper(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. "):ncol(marPN)]), 
                group=marPN$facetFactor)
summary(simMar)





################################################################################
### ALTERNATIVE METHODS 


## A very bad first attempt at a nested PERMANOVA
# There are too many issues to comment on right now. I'll come back to this, maybe

# Need to remove these 2 with no label
nestedPerm = allRegionsWide %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))

adonis2(nestedPerm[,12:ncol(allRegionsWide)]~as.factor(nestedPerm$ocean)/as.factor(nestedPerm$region)/as.factor(nestedPerm$facetFactor)/as.factor(nestedPerm$myLabel), method="bray", sqrt.dist = T)


#########
### Get pairwise adonis function from old package
# Don't use this one because it uses adonis not adonis 2 (even though it gives the same results lol)
# Need to install from GitHub to get pairwise.adonis
remotes::install_github("Jtrachsel/funfuns")
library("funfuns")

# With the function from funfuns package
pairwise.adonis(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), sim.method="bray")

# Compare against this other pairwise adonis method
# Note that this one uses ADONIS 2 which is updated and maybe better?


# This is another way to do the pairwise comparisons, although they do not come with t-values
reg.HSD = TukeyHSD(regionDisp)
reg.HSD
plot(reg.HSD)
