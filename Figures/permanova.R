################################################################################
################################################################################
#### BETADISPER, PERMANOVA and SIMPER analysis

# Created by Stephen Finnis 2022

### Background:
# This script is for testing for significant differences in zooplankton community structure between groups
# We are testing for differences at multiple spatial scales:
# Between oceans (Pacific vs Atlantic)
# Between DFO regions (Pacific, Maritimes, Gulf, Newfoundland)
# Between bays
# Between locations within bay (mid, inner, outer)
# Different days

### Approach:
## 1a. At each spatial scale, conduct BETADISPER test to determine if there are significant
# differences in dispersion between groups
# Dispersion gives interesting information on its own. No differences in dispersion is also
# an assumption for PERMANOVAs
## 1b. Conduct pairwise tests in dispersion at each spatial scale (get t-statistic and permuted significances)

## 2a. At each spatial scale, conduct a PERMANOVA to determine if significant differences exist between groups
## 2b. Conduct pairwise comparisons to find out which groups are significantly different 

## 3. Conduct SIMPER analysis to determine which species contribute most to the dissimilarities
# between groups. Only report differences between groups that are significantly different (from 2b)


### Notes:
# PERMDISP is the same as BETADISPER, but PERMDISP is the function name in PRIMER software

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
# vegdist turns the data into dissimilarities. Bray-Curtis is the default
# Need to also square root transform
# It's  a bit clunky, but data in the species matrix always starts with 'Acartia spp." until the last column (ncol)
# I am using type = "centroid" instead of "spatial" (spatial median) to be consistent with PRIMER. In R, "spatial" is default
# See PRIMER manual for brief explanation of the differences. Manual recommends "centroid" in most cases
oceanDisp = betadisper(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$ocean), type = "centroid")
# This will get you ANOVA results (i.e., are there differences in dispersion) but with non-permuted significance
anova(oceanDisp) 
# Get significance (overall) and also conduct pairwise tests. Report these results instead.
pairOceanDisp = permutest(oceanDisp, pairwise = T, permutations = 9999)

# Construct boxplot to show differences in dispersion
boxplot(oceanDisp, xlab = NULL)
plot(oceanDisp)

# Look at pairRegDisp for permuted p-values. Also can get t-values for pairwise comparisons 
pairOceanDisp
pairOceanDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

# Can also use Tukey HSD to get significance between pairwise groups. Example is at the end of the script. 
# I am sticking with t-values since that is what PRIMER uses and how most people report results. 

### PERMANOVA
# First selects only the species data (i.e., starting at Acartia until the end)
adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$ocean), method="bray", sqrt.dist = T, perm = 9999)

# No need for pairwise tests since only 2 groups at this scale


### SIMPER 
# Use SIMPER to find out which species contribute most to the differences 
simOcean = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]), 
                                      group=allRegionsWide$ocean)

# View the SIMPER results                  
summary(simOcean)

################################################################################
################################################################################
#### Test for differences between DFO Regions
# There are 4 factors (in this study): Maritimes, Gulf, Newfoundland, Pacific

### DISPERSION
regDisp = betadisper(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), type = "centroid")
pairRegDisp = permutest(regDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairRegDisp
pairRegDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

boxplot(regDisp, xlab = NULL)
plot(regDisp)

disRegion <- data.frame(group = regDisp$group, distances = regDisp$distances)

install.packages("ggsignif")
library("ggsignif")

ggplot(disRegion, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  geom_signif(comparisons = list(c("Gulf", "Maritimes")),
              map_signif_level = T)+
  geom_signif(comparisons = list(c("Gulf", "Newfoundland")),
              map_signif_level = T, y_position = 0.9)+
  geom_signif(comparisons = list(c("Maritimes", "Newfoundland")),
              map_signif_level = T, y_position = 1.0)+
  geom_signif(comparisons = list(c("Maritimes", "Pacific")),
              map_signif_level = T, y_position = 1.1)+
  geom_signif(comparisons = list(c("Newfoundland", "Pacific")),
              map_signif_level = T, y_position = 1.2)+
  xlab("Region")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        # setting the margin adds space between tick labels and titles
        axis.title.x = element_text(size = 12.5, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        #plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        legend.position = "none")



### PERMANOVA

# Test 3 different adonis methods to make sure they're all the same. They are!!
adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$region), method="bray", sqrt.dist = T, perm = 9999)

# Make sure I get same results using vegdist. I do!!
adonis2(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]))~
          as.factor(allRegionsWide$region), sqrt.dist = F, perm = 9999)

# Last check- adjust the square root distances. Also the same!
adonis2(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])~
          as.factor(allRegionsWide$region), sqrt.dist = T, perm = 9999)

# Pairwise comparisons between each region
# I don't think I actually need the as.factor() but I've added it just in case
# Could take the square root of the F-value to get t-values (see PERMANOVA/PRIMER guide)
pairwise.adonis2(sqrt(vegdist(allRegionsWide[12:ncol(allRegionsWide)]))~as.factor(region), data = allRegionsWide)


### SIMPER
simRegion = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]), 
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
marDisp = betadisper(sqrt(vegdist(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)])), as.factor(marPN$facetFactor), type = "centroid")
pairMarDisp = permutest(marDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairMarDisp
pairMarDisp$statistic

boxplot(marDisp, xlab = NULL)

### PERMANOVA
# Are there differences between bays (facetFactor)
adonis2(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]~
          as.factor(marPN$facetFactor), method="bray", sqrt.dist = T, perm = 9999)

# Pairwise comparisons between bays
pairwise.adonis2(sqrt(vegdist(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]))~as.factor(facetFactor), data = marPN)

### SIMPER
simMar = simper(sqrt(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]), 
                   group=marPN$facetFactor)
summary(simMar)


################################################################################
## GULF

### DISPERSION
gulfDisp = betadisper(sqrt(vegdist(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)])), as.factor(gulfPN$facetFactor), type = "centroid")
pairGulfDisp = permutest(gulfDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairGulfDisp
pairGulfDisp$statistic

boxplot(gulfDisp, xlab = NULL)

### PERMANOVA
# Are there differences between bays (facetFactor)
adonis2(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)]~
          as.factor(gulfPN$facetFactor), method="bray", sqrt.dist = T, perm = 9999)

# Pairwise comparisons between bays
pairwise.adonis2(sqrt(vegdist(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)]))~as.factor(facetFactor), data = gulfPN)

### SIMPER
simGulf = simper(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)]), 
                group=gulfPN$facetFactor)
summary(simGulf)

################################################################################
## Pacific

# Remember, the tests here are between field seasons, not bays!! Since there is only one bay
# I am removing data from 

# Remove data from March ("reduced" aka "red") since there are only 2 samples and I need to create variance for statistical tests
# However, I will keep them in the SIMPER tests, since I am not looking at significance
pacPNred = pacPN %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))

### DISPERSION
pacDisp = betadisper(sqrt(vegdist(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)])), as.factor(pacPNred$facetFactor), type = "centroid")
pairPacDisp = permutest(pacDisp, pairwise = T, permutations = 9999)

# Look at pairRegDisp for F-values, permuted p-values
pairPacDisp
pairPacDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

boxplot(pacDisp, xlab = NULL)

### PERMANOVA
# Are there differences between bays (facetFactor)
adonis2(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)]~
          as.factor(pacPNred$facetFactor), method="bray", sqrt.dist = T, perm = 9999)

# Pairwise comparisons between bays
pairwise.adonis2(sqrt(vegdist(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)]))~as.factor(facetFactor), data = pacPNred)

### SIMPER
# Here I am using pacPN not pacPNred because I want to know which species make the March 2021 data different
# I can see visually they are different
simPac = simper(sqrt(pacPN[,which(colnames(pacPN)== "Acartia spp."):ncol(pacPN)]), 
                 group=pacPN$facetFactor)
summary(simPac)

################################################################################
## Newfoundland

# No significance tests since only one bay and one field season (so far)


################################################################################
### ALTERNATIVE METHODS 

### Nested PERMANOVA
# There are too many issues to comment on right now. I'll come back to this, maybe

# Here is attempt #1
# Need to remove these 2 with no label
nestedPerm = allRegionsWide %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))

adonis2(nestedPerm[,12:ncol(allRegionsWide)]~as.factor(nestedPerm$ocean)/as.factor(nestedPerm$region)/as.factor(nestedPerm$facetFactor)/as.factor(nestedPerm$myLabel), method="bray", sqrt.dist = T)


### Pairwise comparisons (PERMANOVA) from a different package
# Don't use this one because it uses adonis not adonis 2 (even though it gives the same results??)
# Need to install from GitHub to get pairwise.adonis
remotes::install_github("Jtrachsel/funfuns")
library("funfuns")

# With the function from funfuns package
pairwise.adonis(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), sim.method="bray")

# This is another way to do the pairwise comparisons for BETADISPER, although they do not come with t-values
# See here for more info: https://rdrr.io/rforge/vegan/man/permutest.betadisper.html
reg.HSD = TukeyHSD(regionDisp)
reg.HSD
plot(reg.HSD)
