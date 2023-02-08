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
# For things requiring permutations, use set.seed() to get same results each time

# So I don't lose it: https://onlinelibrary.wiley.com/doi/full/10.1111/jfb.13989
# ^ simper with permutations

################################################################################
################################################################################
## Get things set up

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("Figures/nmdsRegions.R")

# Install this package to get the pairwise.adonis2 function
remotes::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

# This will avoid numbers being written in scientific notation
# (It's mostly so the p-values aren't written as e.g., 1e-04)
options(scipen = 999)

################################################################################
################################################################################
#### Test for differences between oceans
# There are two factors: Atlantic and Pacific 

### DISPERSION
# vegdist turns the data into dissimilarities. Bray-Curtis is the default
# Need to also square root transform
# It's  a bit clunky, but data in the species matrix always starts with 'Acartia spp. (civ-vi)" until the last column (ncol)
# There are two ways to define the centroid: "centroid" or "spatial median"
# See PRIMER manual for brief explanation of the differences. Manual recommends "centroid" in most cases
# However, betadisper function notes error in using type = centroid.
# See warning here: https://rdrr.io/cran/vegan/man/betadisper.html#:~:text=betadisper%20is%20a%20multivariate%20analogue,means%20of%20assessing%20beta%20diversity.
# I am switching this to type = "median" because of this
oceanDisp = betadisper(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)])), as.factor(allRegionsWide$ocean), type = "median")

# This will get you ANOVA results (i.e., are there differences in dispersion) but with non-permuted significance
anova(oceanDisp) 
# Get significance (overall) and also conduct pairwise tests. Report these results instead.
pairOceanDisp = permutest(oceanDisp, pairwise = T, permutations = 9999, set.seed(13))

# Construct boxplot to show differences in dispersion
boxplot(oceanDisp, xlab = NULL)
plot(oceanDisp)

# Look at pairRegDisp for permuted p-values. Also can get t-values for pairwise comparisons 
pairOceanDisp
pairOceanDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

# Can also use Tukey HSD to get significance between pairwise groups. Example is at the end of the script. 
# I am sticking with t-values since that is what PRIMER uses and how most people report results. 

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disOcean = data.frame(group = oceanDisp$group, distances = oceanDisp$distances)

# ggplot boxplot of distance to centroid for each group
ggplot(disOcean, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  # Don't need to add significance because groups aren't significant
  xlab("Ocean")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        # setting the margin adds space between tick labels and titles
        axis.title.x = element_text(size = 12.5, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        #plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        legend.position = "none")

### PERMANOVA
# First selects only the species data (i.e., starting at Acartia until the end)
adonis2(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)])~
          as.factor(allRegionsWide$ocean), method="bray", sqrt.dist = F, perm = 9999)

# No need for pairwise tests since only 2 groups at this scale


### SIMPER 
# Use SIMPER to find out which species contribute most to the differences 
simOcean = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)]), 
                                      group=allRegionsWide$ocean)

# View the SIMPER results                  
summary(simOcean)


################################################################################
################################################################################
#### Test for differences between DFO Regions
# There are 4 factors (in this study): Maritimes, Gulf, Newfoundland, Pacific

### DISPERSION
regDisp = betadisper(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), type = "median")
pairRegDisp = permutest(regDisp, pairwise = T, permutations = 9999, set.seed(13))

# Look at pairRegDisp for F-values, permuted p-values
pairRegDisp
pairRegDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

boxplot(regDisp, xlab = NULL)
plot(regDisp)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disRegion = data.frame(group = regDisp$group, distances = regDisp$distances)

# ggplot boxplot of distance to centroid for each group
ggplot(disRegion, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  # Add significance for each group separately. Need to offset some comparisons so they show up better
  # May need to check significance since p-values were obtained from permutation (not sure how geom_signif calculates them)
  geom_signif(comparisons = list(c("Gulf", "Maritimes")),
              map_signif_level = T, y_position = 0.8)+
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

# Test 2 different adonis methods to make sure they're all the same. They are!!
adonis2(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)])~
  as.factor(allRegionsWide$region), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Make sure I get same results using vegdist. I do!
regPNresults = adonis2(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)]))~
          as.factor(allRegionsWide$region), sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between each region
# I don't think I actually need the as.factor() but I've added it just in case
# Could take the square root of the F-value to get t-values (see PERMANOVA/PRIMER guide)
# Note to get 4 decimal places for P-value, need to set perm = 9999. Function help says "nperm" which is wrong
# set seed to get reproducible p-values
regPairwisePN = pairwise.adonis2(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)]))~as.factor(region), data = allRegionsWide, perm=9999, set.seed(13))


### SIMPER
simRegion = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)]), 
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
marDisp = betadisper(vegdist(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. (civ-vi)"):ncol(marPN)])), as.factor(marPN$facetFactor), type = "median")
pairMarDisp = permutest(marDisp, pairwise = T, permutations = 9999, set.seed(11))

# Look at pairRegDisp for F-values, permuted p-values
pairMarDisp
pairMarDisp$statistic

# boxplot in base R to show differences in dispersion between bays
boxplot(marDisp, xlab = NULL)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disMar = data.frame(group = marDisp$group, distances = marDisp$distances)

# ggplot boxplot of distance to centroid for each group
ggplot(disMar, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  scale_fill_manual(values = marColours)+
  # No groups are significantly different
  xlab("Bay")+
  ylab("Distance to centroid")+
  # AHH There is actually one significant pairwise comparison
  geom_signif(comparisons = list(c("Sober Island", "Whitehead")),
              map_signif_level = T)+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        # setting the margin adds space between tick labels and titles
        axis.title.x = element_text(size = 12.5, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        #plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        legend.position = "none")


### PERMANOVA
# Are there differences between bays (facetFactor)
marPNresults= adonis2(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. (civ-vi)"):ncol(marPN)])~
               as.factor(marPN$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
marPairwisePN = pairwise.adonis2(vegdist(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. (civ-vi)"):ncol(marPN)]))~as.factor(facetFactor), data = marPN, perm =9999, set.seed(13))

### SIMPER
simMar = simper(sqrt(marPN[,which(colnames(marPN)== "Acartia spp. (civ-vi)"):ncol(marPN)]), 
                   group=marPN$facetFactor, permutations = 9999)
summary(simMar)


################################################################################
## GULF

### DISPERSION
gulfDisp = betadisper(vegdist(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp. (civ-vi)"):ncol(gulfPN)])), as.factor(gulfPN$facetFactor), type = "median")
pairGulfDisp = permutest(gulfDisp, pairwise = T, permutations = 9999, perm = 9999, set.seed(13))

# Look at pairRegDisp for F-values, permuted p-values
pairGulfDisp
pairGulfDisp$statistic

boxplot(gulfDisp, xlab = NULL)

# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disGulf = data.frame(group = gulfDisp$group, distances = gulfDisp$distances)

# ggplot boxplot of distance to centroid for each group
ggplot(disGulf, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  scale_fill_manual(values = gulfColours)+
  ### WATCH OUT FOR THIS SIGNIFICANCE LEVEL
  geom_signif(comparisons = list(c("Malpeque", "St. Peters")),
              map_signif_level = T)+
  xlab("Bay")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        # setting the margin adds space between tick labels and titles
        axis.title.x = element_text(size = 12.5, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        #plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        legend.position = "none")

### PERMANOVA
# Are there differences between bays (facetFactor)
gulfPNresults = adonis2(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp. (civ-vi)"):ncol(gulfPN)])~
          as.factor(gulfPN$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
gulfPairwisePN = pairwise.adonis2(vegdist(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp. (civ-vi)"):ncol(gulfPN)]))~as.factor(facetFactor), data = gulfPN, perm =9999, set.seed(13))

### SIMPER
simGulf = simper(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp. (civ-vi)"):ncol(gulfPN)]), 
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
pacDisp = betadisper(vegdist(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp. (civ-vi)"):ncol(pacPNred)])), as.factor(pacPNred$facetFactor), type = "median")
pairPacDisp = permutest(pacDisp, pairwise = T, permutations = 9999, set.seed(13))

# Look at pairRegDisp for F-values, permuted p-values
pairPacDisp
pairPacDisp$statistic # I might need to take the absolute value. Negative values don't mean much in multivariate bray-curtis space????

boxplot(pacDisp, xlab = NULL)


# Extract dispersion info (distances to centroid for each group) and turn it into a dataframe for ggplot
disPac = data.frame(group = pacDisp$group, distances = pacDisp$distances)

# ggplot boxplot of distance to centroid for each group
ggplot(disPac, aes(x = group, y = distances, fill=group))+
  geom_boxplot()+
  scale_fill_manual(values = pacColours)+
  xlab("Field Season")+
  ylab("Distance to centroid")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        # setting the margin adds space between tick labels and titles
        axis.title.x = element_text(size = 12.5, margin = unit(c(3, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 12.5, margin = unit(c(0, 3, 0, 0), "mm")),
        #plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        legend.position = "none")


### PERMANOVA
# Are there differences between bays (facetFactor)
pacPNresults = adonis2(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp. (civ-vi)"):ncol(pacPNred)])~
          as.factor(pacPNred$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
pacPairwisePN = pairwise.adonis2(vegdist(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp. (civ-vi)"):ncol(pacPNred)]))~as.factor(facetFactor), data = pacPNred, perm = 9999, set.seed(13))

### SIMPER
# Here I am using pacPN not pacPNred because I want to know which species make the March 2021 data different
# I can see visually they are different
simPac = simper(sqrt(pacPN[,which(colnames(pacPN)== "Acartia spp. (civ-vi)"):ncol(pacPN)]), 
                 group=pacPN$facetFactor)
summary(simPac)

################################################################################
## Newfoundland

# No significance tests since only one bay and one field season (so far)

#################################################################################
# Things to delete but keeping just in case

################################################################################
### ALTERNATIVE METHODS 


#### NMDS of SIMPER
# ggplot() + 
#   geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, fill = allRegionsWide$region, size = allRegionsWide$`Acartia spp. (civ-vi)`), pch = 21, alpha= 0.9)+
#   # Don't need to define colours. These just show up as default ggplot colours for 4 elements
#   #scale_fill_manual(name = "Region")+ 
#   scale_size(range=c(0.1, 20), name="Acartia")+
#   annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4, hjust=1)+
#   #ggtitle("Atlantic and Pacific")+
#   theme_bw()+
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         #legend.position = "none",
#         panel.border=element_rect(color="black", size=1), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank(),
#         plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
#         plot.title = element_text(size=18))

### Nested PERMANOVA
# There are too many issues to comment on right now. I'll come back to this, maybe

# # Here is attempt #1
# # Need to remove these 2 with no label
# nestedPerm = allRegionsWide %>%
#   filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
#   filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))
# 
# adonis2(nestedPerm[,12:ncol(allRegionsWide)]~as.factor(nestedPerm$ocean)/as.factor(nestedPerm$region)/as.factor(nestedPerm$facetFactor)/as.factor(nestedPerm$myLabel), method="bray", sqrt.dist = T)
# 
# 
# ### Pairwise comparisons (PERMANOVA) from a different package
# # Don't use this one because it uses adonis not adonis 2 (even though it gives the same results??)
# # Need to install from GitHub to get pairwise.adonis
# remotes::install_github("Jtrachsel/funfuns")
# library("funfuns")
# 
# # With the function from funfuns package
# pairwise.adonis(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)"):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), sim.method="bray")
# 
# # This is another way to do the pairwise comparisons for BETADISPER, although they do not come with t-values
# # See here for more info: https://rdrr.io/rforge/vegan/man/permutest.betadisper.html
# reg.HSD = TukeyHSD(regionDisp)
# reg.HSD
# plot(reg.HSD)

