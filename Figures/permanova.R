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

######## IMPORTANT NOTE TO SELF:
# I SQUARE ROOT TRANSFORMED AT THE WRONG SPOT AHHHHHHHHHHHHHH
# COME BACK AND FIX THIS!!!!!
# I NEED TO SQUARE ROOT TRANSFORM THE ABUNDANCE MATRIX. AND THEN TAKE BRAY-CURTIS
# RIGHT NOW IN MANY SPOTS I AM SQUARE ROOT TRANFORMING THE BRAY-CURTIS DISS MATRIX WHICH IS INCORRECT


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
# It's  a bit clunky, but data in the species matrix always starts with 'Acartia spp." until the last column (ncol)
# I am using type = "centroid" instead of "spatial" (spatial median) to be consistent with PRIMER. In R, "spatial" is default
# See PRIMER manual for brief explanation of the differences. Manual recommends "centroid" in most cases
oceanDisp = betadisper(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$ocean), type = "centroid")

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
adonis2(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])~
          as.factor(allRegionsWide$ocean), method="bray", sqrt.dist = F, perm = 9999)

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
regDisp = betadisper(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), type = "centroid")
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
adonis2(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])~
  as.factor(allRegionsWide$region), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Make sure I get same results using vegdist. I do!
regPNresults = adonis2(vegdist(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)]))~
          as.factor(allRegionsWide$region), sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between each region
# I don't think I actually need the as.factor() but I've added it just in case
# Could take the square root of the F-value to get t-values (see PERMANOVA/PRIMER guide)
# Note to get 4 decimal places for P-value, need to set perm = 9999. Function help says "nperm" which is wrong
# set seed to get reproducible p-values
regPairwisePN = pairwise.adonis2(vegdist(sqrt(allRegionsWide[12:ncol(allRegionsWide)]))~as.factor(region), data = allRegionsWide, perm=9999, set.seed(13))


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
marDisp = betadisper(vegdist(sqrt(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)])), as.factor(marPN$facetFactor), type = "centroid")
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
marPNresults= adonis2(sqrt(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)])~
               as.factor(marPN$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
marPairwisePN = pairwise.adonis2(vegdist(sqrt(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]))~as.factor(facetFactor), data = marPN, perm =9999, set.seed(13))

### SIMPER
simMar = simper(sqrt(marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]), 
                   group=marPN$facetFactor, permutations = 9999)
summary(simMar)


################################################################################
## GULF

### DISPERSION
gulfDisp = betadisper(vegdist(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)])), as.factor(gulfPN$facetFactor), type = "centroid")
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
gulfPNresults = adonis2(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)])~
          as.factor(gulfPN$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
gulfPairwisePN = pairwise.adonis2(vegdist(sqrt(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)]))~as.factor(facetFactor), data = gulfPN, perm =9999, set.seed(13))

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
pacDisp = betadisper(vegdist(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)])), as.factor(pacPNred$facetFactor), type = "centroid")
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
pacPNresults = adonis2(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)])~
          as.factor(pacPNred$facetFactor), method="bray", sqrt.dist = F, perm = 9999, set.seed(13))

# Pairwise comparisons between bays
pacPairwisePN = pairwise.adonis2(vegdist(sqrt(pacPNred[,which(colnames(pacPNred)== "Acartia spp."):ncol(pacPNred)]))~as.factor(facetFactor), data = pacPNred, perm = 9999, set.seed(13))

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


#### NMDS of SIMPER

ggplot() + 
 geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, fill = allRegionsWide$region, size = allRegionsWide$`Acartia spp.`), pch = 21, alpha= 0.9)+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  #scale_fill_manual(name = "Region")+ 
  scale_size(range=c(0.1, 20), name="Acartia")+
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4, hjust=1)+
  #ggtitle("Atlantic and Pacific")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

################################################################################
################################################################################
### EXTRACTING THE DATA FROM THE TESTS I JUST RAN



################################################################################
## DISPERSION

# Create a function that reads in the results of the betadisper test and puts it all into 
# a data frame so it can be exported as a csv
# This includes the main test, but also the pairwise comparisons 
dispCreateTable = function(permDispResults){

  # Get the results from the main betadisper test (the table with the degrees of freedom, sum of squares etc.)
  dispDf = data.frame(permDispResults$tab) %>%
    tibble::rownames_to_column(var = "Source") %>%
    # Create a "Total" row that sums a few (but not all) of the columns 
    # I'm confused why the column names have to be written with ` ` when data.frame() gets rid of the spacing. Oh well, it works!
    # It has something to do with the way everything is piped..
    do(bind_rows(., data.frame(Source="Total", Df = sum(permDispResults$tab$Df), `Sum Sq` = sum(permDispResults$tab$`Sum Sq`),
                               `Mean Sq` = sum(permDispResults$tab$`Mean Sq`)))) %>% 
    # round some values to 3 decimal places
    mutate(across(c(Sum.Sq, Mean.Sq, F), round, 3)) %>%
    select(-N.Perm)
  
  
  # Create data frame with t-values and permuted p-values
  # Drop the first value from statistic column since it represents the overall f-value
  dispPairwiseStats = data.frame(tstat = permDispResults$statistic[-1], permP = permDispResults$pairwise$permuted)
  
  # I think I have to break up this step from above or things don't work
  # Make some adjustments to the table
  # The comparisons are already in alphabetical order, yay!
  dispPairwiseStats = dispPairwiseStats %>%
    tibble::rownames_to_column(var = "Comparison") %>%
    # I think I'm supposed to be taking the absolute value?? But maybe not!
    mutate(tstat = abs(tstat)) %>%
    mutate(across(c(tstat), round, 3))
  
  # Combine the ANOVA-like table with the pairwise comparison values
  # This won't be combined perfectly but that's fine!
  bind_rows(dispDf, dispPairwiseStats)

}

# Call the function to get the relevant dispersion test data in a data frame
regDisptable = dispCreateTable(pairRegDisp)
marDisptable = dispCreateTable(pairMarDisp)
gulfDisptable = dispCreateTable(pairGulfDisp)
pacDisptable = dispCreateTable(pairPacDisp)




################################################################################
## SIMPER 

# Making tables of the SIMPER outputs
# The simper function puts together data that is missing some data and values rounded
# to the incorrect number of decimal places
# I am making a function to put it all together so it is less of a mess to manually correct later
# Note that I also want the comparisons to be listed in alphabetical order, so that complicates things
# I am manually choosing the order they should be passed into the function. Then rbinding all data together


# Make adjustments to the simper table so I can export it as a csv
# I have to pass in the Summary object, so species are listed in the correct order
# The full simper table (fullObject) has species listed alphabetically. But I need this to get the "Overall average dissimilarity"
# Also pass in the names of groups being compared. First one is "avA" (average abundance from group A) and second is "avB"". It
# is easier to just rearrange these manually in Excel so group names can be listed alphabetically in the final table
simDfMaker = function(summaryObject, fullObject, comparisonNames){
                     
  summaryObject %>%
    # Make species names the first column (right now they are rownames)
    mutate(species = rownames(summaryObject), .before = average) %>%
    # Turn cumulative contributions into just contributions for each row. Change location of column.
    mutate(cont = diff(c(0, cumsum)), .before = cumsum) %>% 
    # Add new column to show which sites were compared and their order (will be manually moved in Google Doc table)
    mutate(comp = comparisonNames) %>% 
    # Add the overall average dissimilarity (will be manually moved in Google Doc table)
    mutate(overall = fullObject$overall) %>% 
    # Multiply certain columns by 100 to convert to percentages
    mutate_at(vars(c(average, sd, cumsum, cont, overall)), .funs = funs(.*100)) %>% 
    # Round most columns to 2 decimal places
    mutate(across(c(average, sd, ratio, ava, avb, cumsum, cont, overall), round, 2)) %>% 
    # Round p-value to 4 decimal places. Might change this later
    mutate(across(c(p), round, 4)) %>% 
    # Only get the top 5 species
    slice(1:5) 

}

# Run the function and pass in the appropriate data
# Remember the order is important because in the end I want the comparisons to be alphabetical

# Region comparisons

regSimperTable = rbind(
  simDfMaker(summary(simRegion)$Maritimes_Gulf, simRegion$Maritimes_Gulf, "Maritimes_Gulf"),
  simDfMaker(summary(simRegion)$Newfoundland_Gulf, simRegion$Newfoundland_Gulf, "Newfoundland_Gulf"),
  simDfMaker(summary(simRegion)$Pacific_Gulf, simRegion$Pacific_Gulf, "Pacific_Gulf"),
  simDfMaker(summary(simRegion)$Maritimes_Newfoundland, simRegion$Maritimes_Newfoundland, "Maritimes_Newfoundland"),
  simDfMaker(summary(simRegion)$Maritimes_Pacific, simRegion$Maritimes_Pacific, "Maritimes_Pacific"),
  simDfMaker(summary(simRegion)$Newfoundland_Pacific, simRegion$Newfoundland_Pacific, "Newfoundland_Pacific")
)


# Maritimes bay comparisons
marBaySimperTable = rbind(      
  simDfMaker(summary(simMar)$`Country Harbour_Argyle`, simMar$`Country Harbour_Argyle`, "Country Harbour_Argyle"),
  simDfMaker(summary(simMar)$`Sober Island_Argyle`, simMar$`Sober Island_Argyle`, "Sober Island_Argyle"), 
  simDfMaker(summary(simMar)$`Whitehead_Argyle`, simMar$`Whitehead_Argyle`, "Whitehead_Argyle"),
  simDfMaker(summary(simMar)$`Country Harbour_Sober Island`, simMar$`Country Harbour_Sober Island`, "Country Harbour_Sober Island"),
  simDfMaker(summary(simMar)$`Country Harbour_Whitehead`, simMar$`Country Harbour_Whitehead`, "Country Harbour_Whitehead"),
  simDfMaker(summary(simMar)$`Whitehead_Sober Island`, simMar$`Whitehead_Sober Island`, "Whitehead_Sober Island")
)


# Gulf bay comparisons 
gulfBaySimperTable = rbind(
  simDfMaker(summary(simGulf)$`Cocagne_Malpeque`, simGulf$`Cocagne_Malpeque`, "Cocagne_Malpeque"),
  simDfMaker(summary(simGulf)$`Cocagne_St. Peters`, simGulf$`Cocagne_St. Peters`, "Cocagne_St. Peters"), 
  simDfMaker(summary(simGulf)$`Malpeque_St. Peters`, simGulf$`Malpeque_St. Peters`, "Malpeque_St. Peters")
)
  
  
# Pacific field season comparisons
pacFieldSimperTable = rbind(
  simDfMaker(summary(simPac)$`August 2020_March 2021`, simPac$`August 2020_March 2021`, "August 2020_March 2021"),
  simDfMaker(summary(simPac)$`August 2020_June 2021`, simPac$`August 2020_June 2021`, "August 2020_June 2021"), 
  simDfMaker(summary(simPac)$`September 2021_August 2020`, simPac$`September 2021_August 2020`, "September 2021_August 2020"),
  simDfMaker(summary(simPac)$`March 2021_June 2021`, simPac$`March 2021_June 2021`, "March 2021_June 2021"), 
  simDfMaker(summary(simPac)$`September 2021_March 2021`, simPac$`September 2021_March 2021`, "September 2021_March 2021"),
  simDfMaker(summary(simPac)$`September 2021_June 2021`, simPac$`September 2021_June 2021`, "September 2021_June 2021")
)


#################################################################################
## PERMANOVA


# Create a function to combine adonis2, pairwise.adonis2 results in one data frame
# It extracts the relevant data and manipulates data to be in correct format (e.g., round values, add extra columns, etc)
# The resulting dataframe won't be perfect. But it gets it very close so I can just copy and paste the values into the tables already created in Google Docs
# If I want an exact table to be exported (e.g., if using csasdown) it will need a bit of extra work
# Function also passes in the "order" variable which specifies the order (alphabetical) the pairwise comparisons should be listed in
permCreateTable = function(permResults, pairwiseDf, order){

  # Get the results from the regular permanova (adonis2 output)
  permResults = permResults %>%
    # Multiply R2 by 100 to convert to percentages
    mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
    # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
    mutate(MS = Df/SumOfSqs, .before = R2) %>%
    # Round most columns to 2 decimal places
    mutate(across(c(SumOfSqs, MS, R2, F), round, 3))
  
  # Create a function to extract values from the pairwise.adonis2 function
  # Pass in the resulting object. It's  a list of lists so it's a bit confusing what's happening
  # I want to create a dataframe of results containing columns with the comparison (e.g., Maritimes vs Gulf), t-value, and p-value
  pairwise.df = 
    # I probably should have used dplyr but I got confused how to do it
    # Create a new dataframe by combining values into unique colums
    as.data.frame(cbind(
    # Get the names of each list element (i.e., comparisons) but drop the first one since it's named "parent_call"
    comparison = names(pairwiseDf)[-1], 
    # 4th value from the list of lists is the F-value. Each test has one F-value followed by multiple NAs just due to how the function reports results
    # I need to turn it into a number, round it to 3 decimal places, due the "format" function to make sure if there aren't 3 decimal places due to zeroes, they get added anyway
    # Then square root the F-value to turn it into a t-value (see "Pairwise Comparisons" p.26 from http://updates.primer-e.com/primer7/manuals/PERMANOVA+_manual.pdf)
    tValue = format(round(sqrt(as.numeric(na.omit(flatten_chr(map(pairwiseDf, 4))))), 3), nsmall = 3),
    # 5th value is the p-value. Convert from character to numeric and round to 4 decimal places 
    pValue = format(round(as.numeric(na.omit(flatten_chr(map(pairwiseDf, 5)))), 4), nsmall = 4))) %>%
      
      # Then rearrange them to be in alphabetical order
      slice(match(order, comparison))

  # Combine the full permanova with the pairwise outputs
  # This will not be a perfect table (e.g., columns don't perfectly line up)- I can fix that later! 
  # This is good enough that I can copy and paste values into my existing table in Google Docs
  bind_rows(permResults, pairwise.df)
  
}

# I want the pairwise comparisons to be in alphabetical order. Specify the order I want them to be in
# Note that sometimes it is alphabetical based on the second bay name below (e.g., Argyle is 1st). But that's too much work to change. I already have the row names written out in the Google Doc, I just need the data
order.regions = c("Maritimes_vs_Gulf", "Newfoundland_vs_Gulf", "Pacific_vs_Gulf", "Maritimes_vs_Newfoundland", "Maritimes_vs_Pacific", "Newfoundland_vs_Pacific")
order.marBays = c("Country Harbour_vs_Argyle", "Sober Island_vs_Argyle", "Whitehead_vs_Argyle", "Country Harbour_vs_Sober Island", "Country Harbour_vs_Whitehead", "Whitehead_vs_Sober Island")
order.gulfBays = c("Cocagne_vs_Malpeque", "Cocagne_vs_St. Peters", "Malpeque_vs_St. Peters")
order.pacField = c("September 2021_vs_August 2020", "August 2020_vs_June 2021", "September 2021_vs_June 2021")

# Make PERMANOVA tables to be exported that combine adonis2 and pairwise.adonis2 results
# Also rearranges the pairwise.adonis2 in alphabetical order
regPNtable = permCreateTable(regPNresults, regPairwisePN, order.regions)
marPNtable = permCreateTable(marPNresults, marPairwisePN, order.marBays)
gulfPNtable = permCreateTable(gulfPNresults, gulfPairwisePN, order.gulfBays)
pacPNtable = permCreateTable(pacPNresults, pacPairwisePN, order.pacField)









#################################################################################
# Things to delete but keeping just in case

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
# pairwise.adonis(sqrt(vegdist(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp."):ncol(allRegionsWide)])), as.factor(allRegionsWide$region), sim.method="bray")
# 
# # This is another way to do the pairwise comparisons for BETADISPER, although they do not come with t-values
# # See here for more info: https://rdrr.io/rforge/vegan/man/permutest.betadisper.html
# reg.HSD = TukeyHSD(regionDisp)
# reg.HSD
# plot(reg.HSD)

