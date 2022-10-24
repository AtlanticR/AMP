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
# 1. At each spatial scale, conduct a PERMANOVA to determine if significant differences exist
# 2. Conduct pairwise comparisons to find out which groups are significantly different 
# 3. Conduct SIMPER analysis to determine which species contribute most to the dissimilarities
# between groups

# Created by Stephen Finnis 2022

################################################################################
## Get things set up

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("Figures/nmdsSymbols.R")

################################################################################
## A very bad first attempt at a nested PERMANOVA
# There are too many issues to comment on right now. I'll come back to this, maybe

# Need to remove these 2 with no label
nestedPerm = allRegionsWide %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))

adonis2(nestedPerm[,12:ncol(allRegionsWide)]~as.factor(nestedPerm$ocean)/as.factor(nestedPerm$region)/as.factor(nestedPerm$facetFactor)/as.factor(nestedPerm$myLabel), method="bray", sqrt.dist = T)

################################################################################
## Atlantic vs Pacific data

# Run the PERMANOVA
# First selects only the species data (i.e., starting at Acartia until the end)
adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$ocean), method="bray", sqrt.dist = T, perm = 9999)

# No need for pairwise tests since only 2 groups at this scale

# Follow up with SIMPER to find out which species contribute most to the differences 
simOcean = simper(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]), 
                                      group=allRegionsWide$ocean)

# view the SIMPER results                  
summary(simOcean)

################################################################################

adonis2(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]~
          as.factor(allRegionsWide$region), method="bray", sqrt.dist = T, perm = 9999)

install.packages("remotes")
remotes::install_github("Jtrachsel/funfuns")
library("funfuns")

# With the function from funfuns lol
pairwise.adonis(sqrt(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]), as.factor(allRegionsWide$region), sim.method="bray")

pairwise.adonis(allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)], as.factor(allRegionsWide$region), sim.method="bray")



# Compare against

install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
install.packages("amap")
library("")

?pairwise.adonis2


justSpecies = allRegionsWide[,which(colnames(allRegionsWide)== "Acartia spp. "):ncol(allRegionsWide)]
brayTest = amap::as.matrix.dist(vegdist(sqrt(justSpecies), method="bray"))


pairwise.adonis2(allRegionsWide[12:ncol(allRegionsWide)]~region, data = allRegions)


marPN = marMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

adonis2(marPN[,12:ncol(marPN)]~as.factor(marPN$facilityName), method="bray", sqrt.dist = T)


pairwise.adonis((marPN[,12:ncol(marPN)]), as.factor(marPN$facilityName), sim.method = "bray", p.adjust.m = "none")


simBay = simper(sqrt(marPN[,12:ncol(marPN)]), group=marPN$facilityName)
summary(simBay)

acartia = sqrt(permData[,12:ncol(allRegionsWide)]$`Acartia spp. `)



acartia = sqrt(permData[,12:ncol(allRegionsWide)]$`Acartia spp. `)
calanoida = sqrt(permData[,12:ncol(allRegionsWide)]$`Calanoida (unid)`)
cirripedia = sqrt(permData[,12:ncol(allRegionsWide)]$`Cirripedia nauplii`)


ggplot() + 
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, pch = allRegionsWide$ocean, fill = allRegionsWide$region, size = cirripedia), alpha= 0.9)+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_shape_manual(values = regionArray, name = "Region")+ 
  scale_size_continuous(range=c(1,20))+
  #scale_size_area()+
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4, hjust=1)+
  ggtitle("Atlantic and Pacific")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))