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
# Get things set up

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("Figures/nmdsSymbols.R")


# Need to remove these 2 with no label
permData = allRegionsWide %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))


adonis2(permData[,12:ncol(allRegionsWide)]~as.factor(permData$ocean)/as.factor(permData$region)/as.factor(permData$facetFactor)/as.factor(permData$myLabel), method="bray", sqrt.dist = T)


### Test Atlantic vs Pacific data

# Do the PERMANOVA
adonis2(permData[,12:ncol(allRegionsWide)]~as.factor(permData$ocean), method="bray", sqrt.dist = T, perm = 9999)

simOcean = simper(sqrt(permData[,12:ncol(allRegionsWide)]), group=permData$ocean)
summary(simOcean)


marPN = marMerge %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

adonis2(marPN[,12:ncol(marPN)]~as.factor(marPN$facilityName), method="bray", sqrt.dist = T)

source("Figures/corstarsl.R")
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