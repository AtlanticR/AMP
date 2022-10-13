## Draft permanova

# I don't really know what I'm doing lol

# Possible package for nested permanova: may not work for >2 levels of nestedness
install.packages("BiodiversityR")
library("BiodiversityR")

# Need this because it puts data in the correct format
source("Figures/nmdsSymbols.R")


# Need to remove these 2 with no label
permData = allRegionsWide %>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103HT_250UM"))%>%
  filter(sampleCode != c("AMMP_PA_S04Pooled_202103LT_250UM"))


adonis2(permData[,12:ncol(allRegionsWide)]~as.factor(permData$ocean)/as.factor(permData$region)/as.factor(permData$facetFactor)/as.factor(permData$myLabel), method="bray", sqrt.dist = T)
