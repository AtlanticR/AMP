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


### Test Atlantic vs Pacific data

adonis2(permData[,12:ncol(allRegionsWide)]~as.factor(permData$ocean), method="bray", sqrt.dist = T, perm = 9999)

simOcean = simper(sqrt(permData[,12:ncol(allRegionsWide)]), group=permData$ocean)
summary(simOcean)


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