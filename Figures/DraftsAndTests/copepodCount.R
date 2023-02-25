###### COPEPODS
# Get the counts for copepods for Anais
# This is for a student from UNB who might be using the data

source("DataProcessing/zooplanktonCounts.R")

# Get FlowCam counts of all copepod taxa
taxaCountsBay = rbind(gulfMerge, marMerge, nlMerge, pacMerge) %>%
  group_by(class, dataset, facetFactor) %>%
  summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0)%>%
  filter(class == "Acartia spp." |  # calanoida
           class == "Calanoida (unid)"| 
      class == "Calanus spp." |  # calanoida
      class == "Centropages spp." | # calanoida
      class == "Chiridus spp." | # calanoida. should be chiridius
      class == "Copepoda nauplii" | 
      class == "Corycaeidae" | # cyclopoida!
  class == "Cyclopoida" | # 
  class == "Eurytemora spp." | # calanoida
  class == "Euterpina acutifrons" | # harpacticoida
  class == "Harpacticoida" |
  class == "Heterorhabdus spp." | # calanoida
  class == "Labidocera spp." | # calanoida
  class == "Metridia spp." | # calanoida
  class == "Microcalanus spp." | # calanoida
  class == "Microsetella spp." | # harpacticoida
  class == "Monstrillidae" | # oops also it's own type: Monstroilloida
  class == "Oithona spp." | # cyclopoida
  class == "Oncaeidae" | # cyclopoida
  class == "Paracalanus spp." | # calanoida
  class == "Pseudocalanus spp." | # calanoida
  class == "Pseudodiaptomus spp." | # calanoida
  class == "Temora spp." | # calanoida
  class == "Temoridae spp." | # calanoida
  class == "Tortanus spp.") # calanoida
  
# write.csv(taxaCountsBay, "copepodsByBay.csv")
  