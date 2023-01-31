###### COPEPODS
# Get the counts for copepods for Anais
# This is for a student from UNB who might be using the data

source("DataProcessing/zooplanktonCounts.R")

# Get FlowCam counts of all copepod taxa
taxaCountsBay = rbind(gulfMerge, marMerge, nlMerge, pacMerge) %>%
  group_by(class, dataset, facetFactor) %>%
  summarize(countPerClass = sum(count)) %>%
  filter(countPerClass > 0)%>%
  filter(class == "Acartia spp." | 
           class == "Calanoida (unid)"|
      class == "Calanus spp." | 
      class == "Centropages spp." | 
      class == "Chiridus spp." | 
      class == "Copepoda nauplii" | 
      class == "Corycaeidae" |
  class == "Cyclopoida" | 
  class == "Eurytemora spp." | 
  class == "Euterpina acutifrons" |
  class == "Harpacticoida" |
  class == "Heterorhabdus spp." |
  class == "Labidocera spp." |
  class == "Metridia spp." |
  class == "Microcalanus spp." |
  class == "Microsetella spp." |
  class == "Monstrillidae" |
  class == "Oithona spp." |
  class == "Oncaeidae" |
  class == "Paracalanus spp." |
  class == "Pseudocalanus spp." |
  class == "Pseudodiaptomus spp." |
  class == "Temora spp." |
  class == "Temoridae spp." |
  class == "Tortanus spp.")
  
# write.csv(taxaCountsBay, "copepodsByBay.csv")
  