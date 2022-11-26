####################################################################################
#### Venn Diagrams
# For showing species breakdowns between regions, bays, etc.

####################################################################################

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("DataProcessing/zooplanktonCounts.R")

####################################################################################
##### Prepare all the data
# This is a bit clunky but I am also in a rush!

#### Maritimes

marTaxa = marMerge %>%
  group_by(class, facetFactor) %>%
  summarize(countPerClass = sum(abund)) %>%
  # Remove taxa that are not present
  filter(countPerClass > 0)

# Split up the data by bay
argyleVenn =  marTaxa %>%
  filter(facetFactor == "Argyle")
soberVenn = marTaxa %>%
  filter(facetFactor == "Sober Island")
countryVenn = marTaxa %>%
  filter(facetFactor == "Country Harbour")
whiteheadVenn = marTaxa %>%
  filter(facetFactor == "Whitehead")

# Turn it into a vector of taxa names (class) that are present
argyleVec = as.vector(argyleVenn$class)
soberVec = as.vector(soberVenn$class)
countryVec = as.vector(countryVenn$class)
whiteheadVec = as.vector(whiteheadVenn$class)

marBayVen = list("Argyle" = argyleVec, "Sober Island" = soberVec, "Country Harbour" = countryVec, "Whitehead" = whiteheadVec)

#### Gulf

gulfTaxa = gulfMerge %>%
  group_by(class, facetFactor) %>%
  summarize(countPerClass = sum(abund)) %>%
  # Remove taxa that are not present
  filter(countPerClass > 0)

# Split up the data by bay
stPetersVen = gulfTaxa %>%
  filter(facetFactor == "St. Peters")
malpequeVen = gulfTaxa %>%
  filter(facetFactor == "Malpeque")
cocagneVen = gulfTaxa %>%
  filter(facetFactor == "Cocagne")

# Turn it into a vector of taxa names (class) that are present
stPetersVec = as.vector(stPetersVen$class)
malpequeVec = as.vector(malpequeVen$class)
cocagneVec = as.vector(cocagneVen$class)

gulfBayVen = list("St. Peters" = argyleVec, "Malpeque" = soberVec, "Cocagne" = cocagneVec)


### Pacific

pacTaxa = pacMerge %>%
  group_by(class, facetFactor) %>%
  summarize(countPerClass = sum(abund)) %>%
  # Remove taxa that are not present
  filter(countPerClass > 0)

# Split up the data by bay
aug20Ven = pacTaxa %>%
  filter(facetFactor == "August 2020")
mar21Ven = pacTaxa %>%
  filter(facetFactor == "March 2021")
jun21Ven = pacTaxa %>%
  filter(facetFactor == "June 2021")
sept21Ven = pacTaxa %>%
  filter(facetFactor == "September 2021")


# Turn it into a vector of taxa names (class) that are present
aug20Vec = as.vector(aug20Ven$class)
mar21Vec = as.vector(mar21Ven$class)
jun21Vec = as.vector(jun21Ven$class)
sept21Vec = as.vector(sept21Ven$class)

pacVen = list("August 2020" = aug20Vec, "March 2021" = mar21Vec, "June 2021" = jun21Vec, "September 2021" = sept21Vec)


















# Option 1 (ugly, but more customizable)
ggVennDiagram(marBayVen)

# See here for more info on getting fill colours for the overlap regions:
# https://stackoverflow.com/questions/68875752/how-to-edit-ggvenndiagram-intersection-fill-region

venn = Venn(marBayVen)
data = process_data(venn)
data2 = process_data(venn)
data2@region = st_polygonize(data@setEdge)


colfunc = colorRampPalette(marColours)
col = colfunc(15)

getCount = round(data@region$count/sum(data@region$count)*100, 2)

getCount3 = paste("(", getCount, "%)", sep = "")

ggplot()+
  geom_sf(aes(fill = name), data = venn_region(data), show.legend = F)+
  geom_sf(color = "black", data = venn_setedge(data2), show.legend = F)+
  geom_sf_text(aes(label = name), data = venn_setlabel(data), size = 6.5)+
  geom_sf_text(aes(label = count), data = venn_region(data), vjust = -0.5, size = 6)+
  geom_sf_text(aes(label = getCount3), data = venn_region(data), vjust = 1.1, size = 4.5)+
  scale_fill_manual(values = alpha(col, 0.5))+
  scale_colour_manual(values = marColours)+
  #geom_sf(aes(color = id), data = venn_setedge(data)) +
  theme_void()+
  scale_x_continuous(expand = expansion(mult = .2)) # trick to prevent the bay label names from getting cut off



####################################################################################
# Gulf


# Option 1 (ugly, but more customizable)
ggVennDiagram(gulfBayVen)

# Option 2 (prettier, but maybe less customizable?)
ggvenn(gulfBayVen, set_name_size = 4.5)



####################################################################################

# Option 1 (ugly, but more customizable)
ggVennDiagram(pacVen)

# Option 2 (prettier, but maybe less customizable?)
ggvenn(pacVen, set_name_size = 4.5)





################## 
# Rel abundance by region

marTest = marMerge %>%
  # Want counts per taxa (class) for the whole bay, not by tow
  group_by(class, facetFactor) %>%
  summarize(countPerClass = sum(abund)) 


marTest2 = marMerge %>%
  group_by(facetFactor) %>%
  summarize(bayTotal = sum(abund))

marTest3 = left_join(marTest, marTest2, by = "facetFactor") %>%
  mutate(relAbund = countPerClass/bayTotal*100)


ggplot(marTest3, aes(x = facetFactor, y = class))+
  geom_point(aes(size = relAbund, fill=class), alpha = 0.75, shape = 21)+
  scale_size_continuous(limits = c(0.000001, 100), range = c(1,20), breaks = c(1,10,50,75))+
  theme_bw()



