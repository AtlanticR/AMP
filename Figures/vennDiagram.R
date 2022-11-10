####################################################################################
#### Venn Diagrams
# For showing species breakdowns between regions, bays, etc.

####################################################################################

# Add nmds script since this is where I have done basic dataframe manipulation
# for each spatial scale 
source("Figures/nmdsSymbols.R")

# Install packages: 2 possible ones to use 
install.packages("ggVennDiagram")
library("ggVennDiagram")

install.packages("ggvenn")
library("ggvenn")

testMar = marMerge %>%
  group_by(class, facetFactor) %>%
  summarize(countPerClass = sum(abund)) %>%
  filter(countPerClass > 0)

argyle = testMar %>%
  filter(facetFactor == "Argyle")
sober = testMar %>%
  filter(facetFactor == "Sober Island")
country = testMar %>%
  filter(facetFactor == "Country Harbour")
whitehead = testMar %>%
  filter(facetFactor == "Whitehead")

argyleVec = as.vector(argyle$class)
soberVec = as.vector(sober$class)
countryVec = as.vector(country$class)
whiteheadVec = as.vector(whitehead$class)


x = list("Argyle" = argyleVec, "Sober Island" = soberVec, "Country Harbour" = countryVec, "Whitehead" = whiteheadVec)

# Option 1 (ugly, but more customizable)
ggVennDiagram(x)

# Option 2 (prettier, but maybe less customizable?)
ggvenn(x, set_name_size = 4.5)







###### 
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



