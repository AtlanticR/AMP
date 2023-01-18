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

gulfBayVen = list("St. Peters" = stPetersVec, "Malpeque" = malpequeVec, "Cocagne" = cocagneVec)


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


####################################################################################
#### MAKE THE VENN DIAGRAMS

# Original method for constructing using the ggVennDiagram function:
ggVennDiagram(pacVen)

makeVennDiagram = function(vennDataList, bayColours, plotLetter){

  # The default venn diagram is HIDEOUS and I want to use my own colour scheme
  # See here for more info on getting fill colours for the overlap regions:
  # https://stackoverflow.com/questions/68875752/how-to-edit-ggvenndiagram-intersection-fill-region
  
  # There is another weird roundabout way where you do this all in ggplot by creating a "Venn" object
  venn = Venn(vennDataList) # create the Venn object
  # gVenn2 is used for fill stuff
  # gVenn1 contains the regular data
  gVenn1 = process_data(venn) # get the plot data (I think this makes it useable for ggplot)
  gVenn2 = process_data(venn) # make another that will be turned into polygons
  gVenn2@region = st_polygonize(gVenn2@setEdge) # create polygons to form the output shapes. These get coloured
  
  # The percentages don't get saved automatically when you use the Venn() method
  # Calculate these yourself I guess
  gPercents = round(gVenn1@region$count/sum(gVenn1@region$count)*100, 2)
  
  # Put brackets around the values
  gPercentsBrackets = paste("(", gPercents, "%)", sep = "")
  
  p1 = 
  ggplot()+
    geom_sf(aes(fill = name), data = venn_region(gVenn2), show.legend = F)+  
    geom_sf(color = "black", data = venn_setedge(gVenn1), show.legend = F)+
    # Use this if I want to set the actual colours as the outlines. I think it's a bit ugly
    #geom_sf(aes(color = name), data = venn_setedge(gVenn1), show.legend = F, linewidth = 1.1)+
    geom_sf_text(aes(label = name), data = venn_setlabel(gVenn1), size = 5.5, vjust = -0.1)+
    geom_sf_text(aes(label = count), data = venn_region(gVenn1), vjust = -0.5, size = 5)+ # add richness amounts
    geom_sf_text(aes(label = gPercentsBrackets), data = venn_region(gVenn1), vjust = 0.75, size = 4.2)+ # add percents
    scale_fill_manual(values = alpha(bayColours, 0.4))+ # adjust transparency of the fill
    ggtitle(plotLetter)+
    theme_void()+
    theme(
      plot.title = element_text(size=20))+
    scale_x_continuous(expand = expansion(mult = .2)) # trick to prevent the bay label names from getting cut off

  return(p1)
}

# Note that the outlines may look jagged in the plotting window, but if you use ggsave that goes away

# Make them!
marVennPlot = makeVennDiagram(marBayVen, marColours, "(A) Maritimes")
gulfVennPlot = makeVennDiagram(gulfBayVen, gulfColours, "(B) Gulf")
pacVennPlot = makeVennDiagram(pacVen, pacColours, ("(C) Pacific"))


# Note that the (B) will not be aligned with the rest of the plots- I'll have to fix this at some point
plot_grid(marVennPlot, gulfVennPlot, pacVennPlot, align = "v", ncol = 1)

# COMPARE WITH EGG PACKAGE!!! MIGHT BE BETTER: but the sizing of the middle plot gets thrown off
ggarrange(marVennPlot, gulfVennPlot, pacVennPlot)

# NOTE ABOUT USING GGSAVE!!!!!
# do NOT use dev.size("in") to get width/height dimensions. I do NOT know why lol
# Instead, go to Export --> Save as PDF --> and copy the dimensions in the top right instead
# This will stop any annoying issues happening with font sizes 
# Always save at 300 dpi for journal-quality resolution
# Example:
# ggsave("test.png", width = 16.25, height = 14.29, units = "in", dpi = 100)
