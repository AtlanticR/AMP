###########################################################################################################################
### RAREFACTION CURVES

# Create rarefaction curves for each bay using the iNEXT R package
# I am also creating a data frame for each region with the asymptotic estimation of each bay 
# i.e., # of observed taxa, asymptotic estimator, confidence intervals, etc

# I am using the sample-based rarefaction approach (i.e., # of samples/tows on the x-axis)
# Currently, I am unsure how to handle the issue of unequal tow volumes 
# I think I will multiply the x-axis by the average tow volume? Or just menton that in the methods/results?

# I am also unsure how to handle the issue of different taxonomic resolutions in the data
# e.g., some measured to order, some to class, some to genus, etc. 
# Currently just treating them as separate "taxa". TBD how to handle this 

# Note, there are many different approaches for rarefaction
# I am also including the approach with the vegan package. It does not do estimates for Shannon or Simpson diversity, like
# iNEXT does
# Also use the iNEXT 4 steps package for additional information about sample completeness. I think this is beyond the scope
# of the project. I am also confused what "completeness" actually means. The definition in Chao et at al. 2020 is confusing
# See https://esj-journals.onlinelibrary.wiley.com/doi/10.1111/1440-1703.12102

# I am not actually conducting comparisons between bays. Just providing info for each bay (i.e, number of samples required, etc)
# Therefore, I am not "rarefying by sample coverage" etc. or plotting the curves on the same graph for each bay. 


# Important papers to read for taxonomic combining:
# https://www.journals.uchicago.edu/doi/full/10.1899/0887-3593%282007%2926%5B286%3AATEOTC%5D2.0.CO%3B2#_i36
# https://www.journals.uchicago.edu/doi/full/10.1086/680962
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6550328/
# https://onlinelibrary.wiley.com/doi/full/10.1111/fwb.13031

# Here's a study that uses weights with the random method to account for differences in water volume:
# https://onlinelibrary.wiley.com/doi/full/10.1002/edn3.74

# See here for some good examples
# https://royalsocietypublishing.org/doi/10.1098/rspb.2020.0248
# Has extrapolations and asymptotes

# https://www.neonscience.org/resources/learning-hub/tutorials/aquatic-diversity-algae

###########################################################################################################################
# SETUP

# Read in data with counts per bay
source("DataProcessing/bayBreakdown.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("Figures/colourPchSchemes.R")

# Install necessary packages
# iNEXT.4steps is not in CRAN yet!!
install_github('AnneChao/iNEXT.4steps')
library("iNEXT.4steps")


###########################################################################################################################
### With the iNEXT package

# For sample-based rarefaction (what I want to do) data must be converted to presence/absence

# I am prepping my data as "incidence freqencies" which normally I would only do if I have >1 assemblage
# This means that the data frame will be converted to presence/absence
# I then sum all the presence/absence values for each species
# I will then sum incidences across ALL TOWS. e.g., if Acartia was present in 14 of the 15 tows, its incidence frequency is 14

# Create function to create graphs and return summary statistics for each bay
inextPrep = function(bayData, avTowVol, colourScheme, plotLetter){

  
  # I do not want these higher-order taxa to be included
  taxa_to_remove = c("Cnidaria (larvae)", "Copepoda (nauplii)", "Invertebrate (egg, trochophore larvae)")
  
  # Remove the taxa specified above
  # However, I need to check if they actually exist in the dataframe, otherwise I'll get an error
  if(any(taxa_to_remove %in% colnames(bayData))){
    # If they are present, remove them
    bayData = bayData %>%
      select(-taxa_to_remove[taxa_to_remove %in% colnames(bayData)])
  }
  
  # Next, just extract only the taxa info:
  # Remember: extracting data is df[rows, cols]. If left blank, it includes all the data
  bayTaxa = bayData[,which(colnames(bayData)== "Acartia spp. (civ-vi)"): ncol(bayData)]

  # Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
  bayTaxa[bayTaxa>0] = 1
  
  # I feel like this could be an incidence_raw matrix but TRULY I have NO IDEA how the want the data to be formatted
  # It never works!!! Instead, convert to incidence_freq lol
  # Need to get incidence freqncies by summing the columns
  baySums = as.vector(colSums(bayTaxa))
  
  # It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
  baySumsList = list(append(baySums, nrow(bayTaxa), after = 0))
  
  # Create the iNEXT object! Calculate for all Hill numbers (q = 1, 2, and 3)
  bay.inext = iNEXT(baySumsList, q = c(0,1,2), datatype = "incidence_freq")
  
  # Plot the graph of diversity vs sampling units
  bay.gg = ggiNEXT(bay.inext, facet.var = "Order.q")+
      scale_colour_manual(values=colourScheme) +
      #geom_hline(yintercept=bay.inext$AsyEst$Estimator, linetype = "dashed")+
      scale_fill_manual(values=colourScheme)+
      scale_x_continuous(sec.axis = sec_axis(~.*avTowVol, name = bquote(paste("Cumulative water volume ",(m^-3)))))+
      xlab("Number of samples")+
      ylab("Taxa diversity")+
      ggtitle(plotLetter)+
      theme_bw(base_size = 14)+ # cool trick so I don't have to adjust the size of everything manually
      theme(
        # axis.title = element_blank(),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.position = "none",
        plot.margin=unit(c(0.1, 0.5, 0.6, 0.5),"cm"), # add spacing around plots: top, right, bottom, left
        plot.title = element_text(size = 11.5),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 8))
  
  # Calculate 80%, 90% and 95% of asymptotic estimator for richness only
  chao2_est = bay.inext$AsyEst$Estimator[1] # Get the asymptotic est for richness (the 1st of the 3 listed)
  
  # Get x, y values of richness information only
  richCoords = fortify(bay.inext) %>%
    filter(Order.q == 0)
  
  n80 = richCoords$x[which.min(abs(richCoords$y - (chao2_est*0.8)))]
  n90 = richCoords$x[which.min(abs(richCoords$y - (chao2_est*0.9)))]
  n95 = richCoords$x[which.min(abs(richCoords$y - (chao2_est*0.95)))]
  
  
  # Extract the asymptotic diversity estimates
  asy.df = data.frame(bay.inext$AsyEst) %>%
    # Remove the Assemblage column
    select(-c(Assemblage)) %>%
    # Add a column with undetected species and put the column before the s.e. column
    # Actually I don't want this anymore
    # mutate(Undetected = Estimator-Observed, .before = s.e.) %>%
    # Round numeric values to 2 decimal places
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(bay = bayData$facetFactor[1], .before = Diversity) %>%
    mutate(Diversity = ifelse(Diversity == "Species richness", "Taxa richness", Diversity)) %>%
    mutate(n80 = ifelse(Diversity == "Taxa richness", n80, ""), 
           n90 = ifelse(Diversity == "Taxa richness", n90, ""),
           n95 = ifelse(Diversity == "Taxa richness", n95, "")) %>%
    select(-c("s.e.", "LCL", "UCL")) # I actually don't want these columns
  


return(list(bay.gg, asy.df, baySumsList, asy.df, fortify(bay.inext), bay.inext))

}


### Maritimes
argInext = inextPrep(argyle, mean(argyle$waterVolAnalyzed), marColours[[1]], "(A) Argyle")
countryInext = inextPrep(country, mean(country$waterVolAnalyzed), marColours[[2]], "(B) Country Harbour")
soberInext = inextPrep(sober, mean(sober$waterVolAnalyzed), marColours[[3]], "(C) Sober Island")
whiteheadInext = inextPrep(whitehead, mean(whitehead$waterVolAnalyzed), marColours[[4]], "(D) Whitehead")

# Plots are stored in the first list element
plot_grid(argInext[[1]], countryInext[[1]], align = "v", ncol = 1)
plot_grid(soberInext[[1]], whiteheadInext[[1]], align = "v", ncol = 1)

# View each one then save it
# ggsave("mar2.png", width = 6.67, height = 4.31, units = "in", dpi = 300)
# ggsave("axisLabels.png", width = 6.67, height = 4.31, units = "in", dpi = 300)

# Get the dataframe of asymptotic estimator results
marInextResults = bind_rows(argInext[[2]], countryInext[[2]], soberInext[[2]], whiteheadInext[[2]])


### Gulf
cocagneInext = inextPrep(cocagne, mean(cocagne$waterVolAnalyzed), gulfColours[[1]], "(A) Cocagne")
malpequeInext = inextPrep(malpeque, mean(malpeque$waterVolAnalyzed), gulfColours[[2]], "(B) Malpeque")
stPetersInext = inextPrep(stPeters, mean(stPeters$waterVolAnalyzed), gulfColours[[3]], "(C) St. Peters")

# Add an extra plot of St. Peters so all plots in all regions are the same size
plot_grid(cocagneInext[[1]], malpequeInext[[1]], align = "v", ncol = 1)
plot_grid(stPetersInext[[1]], malpequeInext[[1]], align = "v", ncol = 1)
# ggsave("gulf2.png", width = 6.67, height = 4.31, units = "in", dpi = 300)

gulfInextResults = bind_rows(cocagneInext[[2]], malpequeInext[[2]], stPetersInext[[2]])


### Pacific
pacAug2020Inext = inextPrep(pacAug2020, mean(pacAug2020$waterVolAnalyzed), pacColours[[1]], "(A) August 2020")
pacJun2021Inext = inextPrep(pacJun2021, mean(pacJun2021$waterVolAnalyzed), pacColours[[3]], "(B) June 2021")
pacSept2021Inext = inextPrep(pacSept2021, mean(pacSept2021$waterVolAnalyzed), pacColours[[4]], "(C) September 2021")

# For now, add one extra plot so all the graphs look the same for each region
plot_grid(pacAug2020Inext[[1]], pacJun2021Inext[[1]], align = "v", ncol = 1)
plot_grid(pacJun2021Inext[[1]], pacSept2021Inext[[1]], align = "v", ncol = 1)

# ggsave("pac2.png", width = 6.67, height = 4.31, units = "in", dpi = 300)

pacInextResults = bind_rows(pacAug2020Inext[[2]], pacJun2021Inext[[2]], pacSept2021Inext[[2]])

### Newfoundland
seArm2020Inext = inextPrep(seArm2020, mean(seArm2020$waterVolAnalyzed), nlColours[[1]], "(A) September 2020")
seArm2021Inext = inextPrep(seArm2021, mean(seArm2021$waterVolAnalyzed), "dark blue", "(B) October 2021")

nlInextResults = bind_rows(seArm2020Inext[[2]], seArm2021Inext[[2]])


plot_grid(seArm2020Inext[[1]], seArm2021Inext[[1]], align = "v", ncol = 1)
# ggsave("nl.png", width = 6.67, height = 4.31, units = "in", dpi = 300)


# write.csv(marInextResults, "marInextResults.csv")
# write.csv(gulfInextResults, "gulfInextResults.csv")
# write.csv(pacInextResults, "pacInextResults.csv")
# write.csv(nlInextResults, "nlInextResults.csv")


###############################################################################
###############################################################################

## Create an extra figure that plots the Percent of estimated richness (y-axis) vs # of samples (x-axis)

# Prep the data from each region
# This really should have been a function, but I was in a rush

# Lemmens Aug 2020
iLemAug20 = pacAug2020Inext[[5]] %>%
  mutate(dataset = "Lemmens Aug 2020") %>%
  mutate(percTax = y / pacAug2020Inext[[2]]$Estimator[[1]] * 100) %>% # percent of estimated richness
  mutate(region = "Pacific") %>%
  mutate(xVol = x * mean(pacAug2020$waterVolAnalyzed))
  
# Lemmens June 2021
iLemJun21 = pacJun2021Inext[[5]] %>%
  mutate(dataset = "Lemmens Jun 2021") %>%
  mutate(percTax = y / pacJun2021Inext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Pacific") %>%
  mutate(xVol = x * mean(pacJun2021$waterVolAnalyzed))

# Lemmens Sept 2021
iLemSep21 = pacSept2021Inext[[5]] %>%
  mutate(dataset = "Lemmens Sept 2021") %>%
  mutate(percTax = y / pacSept2021Inext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Pacific") %>%
  mutate(xVol = x * mean(pacSept2021$waterVolAnalyzed))
  
# Cocagne
iCoc = cocagneInext[[5]] %>%
  mutate(dataset = "Cocagne") %>%
  mutate(percTax = y / cocagneInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Gulf") %>%
  mutate(xVol = x * mean(cocagne$waterVolAnalyzed))

# Malpeque
iMal = malpequeInext[[5]] %>%
  mutate(dataset = "Malpeque") %>%
  mutate(percTax = y / malpequeInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Gulf") %>%
  mutate(xVol = x * mean(malpeque$waterVolAnalyzed))

# St. Peters
iStPeters = stPetersInext [[5]] %>%
  mutate(dataset = "St. Peters") %>%
  mutate(percTax = y / stPetersInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Gulf") %>%
  mutate(xVol = x * mean(stPeters$waterVolAnalyzed))

# Argyle
iArgyle = argInext[[5]] %>%
  mutate(dataset = "Argyle") %>%
  mutate(percTax = y / argInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Maritimes") %>%
  mutate(xVol = x * mean(argyle$waterVolAnalyzed))

# Country Harbour
iCountry = countryInext[[5]] %>%
  mutate(dataset = "Country Harbour") %>%
  mutate(percTax = y / countryInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Maritimes") %>%
  mutate(xVol = x * mean(country$waterVolAnalyzed))

# Sober Island
iSober = soberInext[[5]] %>%
  mutate(dataset = "Sober Island") %>%
  mutate(percTax = y / soberInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Maritimes") %>%
  mutate(xVol = x * mean(sober$waterVolAnalyzed))

# Whitehead
iWhitehead = whiteheadInext[[5]] %>%
  mutate(dataset = "Whitehead") %>%
  mutate(percTax = y / whiteheadInext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Maritimes") %>%
  mutate(xVol = x * mean(whitehead$waterVolAnalyzed))

# South Arm Sept 2020
iSEAsept = seArm2020Inext[[5]] %>%
  mutate(dataset = "Southeast Arm Sept 2020") %>%
  mutate(percTax = y / seArm2020Inext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Newfoundland") %>%
  mutate(xVol = x * mean(seArm2020$waterVolAnalyzed))

# South Arm Oct 2021
iSEAoct = seArm2021Inext[[5]] %>%
  mutate(dataset = "Southeast Arm Oct 2021") %>%
  mutate(percTax = y / seArm2021Inext[[2]]$Estimator[[1]] * 100) %>%
  mutate(region = "Newfoundland") %>%
  mutate(xVol = x * mean(seArm2021$waterVolAnalyzed))
  

# Get the "rarefied" part of the data (i.e., to plot as a solid line, up to the observed data point)
datRar = rbind(iLemAug20, iLemJun21, iLemSep21, iCoc, iMal, iStPeters, iArgyle, iCountry, iSober, iWhitehead, iSEAsept, iSEAoct) %>%
  filter(Order.q == 0) %>%
  filter(Method != "Extrapolation")

# Get the observed data (to plot as a single point)
datObs = rbind(iLemAug20, iLemJun21, iLemSep21, iCoc, iMal, iStPeters, iArgyle, iCountry, iSober, iWhitehead, iSEAsept, iSEAoct) %>%
  filter(Order.q == 0) %>%
  filter(Method == "Observed")

# Get the "extrapolated" part of the data (to plot as a dashed line)
datExt = rbind(iLemAug20, iLemJun21, iLemSep21, iCoc, iMal, iStPeters, iArgyle, iCountry, iSober, iWhitehead, iSEAsept, iSEAoct) %>%
  filter(Order.q == 0) %>%
  filter(Method != "Rarefaction")

# Set the colour scheme for each site
colsRar = c("Argyle" = marColours[[1]], "Country Harbour" = marColours[[2]], "Sober Island" = marColours[[3]], "Whitehead" = marColours[[4]],
            "Lemmens Aug 2020" = pacColours[[1]], "Lemmens Jun 2021" = pacColours[[2]], "Lemmens Sept 2021" = pacColours[[3]],
            "Cocagne" = gulfColours[[1]], "Malpeque" = gulfColours[[2]], "St. Peters" = gulfColours[[3]],
            "Southeast Arm Sept 2020" = nlColours[[1]], "Southeast Arm Oct 2021" = "dark blue")



### Make the plot: percent of estimated richness (y-axis) vs number of samples (x-axis)
ggplot()+
  geom_line(data = datRar %>% filter(region == "Gulf"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Gulf"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Gulf"), mapping = aes(x=x, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Gulf", guide = guide_legend(order = 1))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Maritimes"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Maritimes"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Maritimes"), mapping = aes(x=x, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Maritimes", guide = guide_legend(order = 2))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Newfoundland"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Newfoundland"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Newfoundland"), mapping = aes(x=x, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Newfoundland", guide = guide_legend(order = 3))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Pacific"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Pacific"), mapping = aes(x = x, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Pacific"), mapping = aes(x=x, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Pacific", guide = guide_legend(order = 4))+
  
  xlab("Number of samples")+
  ylab("Percent of estimated richness")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )




#######
# Alternative plots


### Richness (y-axis) vs number of samples (x-axis). All data
ggplot()+
  geom_line(data = datRar %>% filter(region == "Gulf"), mapping = aes(x = x, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Gulf"), mapping = aes(x = x, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Gulf"), mapping = aes(x=x, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Gulf", guide = guide_legend(order = 1))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Maritimes"), mapping = aes(x = x, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Maritimes"), mapping = aes(x = x, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Maritimes"), mapping = aes(x=x, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Maritimes", guide = guide_legend(order = 2))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Newfoundland"), mapping = aes(x = x, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Newfoundland"), mapping = aes(x = x, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Newfoundland"), mapping = aes(x=x, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Newfoundland", guide = guide_legend(order = 3))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Pacific"), mapping = aes(x = x, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Pacific"), mapping = aes(x = x, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Pacific"), mapping = aes(x=x, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Pacific", guide = guide_legend(order = 4))+
  
  xlab("Number of samples")+
  ylab("Richness")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )


### Richness (y-axis) vs water (x-axis)
ggplot()+
  geom_line(data = datRar %>% filter(region == "Gulf"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Gulf"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Gulf"), mapping = aes(x=xVol, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Gulf", guide = guide_legend(order = 1))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Maritimes", guide = guide_legend(order = 2))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Newfoundland", guide = guide_legend(order = 3))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=y, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=y, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Pacific", guide = guide_legend(order = 4))+
  
  xlab("Number of samples")+
  ylab("Water volume")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )


### Percent of estimated richness (y-axis) vs water (x-axis)
ggplot()+
  geom_line(data = datRar %>% filter(region == "Gulf"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Gulf"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Gulf"), mapping = aes(x=xVol, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Gulf", guide = guide_legend(order = 1))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Maritimes"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Maritimes", guide = guide_legend(order = 2))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Newfoundland"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Newfoundland", guide = guide_legend(order = 3))+
  
  new_scale_colour()+
  geom_line(data = datRar %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5)+
  geom_line(data = datExt %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 1.5, linetype = "dashed")+
  geom_point(data = datObs %>% filter(region == "Pacific"), mapping = aes(x = xVol, y=percTax, col = dataset), size = 4)+
  scale_colour_manual(values = colsRar, name = "Pacific", guide = guide_legend(order = 4))+
  
  xlab("Number of samples")+
  ylab("Percent of estimated richness")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )




################################################################################
################################################################################
################################################################################
################################################################################

#### Everything below this point is just drafts of things, and me testing stuff



# # Estimate the number of samples it would take to obtain 95%, 90% and 80% of taxa
# # within a bay: the long way
# # There must be an easier way to do this, but I can't see one?
# # I figured it out! It's written into the function above
# # But I am keeping this as a reference just in case
# 
# # Argyle
# argInext[[4]]$Estimator[1] # asymptotic diversity ie Chao2
# 
# # What's the diversity at 2x the sample size (up to extrapolation point)
# estimateD(argInext[[3]], q = 0, datatype = "incidence_freq", level = 30)$qD
# # Gives same answer as 
# estimateD(argInext[[3]], q = 0, datatype = "incidence_freq")$qD # does sampling 2x the # of samples hit the 95% mark?
# 
# # Figure out 80%
# estimateD(argInext[[3]], q = 0, datatype = "incidence_freq", level = 17)$qD # does sampling 2x the # of samples hit the 95% mark?
# 
# 
# pacSept2021Inext[[4]]$Estimator[1]*0.95
# estimateD(pacSept2021Inext[[3]], q = 0, datatype = "incidence_freq", level = 58)$qD
# 
# 
# ###########################################################################################################################
# ### Practice with the vegan package
# # The vegan package also creates rarefaction curves but it is only for richness. And does not do extrapolation although asymptotic estimators can be calculated
# 
# # I think I finally get it
# # "exact" is "sample-based rarefaction". Some say that sample-based is not true rarefaction, it is averaged species accumulation curves
# # They also state that "rarefaction" is "individual-based rarefaction". This is the distinction the vegan package makes and it's why it's so confusing
# # Others say this distinction is dumb and both count as "rarefaction". I am going to call them both "rarefaction"
# 
# # use the specaccum function for both individual-based and sample-based rarefaction
# # The "exact" method is for sample-based rarefaction. It is also called the "Mao Tau" estimator 
# # see https://academic.oup.com/jpe/article/5/1/3/1296712 and vegan help for specaccum
# 
# # Make a plot for the Argyle data just to test it
# 
# # Think about this some more: I think rounding (especially if close to zero) causes slightly different plots
# # But I will be using sample-based rarefaction where it's just presence-absence. So... it's not a huge issue?
# 
# # Make plots of both sample-based and individual-based
# plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method= "exact")) # sample-based
# 
# # Plot individual-based rarefaction over top. Data need to be rounded. (individual-based should be higher)
# lines(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method = "rarefaction"), lty = 4)
# 
# # I think it would make sense if individual-based rarefaction was instead plotted with xvar = "individuals" as default
# # It is odd to me that is not the default (it scales the x-axis to "Sites" which is confusing)
# plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method = "rarefaction"), xvar = "individuals")
# 
# x = as.matrix(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi) (civ-vi)"): ncol(argyle)])
# 
# plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi) (civ-vi)"): ncol(argyle)]), method= "random", weights = argyle$waterVolume))
# 
# 
# sp1 = specaccum(x)
# sp2 = specaccum(x, "random", weights = argyle$waterVolume)
# plot(sp2)
# 
# mod1 = fitspecaccum(x, "lomolino")
# 
# mod1 = fitspecaccum(sp1, "lomolino")
# 
# plot(sp1)
# plot(mod1, add = T, col = 2, lwd = 2)
# 
# 
# mods = fitspecaccum(sp2, "arrh")
# plot(mods, col = "hotpink")
# boxplot(sp2, col = "yellow", border = "blue", lty = 1, cex = 0.3, add = T)
# 
# sapply(mods$models, AIC)
# 
# 
# 
# ###########################################################################################################################
# # FOR PRESENTATIONS
# # For CSRF meeting I only want Newfoundland (poorly-sampled, curve doesn't level off) and St. Peters data (well-sampled, curve levels off)
# 
# inextCSRF = function(bayData, colourScheme, plotLetter){
#   
#   # I do not want these higher-order taxa to be included
#   taxa_to_remove = c("Cnidaria (larvae)", "Copepoda (nauplii)", "Invertebrate (egg, trochophore larvae)")
#   
#   # Remove the taxa specified above
#   # However, I need to check if they actually exist in the dataframe, otherwise I'll get an error
#   if(any(spp_to_remove %in% colnames(bayData))){
#     # If they are present, remove them
#     bayData = bayData %>%
#       select(-taxa_to_remove[taxa_to_remove %in% colnames(bayData)])
#   }
#   
#   # First, just extract only the taxa info:
#   # Remember: extracting data is df[rows, cols]. If left blank, it includes all the data
#   bayTaxa = bayData[,which(colnames(bayData)== "Acartia spp. (civ-vi)"): ncol(bayData)]
#   
#   # Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
#   bayTaxa[bayTaxa>0] = 1
#   
#   # I feel like this could be an incidence_raw matrix but TRULY I have NO IDEA how the want the data to be formatted
#   # It never works!!! Instead, convert to incidence_freq lol
#   # Need to get incidence freqncies by summing the columns
#   baySums = as.vector(colSums(bayTaxa))
#   
#   # It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
#   baySumsList = list(append(baySums, nrow(bayTaxa), after = 0))
#   
#   # Create the iNEXT object! Calculate for richness only
#   bay.inext = iNEXT(baySumsList, q = c(0), datatype = "incidence_freq")
#   # Plot the graph of diversity vs sampling units
#   
#   # Calculate 80%, 90% and 95% of asymptotic estimator for richness only
#   chao2_est = bay.inext$AsyEst$Estimator[1] # Get the asymptotic est for richness (the 1st of the 3 listed)
#   
#   ggiNEXT(bay.inext, color.var = "Order.q")+
#     scale_colour_manual(values=colourScheme) +
#     scale_fill_manual(values=colourScheme)+
#     #scale_y_continuous(limits = c(13, 40))+
#     xlab("Number of samples")+
#     ylab("Taxa richness")+
#     geom_hline(aes(yintercept = chao2_est), col = "red", linetype = "dashed")+
#     # geom_text(aes( 0, chao2_est, label = chao2_est, vjust = -1), size = 3)+
#     ggtitle(plotLetter)+
#     theme_bw(base_size = 18)+ # cool trick so I don't have to adjust the size of everything manually
#     theme(
#       #axis.title.x = element_blank(),
#       legend.position = "none",
#       plot.margin=unit(c(0.1, 1, 0.6, 0.5),"cm"), # add spacing around plots: top, right, bottom, left
#       plot.title = element_text(size = 15),
#       plot.title.position = "plot")
#   
# }
#  
# # Pass in the dataframe of species counts, colour, and label
# argCSRF = inextCSRF(argyle, marColours[[1]], "(A) Argyle")
# countryCSRF = inextCSRF(country, marColours[[2]], "(B) Country Harbour")
# soberCSRF = inextCSRF(sober, marColours[[3]], "(C) Sober Island")
# whiteheadCSRF = inextCSRF(whitehead, marColours[[4]], "(D) Whitehead")
# 
# #  Plot them both together
# plot_grid(argCSRF, countryCSRF, soberCSRF, whiteheadCSRF, ncol = 4)
# 
# cocagneCSRF = inextCSRF(cocagne, gulfColours[[1]], "(A) Cocagne")
# malpequeCSRF = inextCSRF(malpeque, gulfColours[[2]], "(B) Malpeque")
# stPetersCSRF = inextCSRF(stPeters, gulfColours[[3]], "(C) St. Peters")
# plot_grid(cocagneCSRF, malpequeCSRF, stPetersCSRF, ncol = 3)
# 
# 
# pacAug2020CSRF = inextCSRF(pacAug2020, pacColours[[1]], "(A) August 2020")
# pacJun2021CSRF = inextCSRF(pacJun2021, pacColours[[3]], "(B) June 2021")
# pacSept2021CSRF = inextCSRF(pacSept2021, pacColours[[4]], "(C) September 2021")
# plot_grid(pacAug2020CSRF, pacJun2021CSRF, pacSept2021CSRF, ncol = 3)
# 
# seArm2020CSRF = inextCSRF(seArm2020, nlColours[[1]], "(A) September 2020")
# seArm2021CSRF = inextCSRF(seArm2021, "dark blue", "(B) October 2021")
# plot_grid(seArm2020CSRF, seArm2021CSRF, ncol = 2)
# 
# 
# 
# ###########################################################################################################################
# # Play around with HT vs LT data to see if either is adequately sampled
# 
# # Test breaking up HT/LT data
# 
# argHT = argyle %>%
#   subset(tidePhase == "High")
# 
# argLT = argyle %>%
#   subset(tidePhase == "Low")
# 
# 
# argHTtaxa = argHT[,which(colnames(argHT)== "Acartia spp. (civ-vi)"): ncol(argHT)]
# argLTtaxa = argLT[,which(colnames(argLT)== "Acartia spp. (civ-vi)"): ncol(argLT)]
# 
# argHTtaxa[argHTtaxa>0] = 1
# argLTtaxa[argLTtaxa>0] = 1
# 
# argHTsums = as.vector(colSums(argHTtaxa))
# argLTsums = as.vector(colSums(argLTtaxa))
# 
# argTides = list("High Tide" = append(argHTsums, nrow(argHTtaxa), after = 0),
#                    "Low Tide" = append(argLTsums, nrow(argLTtaxa), after = 0))
# 
# hi = iNEXT(argTides, q = c(0,1,2), datatype = "incidence_freq")
# 
# ggiNEXT(hi, facet.var = "Order.q", type = 1)
# 
# 
# 
# ###########################################################################################################################
# ### Using the iNEXT4steps methods i.e., Chao et al. (2020)
# # Recall that iNEXT.4steps is not in CRAN yet!!
# 
# argTaxa = argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]
# 
# # Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
# argTaxa[argTaxa>0] = 1
# 
# # I feel like this could be an incidence_raw matrix but TRULY I have NO IDEA how the want the data to be formatted
# # It never works!!! Instead, convert to incidence_freq lol
# # Need to get incidence freqncies by summing the columns
# argSums = as.vector(colSums(argTaxa))
# 
# # It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
# argSumsList = list(append(argSums, nrow(argTaxa), after = 0))
# 
# 
# # I need to play around with this, but I think my data needs to be in data frame format
# arg4StepsPrep = as.data.frame(argSumsList)
# 
# # Computes everything
# arg4Steps = iNEXT4steps(arg4StepsPrep, datatype = "incidence_freq", diversity = "TD")
# 
# arg4Steps$summary # gives summary data
# arg4Steps$figure # gives all the figures together
# 
# # Note: There are ways to get the figures separate. But that is for another day!!
# 
# cocagneInext[[1]]
# 
# estimateD(cocagneInext[[3]], q = 0, datatype = "incidence_freq")
# 
# 
# estimateD(whiteheadInext[[3]], q = 0, datatype = "incidence_freq", level = 1000)
# 
# factorial(8)
# (factorial(4))^2
