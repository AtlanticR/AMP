###########################################################################################################################
### RAREFACTION CURVES


###########################################################################################################################
# SETUP

# Read in data with counts per bay
source("TechReport/DataProcessing/bayBreakdown.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("TechReport/Figures/colourPchSchemes.R")

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
inextPrep = function(bayData, colourScheme, plotLetter){


  # My data frame has many extra columns. 
  # Just extract the species data (starting with Acartia spp. to the end of the data frame)
  bayTaxa = bayData[,which(colnames(bayData)== "Acartia spp. (civ-vi)"): ncol(bayData)]

  # Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
  bayTaxa[bayTaxa>0] = 1
  
  # Convert data to incidence frequencies by summing the columns
  baySums = as.vector(colSums(bayTaxa))
  
  # It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of plankton tows)
  baySumsList = list(append(baySums, nrow(bayTaxa), after = 0))
  
  # Create the iNEXT object! Calculate for all Hill numbers (q = 1, 2, and 3)
  bay.inext = iNEXT(baySumsList, q = c(0,1,2), datatype = "incidence_freq")
  
  # Plot the graph of diversity vs # of samples (plankton tows)
  bay.gg = ggiNEXT(bay.inext, facet.var = "Order.q")+
      scale_colour_manual(values=colourScheme) +
      scale_fill_manual(values=colourScheme)+
      xlab("Number of samples")+
      ylab("Taxa diversity")+
      ggtitle(plotLetter)+
      theme_bw(base_size = 14)+ # cool trick so I don't have to adjust the size of everything manually
      theme(
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        legend.position = "none",
        plot.margin=unit(c(0.1, 0.5, 0.6, 0.5),"cm"), # add spacing around plots: top, right, bottom, left
        plot.title = element_text(size = 11.5),
        plot.title.position = "plot",
        strip.text.x = element_text(size = 8))
  
  # Get the asymptotic est for richness
  chao2_est = bay.inext$AsyEst$Estimator[1]
  
  # Get x, y values of richness information only
  richCoords = fortify(bay.inext) %>%
    filter(Order.q == 0)
  
  # Extract the asymptotic diversity estimates
  asy.df = data.frame(bay.inext$AsyEst) %>%
    # Remove the Assemblage column
    select(-c(Assemblage)) %>%
    # If needed, add a column with undetected species and put the column before the s.e. column
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
argInext = inextPrep(argyle, marColours[[1]], "(A) Argyle")
countryInext = inextPrep(country, marColours[[2]], "(B) Country Harbour")
soberInext = inextPrep(sober, marColours[[3]], "(C) Sober Island")
whiteheadInext = inextPrep(whitehead, marColours[[4]], "(D) Whitehead")

# Plots are stored in the first list element
plot_grid(argInext[[1]], countryInext[[1]], align = "v", ncol = 1)
plot_grid(soberInext[[1]], whiteheadInext[[1]], align = "v", ncol = 1)

# View each one then save it
# ggsave("mar2.png", width = 6.67, height = 4.31, units = "in", dpi = 300)
# ggsave("axisLabels.png", width = 6.67, height = 4.31, units = "in", dpi = 300)

# Get the dataframe of asymptotic estimator results
marInextResults = bind_rows(argInext[[2]], countryInext[[2]], soberInext[[2]], whiteheadInext[[2]])


###############################################################################
###############################################################################

## Create an extra figure that plots the Percent of estimated richness (y-axis) vs # of samples (x-axis)

# Prep the data from each region
# This really should have been a function, but I was in a rush

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
