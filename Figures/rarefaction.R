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

# Test with updated token

###########################################################################################################################
### With the iNEXT package

# For sample-based rarefaction (what I want to do) data must be converted to presence/absence

# I am prepping my data as "incidence freqencies" which normally I would only do if I have >1 assemblage
# This means that the data frame will be converted to presence/absence
# I then sum all the presence/absence values for each species
# I will then sum incidences across ALL TOWS. e.g., if Acartia was present in 14 of the 15 tows, its incidence frequency is 14

# Create function to create graphs and return summary statistics for each bay
inextPrep = function(bayData, avTowVol, colourScheme, plotLetter){

# First, just extract only the taxa info:
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
    scale_fill_manual(values=colourScheme)+
    scale_x_continuous(sec.axis = sec_axis(~.*avTowVol, name = bquote(paste("Cumulative water volume ",(m^-3)))))+
    xlab("Number of zooplankton tows")+
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

# Extract the asymptotic diversity estimates
asy.df = data.frame(bay.inext$AsyEst) %>%
  # Remove the Assemblage column
  select(-c(Assemblage)) %>%
  # Add a column with undetected species and put the column before the s.e. column
  mutate(Undetected = Estimator-Observed, .before = s.e.) %>%
  # Round numeric values to 2 decimal places
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(bay = bayData$facetFactor[1], .before = Diversity)

# Calculate 80%, 90% and 95% of asymptotic estimator for richness only
chao2_est = bay.inext$AsyEst$Estimator[1] # Get the asymptotic est for richness (the 1st of the 3 listed)

# Get x, y values of richness information only
richCoords = fortify(bay.inext) %>%
  filter(Order.q == 0)

n80 = richCoords$x[which.min(abs( richCoords$y - (chao2_est*0.8)))]
n90 = richCoords$x[which.min(abs( richCoords$y - (chao2_est*0.9)))]
n95 = richCoords$x[which.min(abs( richCoords$y - (chao2_est*0.95)))]



print(c(chao2_est, n80, n90, n95))

return(list(bay.gg, asy.df, baySumsList, asy.df, fortify(bay.inext)))

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
pacJun2021Inext = inextPrep(pacJun2021, mean(pacJun2021$waterVolAnalyzed), pacColours[[3]], "(C) June 2021")
pacSept2021Inext = inextPrep(pacSept2021, mean(pacSept2021$waterVolAnalyzed), pacColours[[4]], "(D) September 2021")

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
# Estimate the number of samples it would take to obtain 95%, 90% and 80% of taxa
# within a bay
# There must be an easier way to do this, but I can't see one?

# Argyle
argInext[[4]]$Estimator[1] # asymptotic diversity ie Chao2

# What's the diversity at 2x the sample size (up to extrapolation point)
estimateD(argInext[[3]], q = 0, datatype = "incidence_freq", level = 30)$qD
# Gives same answer as 
estimateD(argInext[[3]], q = 0, datatype = "incidence_freq")$qD # does sampling 2x the # of samples hit the 95% mark?


argInext[[4]]$Estimator[1]* 0.95 # 32.7465 i.e. >30
argInext[[4]]$Estimator[1]* 0.9 # 31.023 i.e. >30
targ = argInext[[4]]$Estimator[1]* 0.8 # 27.576

# Figure out 80%
estimateD(argInext[[3]], q = 0, datatype = "incidence_freq", level = 17)$qD # does sampling 2x the # of samples hit the 95% mark?




# Country Harbour
countryInext[[4]]$Estimator[1]
countryInext[[4]]$Estimator[1] * 0.95
estimateD(countryInext[[3]], q = 0, datatype = "incidence_freq")$qD

# Try and get close to 34.16797
estimateD(countryInext[[3]], q = 0, datatype = "incidence_freq", level = 6)$qD
estimateD(countryInext[[3]], q = 0, datatype = "incidence_freq", level = 7)$qD

# Sober Island
soberInext[[4]]$Estimator[1]
soberInext[[4]]$Estimator[1] * 0.95
estimateD(countryInext[[3]], q = 0, datatype = "incidence_freq")$qD

# Whitehead
whiteheadInext[[4]]$Estimator[1]
whiteheadInext[[4]]$Estimator[1] * 0.95
estimateD(whiteheadInext[[3]], q = 0, datatype = "incidence_freq")$qD

# Try and get close to 29.0605
estimateD(whiteheadInext[[3]], q = 0, datatype = "incidence_freq", level = 6)$qD # 6 is closer
estimateD(whiteheadInext[[3]], q = 0, datatype = "incidence_freq", level = 7)$qD

data_list = c(argInext, countryInext, soberInext, whiteheadInext)

for(i in seq_along(data_list)){
  df = data_list[[i]][[5]] # x, y values are stored in the 5th element
  chao2_est = data_list[[4]][i]$Estimator[1]
  print(chao2_est)
  
  
}


test = argInext[[5]] %>%
  filter(Order.q == 0)

closest = test$x[which.min(abs(test$y - targ))]



# Cocagne
cocagneInext[[4]]$Estimator[1]
cocagneInext[[4]]$Estimator[1] * 0.95
estimateD(cocagneInext[[3]], q = 0, datatype = "incidence_freq")$qD

# Malpeque
malpequeInext[[4]]$Estimator[1]
malpequeInext[[4]]$Estimator[1] * 0.95
estimateD(malpequeInext[[3]], q = 0, datatype = "incidence_freq", level = 3)$qD

# St Peters
stPetersInext[[4]]$Estimator[1]
stPetersInext[[4]]$Estimator[1] * 0.95
estimateD(stPetersInext[[3]], q = 0, datatype = "incidence_freq")$qD

# Get to 37.8575
estimateD(stPetersInext[[3]], q = 0, datatype = "incidence_freq", level = 20)$qD
estimateD(stPetersInext[[3]], q = 0, datatype = "incidence_freq", level = 21)$qD

# Pacific Aug 2020
pacAug2020Inext[[4]]$Estimator[1]
pacAug2020Inext[[4]]$Estimator[1] * 0.95

# Pacific Jun 2021
pacJun2021Inext[[4]]$Estimator[1]
pacJun2021Inext[[4]]$Estimator[1] * 0.95

# Try to get to 39.349
estimateD(pacJun2021Inext[[3]], q = 0, datatype = "incidence_freq", level = 14)$qD

# Pacific Sept 2021
pacSept2021Inext[[4]]$Estimator[1]
pacSept2021Inext[[4]]$Estimator[1] * 0.95


###########################################################################################################################

# Calculate statistics for each bay regarding the water volume (to add to tables with rarefaction stats )
# This is just a very quick way. Need to change each dataframe to get new results for each region
volStats = nl %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(waterVolAnalyzed = (waterVolume * PercZooIdentified * PercSampleCleaned)/4) %>%
  group_by(facetFactor) %>%
  mutate(avWaterVolume = round(mean(waterVolume), 2),
         sdWaterVolume = round(sd(waterVolume),2),
         avAnalyzed = round(mean(PercZooIdentified * PercSampleCleaned)*100,2),
         sdAnalyzed = round(sd(PercZooIdentified * PercSampleCleaned)*100,2)) %>%
  select(facetFactor, avWaterVolume, sdWaterVolume, avAnalyzed, sdAnalyzed) %>%
  unique()


###########################################################################################################################
### Practice with the vegan package
# The vegan package also creates rarefaction curves but it is only for richness. And does not do extrapolation although asymptotic estimators can be calculated

# I think I finally get it
# "exact" is "sample-based rarefaction". Some say that sample-based is not true rarefaction, it is averaged species accumulation curves
# They also state that "rarefaction" is "individual-based rarefaction". This is the distinction the vegan package makes and it's why it's so confusing
# Others say this distinction is dumb and both count as "rarefaction". I am going to call them both "rarefaction"

# use the specaccum function for both individual-based and sample-based rarefaction
# The "exact" method is for sample-based rarefaction. It is also called the "Mao Tau" estimator 
# see https://academic.oup.com/jpe/article/5/1/3/1296712 and vegan help for specaccum

# Make a plot for the Argyle data just to test it

# Think about this some more: I think rounding (especially if close to zero) causes slightly different plots
# But I will be using sample-based rarefaction where it's just presence-absence. So... it's not a huge issue?

# Make plots of both sample-based and individual-based
plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method= "exact")) # sample-based

# Plot individual-based rarefaction over top. Data need to be rounded. (individual-based should be higher)
lines(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method = "rarefaction"), lty = 4)

# I think it would make sense if individual-based rarefaction was instead plotted with xvar = "individuals" as default
# It is odd to me that is not the default (it scales the x-axis to "Sites" which is confusing)
plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]), method = "rarefaction"), xvar = "individuals")

x = as.matrix(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi) (civ-vi)"): ncol(argyle)])

plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi) (civ-vi)"): ncol(argyle)]), method= "random", weights = argyle$waterVolume))


sp1 = specaccum(x)
sp2 = specaccum(x, "random", weights = argyle$waterVolume)
plot(sp2)

mod1 = fitspecaccum(x, "lomolino")

mod1 = fitspecaccum(sp1, "lomolino")

plot(sp1)
plot(mod1, add = T, col = 2, lwd = 2)


mods = fitspecaccum(sp2, "arrh")
plot(mods, col = "hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty = 1, cex = 0.3, add = T)

sapply(mods$models, AIC)



###########################################################################################################################
# FOR PRESENTATIONS
# For CSRF meeting I only want Newfoundland (poorly-sampled, curve doesn't level off) and St. Peters data (well-sampled, curve levels off)

inextCSRF = function(bayData, colourScheme, plotLetter){
  
  # First, just extract only the taxa info:
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
  bay.inext = iNEXT(baySumsList, q = c(0), datatype = "incidence_freq")
  # Plot the graph of diversity vs sampling units
  
  ggiNEXT(bay.inext, color.var = "Order.q")+
    scale_colour_manual(values=colourScheme) +
    scale_fill_manual(values=colourScheme)+
    xlab("Number of zooplankton tows")+
    ylab("Taxa richness")+
    ggtitle(plotLetter)+
    theme_bw(base_size = 18)+ # cool trick so I don't have to adjust the size of everything manually
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin=unit(c(0.1, 1, 0.6, 0.5),"cm"), # add spacing around plots: top, right, bottom, left
      plot.title = element_text(size = 15),
      plot.title.position = "plot")
  
  
  # ggiNEXT(bay.inext, facet.var = "Order.q", color.var = "Order.q")+
  #   theme_bw()
  
  # For species diversity vs coverage
  # ggiNEXT(bay.inext, facet.var = "Order.q", color.var = "Order.q", type = 3)
  
  #return(bay.inext)
  
}
 
# Pass in the dataframe of species counts, colour, and label
stPetersCSRF = inextCSRF(stPeters, "MediumBlue", "(B) St. Peters (Gulf)")
nlCSRF = inextCSRF(seArm2020, "red", "(A) Southeast Arm (Newfoundland)")

#  Plot them both together
plot_grid(nlCSRF, stPetersCSRF, ncol = 1)


###########################################################################################################################
# Play around with HT vs LT data to see if either is adequately sampled

# Test breaking up HT/LT data

argHT = argyle %>%
  subset(tidePhase == "High")

argLT = argyle %>%
  subset(tidePhase == "Low")


argHTtaxa = argHT[,which(colnames(argHT)== "Acartia spp. (civ-vi)"): ncol(argHT)]
argLTtaxa = argLT[,which(colnames(argLT)== "Acartia spp. (civ-vi)"): ncol(argLT)]

argHTtaxa[argHTtaxa>0] = 1
argLTtaxa[argLTtaxa>0] = 1

argHTsums = as.vector(colSums(argHTtaxa))
argLTsums = as.vector(colSums(argLTtaxa))

argTides = list("High Tide" = append(argHTsums, nrow(argHTtaxa), after = 0),
                   "Low Tide" = append(argLTsums, nrow(argLTtaxa), after = 0))

hi = iNEXT(argTides, q = c(0,1,2), datatype = "incidence_freq")

ggiNEXT(hi, facet.var = "Order.q", type = 1)



###########################################################################################################################
### Using the iNEXT4steps methods i.e., Chao et al. (2020)
# Recall that iNEXT.4steps is not in CRAN yet!!

argTaxa = argyle[,which(colnames(argyle)== "Acartia spp. (civ-vi)"): ncol(argyle)]

# Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
argTaxa[argTaxa>0] = 1

# I feel like this could be an incidence_raw matrix but TRULY I have NO IDEA how the want the data to be formatted
# It never works!!! Instead, convert to incidence_freq lol
# Need to get incidence freqncies by summing the columns
argSums = as.vector(colSums(argTaxa))

# It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
argSumsList = list(append(argSums, nrow(argTaxa), after = 0))


# I need to play around with this, but I think my data needs to be in data frame format
arg4StepsPrep = as.data.frame(argSumsList)

# Computes everything
arg4Steps = iNEXT4steps(arg4StepsPrep, datatype = "incidence_freq", diversity = "TD")

arg4Steps$summary # gives summary data
arg4Steps$figure # gives all the figures together

# Note: There are ways to get the figures separate. But that is for another day!!

cocagneInext[[1]]

estimateD(cocagneInext[[3]], q = 0, datatype = "incidence_freq")


estimateD(whiteheadInext[[3]], q = 0, datatype = "incidence_freq", level = 1000)

