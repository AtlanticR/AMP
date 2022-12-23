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
plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp."): ncol(argyle)]), method= "exact")) # sample-based

# Plot individual-based rarefaction over top. Data need to be rounded. (individual-based should be higher)
lines(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp."): ncol(argyle)]), method = "rarefaction"), lty = 4)

# I think it would make sense if individual-based rarefaction was instead plotted with xvar = "individuals" as default
# It is odd to me that is not the default (it scales the x-axis to "Sites" which is confusing)
plot(specaccum(round(argyle[,which(colnames(argyle)== "Acartia spp."): ncol(argyle)]), method = "rarefaction"), xvar = "individuals")

###########################################################################################################################
### With the iNEXT package

# For sample-based rarefaction (what I want to do!!!) data must be converted to presence/absence

# I am prepping my data as "incidence freqencies" which normally I would only do if I have >1 assemblage
# This means that the data frame will be converted to presence/absence
# I then sum all the presence/absence values for each species
# I will then sum incidences across ALL TOWS. e.g., if Acartia was present in 14 of the 15 tows, its incidence frequency is 14


inextPrep = function(bayData, colourScheme, plotLetter){

# First, just extract only the taxa info:
# Remember: extracting data is df[rows, cols]. If left blank, it includes all the data
bayTaxa = bayData[,which(colnames(bayData)== "Acartia spp."): ncol(bayData)]

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
bay.gg = ggiNEXT(bay.inext, facet.var = "Order.q", color.var = "Order.q")+
    scale_colour_manual(values=colourScheme) +
    scale_fill_manual(values=colourScheme)+
    xlab("Number of zooplankton tows")+
    ylab("Taxa diversity")+
    ggtitle(plotLetter)+
    theme_bw(base_size = 18)+ # cool trick so I don't have to adjust the size of everything manually
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin=unit(c(0.1, 1, 0.6, 0.5),"cm"), # add spacing around plots: top, right, bottom, left
      plot.title = element_text(size = 15),
      plot.title.position = "plot")

# Extract the asymptotic diversity estimates
asy.df = data.frame(bay.inext$AsyEst) %>%
  # Remove the Assemblage column
  select(-c(Assemblage)) %>%
  # Add a column with undetected species and put the column before the s.e. column
  mutate(Undetected = Estimator-Observed, .before = s.e.) %>%
  # Round numeric values to 2 decimal places
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(bay = bayData$facetFactor[1], .before = Diversity)


return(list(bay.gg, asy.df))

}

# Maritimes
argInext = inextPrep(argyle, marColours[[1]], "(A) Argyle")
countryInext = inextPrep(country, marColours[[2]], "(B) Country Harbour")
soberInext = inextPrep(sober, marColours[[3]], "(C) Sober Island")
whiteheadInext = inextPrep(whitehead, marColours[[4]], "(D) Whitehead")

# Plots are stored in the first list element
plot_grid(argInext[[1]], countryInext[[1]], soberInext[[1]], whiteheadInext[[1]], align = "v", ncol = 1)
# Get the dataframe of asymptotic estimator results
marInextResults = bind_rows(argInext[[2]], countryInext[[2]], soberInext[[2]], whiteheadInext[[2]])


# Gulf
cocagneInext = inextPrep(cocagne, gulfColours[[1]], "(A) Cocagne")
malpequeInext = inextPrep(malpeque, gulfColours[[2]], "(B) Malpeque")
stPetersInext = inextPrep(stPeters, gulfColours[[3]], "(C) St. Peters")

# Add an extra plot of St. Peters so all plots in all regions are the same size
plot_grid(cocagneInext[[1]], malpequeInext[[1]], stPetersInext[[1]], stPetersInext[[1]], align = "v", ncol = 1)
gulfInextResults = bind_rows(cocagneInext[[2]], malpequeInext[[2]], stPetersInext[[2]])


# Pacific
pacAug2020Inext = inextPrep(pacAug2020, pacColours[[1]], "(A) August 2020")
#pacMar2021Inext = inextPrep(pacMar2021, pacColours[[2]], "(B) March 2021") # not enough data
pacJun2021Inext = inextPrep(pacJun2021, pacColours[[3]], "(C) June 2021")
pacSept2021Inext = inextPrep(pacSept2021, pacColours[[4]], "(D) September 2021")

# For now, add one extra plot so all the graphs look the same for each region
plot_grid(pacAug2020Inext[[1]], pacJun2021Inext[[1]], pacSept2021Inext[[1]], pacSept2021Inext[[1]], align = "v", ncol = 1)
pacInextResults = bind_rows(pacAug2020Inext[[2]], pacJun2021Inext[[2]], pacSept2021Inext[[2]])

# Newfoundland
seArm2020Inext = inextPrep(seArm2020, nlColours[[1]], "(A) Southeast Arm")
plot_grid(seArm2020Inext[[1]], seArm2020Inext[[1]], seArm2020Inext[[1]], seArm2020Inext[[1]], align = "v", ncol = 1)
seInextResults = seArm2020Inext[[2]]










###########################################################################################################################
# For CSRF meeting I only want Newfoundland and St. Peters data


inextCSRF = function(bayData, colourScheme, plotLetter){
  
  # First, just extract only the taxa info:
  # Remember: extracting data is df[rows, cols]. If left blank, it includes all the data
  bayTaxa = bayData[,which(colnames(bayData)== "Acartia spp."): ncol(bayData)]
  
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
  
  return(bay.inext)
  
}


stPetersCSRF = inextCSRF(stPeters, "MediumBlue", "(B) St. Peters (Gulf)")
nlCSRF = inextCSRF(seArm2020, "red", "(A) Southeast Arm (Newfoundland)")

plot_grid(nlCSRF, stPetersCSRF, ncol = 1)





# Test breaking up HT/LT data

argHT = argyle %>%
  subset(tidePhase == "High")

argLT = argyle %>%
  subset(tidePhase == "Low")


argHTtaxa = argHT[,which(colnames(argHT)== "Acartia spp."): ncol(argHT)]
argLTtaxa = argLT[,which(colnames(argLT)== "Acartia spp."): ncol(argLT)]

argHTtaxa[argHTtaxa>0] = 1
argLTtaxa[argLTtaxa>0] = 1

argHTsums = as.vector(colSums(argHTtaxa))
argLTsums = as.vector(colSums(argLTtaxa))

argTides = list("High Tide" = append(argHTsums, nrow(argHTtaxa), after = 0),
                   "Low Tide" = append(argLTsums, nrow(argLTtaxa), after = 0))

hi = iNEXT(argTides, q = c(0,1,2), datatype = "incidence_freq")

ggiNEXT(hi, facet.var = "Order.q", type = 1)




# https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781118445112.stat07841



###########################################################################################################################
### Using the iNEXT4steps methods


argTaxa = argyle[,which(colnames(argyle)== "Acartia spp."): ncol(argyle)]

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
