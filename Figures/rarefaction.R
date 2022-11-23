###########################################################################################################################
### RAREFACTION CURVES

# Create rarefaction curves for each bay using the iNEXT R package
# Also use the iNEXT 4 steps package for additional information about sample completeness
# Undecided: should this also be for HT/LT comparisons, etc

###########################################################################################################################
# SETUP

# Read in data with counts per bay
source("bayBreakdown.R")

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

# First, just extract only the taxa info:
# Remember: extracting data is df[rows, cols]. If left blank, it includes all the data
argyleTaxa = argyle[,which(colnames(argyle)== "Acartia spp."): ncol(argyle)]

# Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
argyleTaxa[argyleTaxa>0] = 1

# I feel like this could be an incidence_raw matrix but TRULY I have NO IDEA how the want the data to be formatted
# It never works!!! Instead, convert to incidence_freq lol
# Need to get incidence freqncies by summing the columns
argSums = as.vector(colSums(argyleTaxa))

# It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
argSumsList = list(append(argSums, 15, after = 0))

# Create the iNEXT object! Calculate for all Hill numbers (q = 1, 2, and 3)
argyle.inext = iNEXT(argSumsList, q = c(0,1,2), datatype = "incidence_freq")
# Plot the graph of diversity vs sampling units

ggiNEXT(argyle.inext, facet.var = "Order.q")

ggiNEXT(argyle.inext, facet.var = "Order.q", color.var = "Order.q")+
  theme_bw()

# For species diversity vs coverage
ggiNEXT(argyle.inext, facet.var = "Order.q", color.var = "Order.q", type = 3)

###########################################################################################################################
######## Using the iNEXT4steps methods


# I need to play around with this, but I think my data needs to be in data frame format
arg4StepsPrep = as.data.frame(argSumsList)

# Computes everything
arg4Steps = iNEXT4steps(arg4StepsPrep, datatype = "incidence_freq", diversity = "TD")

arg4Steps$summary # gives summary data
arg4Steps$figure # gives all the figures together

# Note: There are ways to get the figures separate. But that is for another day!!
