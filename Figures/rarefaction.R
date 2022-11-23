#### Rarefaction Curves

# Just experimenting with creating rarefaction curves and all the different possible outputs

# This has all the plankton data with counts for each file
source("Figures/nmdsSymbols.R")
source("Figures/permanova.R")


# LOOK UP THIS FOR TOMORROW: https://www.rdocumentation.org/packages/rareNMtests/versions/1.2/topics/rarefaction.individual

# Install necessary packages
# iNEXT.4steps is not in CRAN yet!!
install_github('AnneChao/iNEXT.4steps')
library("iNEXT.4steps")



###########################################################################################################################v
# Practice with BCI data in the vegan package

# I think I finally get it
# "exact" is "sample-based rarefaction". Some say that sample-based is not true rarefaction, it is averaged species accumulation curves
# They also state that "rarefaction" is "individual-based rarefaction". This is the distinction the vegan package makes and it's why it's so confusing
# Others say this distinction is dumb and both count as "rarefaction". I am going to call them both "rarefaction"

# use the specaccum function for both individual-based and sample-based rarefaction
# The "exact" method is for sample-based rarefaction. It is also called the "Mao Tau" estimator 
# see https://academic.oup.com/jpe/article/5/1/3/1296712 and vegan help for specaccum

# Remember that individual-based rarefaction curves should always be above the sample-based data

# Load in sample data
data(BCI)

# Make plots of both sample-based and individual-based
plot(specaccum(BCI, method= "exact")) # sample-based
lines(specaccum(BCI, method = "rarefaction"), lty = 4) # individual-based, but scaled to # of sites on the x-axis

# I think it would make sense if individual-based rarefaction was instead plotted with xvar = "individuals" as default
# It is odd to me that is not the default (it scales the x-axis to "Sites" which is confusing)
plot(specaccum(BCI, method = "rarefaction"), xvar = "individuals")

###########################################################################################################################
######## Instead I want to use the iNEXT package for this 


# Prep my data. For now, just using data from Argyle
# For sample-based rarefaction (what I want to do!!!) data must be converted to presence/absence

# I am prepping my data as "incidence freqencies" which normally I would only do if I have >1 assemblage
# This means that the data frame will be converted to presence/absence
# I then sum all the presence/absence values for each species
# I will then sum incidences across ALL TOWS. e.g., if Acartia was present in 14 of the 15 tows, its incidence frequency is 14

# Rearrange data to be in correct format (species as columns, counts as cells, tows as rows)
# Search only for Argyle data 
argyle = marMerge %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  filter(facilityName == "Argyle")

# Extract only the taxa information
argyleTaxa = argyle[,12:ncol(argyle)]

# Convert it to a presence/absence matrix (data need to be incidence data for sample-based rarefaction)
argyleTaxa[argyleTaxa>0] = 1

# I'm going to do the incidence frequency approach since I may be showing >1 assemblage per graph (TBD)
# Need to get incidence freqncies by summing the columns
argSums = as.vector(colSums(argyleTaxa))

# It then needs to be converted to a list. The first value must also be the # of sampling units (i.e., number of nets)
argSumsList = list(append(argSums, 15, after = 0))

# Create the iNEXT object! Calculate for all Hill numbers (q = 1, 2, and 3)
argyle.inext = iNEXT(argSumsList, q = c(0,1,2), datatype = "incidence_freq")
# Plot the graph of diversity vs sampling units
ggiNEXT(argyle.inext, facet.var = "Order.q", color.var = "Order.q")

# For species diversity vs coverage
ggiNEXT(argyle.inext, facet.var = "Order.q", color.var = "Order.q", type = 3)

###########################################################################################################################
######## Using the iNEXT4steps methods


# I need to play around with this, but I think my data needs to be in data frame format
arg4StepsPrep = as.data.frame(argSums)

# Computes everything
arg4Steps = iNEXT4steps(arg4StepsPrep, datatype = "incidence_freq", diversity = "TD")

arg4Steps$summary # gives summary data
arg4Steps$figure # gives all the figures together

# Note: There are ways to get the figures separate. But that is for another day!!
