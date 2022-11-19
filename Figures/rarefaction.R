#### Rarefaction Curves

# Just experimenting with creating rarefaction curves and all the different possible outputs

# This has all the plankton data with counts for each file
source("Figures/nmdsSymbols.R")
source("Figures/permanova.R")


# LOOK UP THIS FOR TOMORROW: https://www.rdocumentation.org/packages/rareNMtests/versions/1.2/topics/rarefaction.individual






###########################################################################################################################v
# Practice with BCI data

# I think I finally get it
# "exact" is "sample-based rarefaction". Some say that sample-based is not true rarefaction, it is averaged SACs
# others say this distinction is dumb.
# "rarefaction" is "individual-based rarefaction"

#  The "exact" method finds the expected SAC using sample-based rarefaction methods
# See "Sample-based interpolation (rarefaction)" i.e., "Mao Tau" estimator  https://academic.oup.com/jpe/article/5/1/3/1296712

data(BCI)


# Individual-based data should always be above the sample-based data
plot(specaccum(BCI, method= "exact")) # sample-based
lines(specaccum(BCI, method = "rarefaction"), lty = 4) # individual-based, scaled to # of sites on the x-axis

# I think it would make sense if individual-based rarefaction was instead plotted with xvar = "individuals" as default
plot(specaccum(BCI, method = "rarefaction"), xvar = "individuals")



# Now do some tests with iNEXT()

### individual-based
data(spider)

# How to read the data
str(spider)
# e.g., for Girdled: there are 46 individuals (abundance) of spider species 1, 22 of species 2, 17 of species 3...
# for a total of 168 individuals
sum(spider$Girdled) # use ?spider for an explanation of dataset


iSpider = iNEXT(spider, q = 0, datatype = "abundance")
ggiNEXT(iSpider)

####
# Incidence data

data(ant)
str(ant)

out.inc = iNEXT(ant, q = c(0,1,2), datatype = "incidence_freq")

ggiNEXT(out.inc, facet.var = "Order.q")


### TESTING WITH MY DATA

testIn = marMerge %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))

argyleTest = testIn %>%
  filter(facilityName == "Argyle")

roundedDf = round(argyleTest[,12:ncol(argyleTest)])

roundedDf[roundedDf>0] = 1

sumArg = as.vector(colSums(roundedDf))

sumArg2 = list(append(sumArg, 15, after = 0))



argyle.inc = iNEXT(sumArg2, q = c(0,1,2), datatype = "incidence_freq")
ggiNEXT(argyle.inc, facet.var = "Order.q", color.var = "Order.q")

ggiNEXT(argyle.inc, type = 1, facet.var = "Order.q", color.var = "Order.q")



i.next.out = ChaoRichness(sumArg2)
z = estimateD(sumArg2, datatype = "incidence_freq", level = 26)





plot(specaccum(roundedDf, method = "exact"))
plot(specaccum(roundedDf, method = "rarefaction"), xvar = "individuals")


#######
# Testing inext package

data(spider)
out = iNEXT(spider, q=c(0, 1, 2), datatype="abundance", endpoint=500)
# Sample-size-based R/E curves, separating plots by "Assemblage"
ggiNEXT(out, type=1, facet.var="Assemblage")

ggiNEXT(out, type=1, facet.var="Order.q")


data(bird)
str(bird) # 41 obs. of 2 variables
x = iNEXT(bird, q=c(0,1,2), datatype="abundance")
ggiNEXT(x, type = 1, facet.var = "Assemblage")

## Just Argyle
argyleSpecies = argyleTest[,12:ncol(argyleTest)]
tArgyle = as.data.frame(t(argyleSpecies))
tArgyleSum = as.data.frame(sort(round(rowSums(tArgyle))))

test = iNEXT(tArgyleSum, q = c(0), datatype = "abundance", knots = 2000)
ggiNEXT(test, type = 3, facet.var = "Assemblage")
test

test2 = iNEXT(tArgyleSum, q = c(0), datatype = "abundance")
ggiNEXT(test2, type = 3, facet.var = "Assemblage")

test3 = iNEXT(tArgyleSum, q = c(0,1,2), datatype = "abundance", knots = 2000)
ggiNEXT(test3, type = 1, facet.var = "Assemblage")

test4 = iNEXT(tArgyleSum, q = 0, datatype = "abundance", size = 15)
ggiNEXT(test4, type = 2, facet.var = "Assemblage")

## Argyle as an incidence frequency
# Argyle is made up of 15 samples

argInFreq = as.incfreq(rbind(15, tArgyleSum))

z = c(15, 0, 0, 0, 3, 0, 0, 0, 1, 1, 1, 5, 7, 8, 9, 115, 160, 200, 2677, 303, 298, 303, 426, 708, 3000, 14426)

argFreqInext = iNEXT(z, q = 0, datatype = "incidence_freq")


## SOBER ISLAND

soberTest = testIn %>%
  filter(facilityName == "Sober Island Oyster")

roundedDfS = round(soberTest[,12:ncol(soberTest)])


p <- 1/1:sample(1:50, 1)
p <- p/sum(p)
dat <- as.data.frame(rmultinom(9, 200, p))




## All Maritimes
mar = testIn[,12:ncol(argyleTest)]
tMar = as.data.frame(t(mar))
tMarSum = as.data.frame(sort(round(rowSums(tMar))))

test = iNEXT(tMarSum, q = c(0), datatype = "abundance")
test

ggiNEXT(test, type = 1, facet.var = "Assemblage")+
  # this function sets x-axis scale without clipping data outside of range
  coord_cartesian(xlim = c(0, 10000))





## Test Argyle within Maritimes
argMar = cbind(tMarSum, tArgyleSum)


test = iNEXT(argMar, q = c(0), datatype = "abundance")
ggiNEXT(test, type = 1, facet.var = "Assemblage")









