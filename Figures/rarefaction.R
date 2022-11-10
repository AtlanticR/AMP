#### Rarefaction Curves

# Just experimenting with creating rarefaction curves and all the different possible outputs

# This has all the plankton data with counts for each file
source("Figures/nmdsSymbols.R")
source("Figures/permanova.R")

data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)



plot(specaccum(BCI, "rarefaction"), xvar = "individuals")
plot(specaccum(BCI, "random"), xvar = "individuals")




### TESTING WITH MY DATA

testIn = marMerge %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))

argyleTest = testIn %>%
  filter(facilityName == "Argyle")

roundedDf = round(argyleTest[,12:ncol(argyleTest)])


S <- specnumber(roundedDf) # observed number of species
raremax <- min(rowSums(roundedDf))
Srare <- rarefy(roundedDf, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(roundedDf, step = 20, sample = raremax, col = "blue", cex = 0.6)

sp2 <- specaccum(roundedDf, method = "rarefaction")
plot(sp2, xvar = "individuals")


plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", 
     main = "Default: Prettier CI")

plot(specaccum(roundedDf, 
               method = "rarefaction"), 
     xvar = "individuals",
     main = "rarefaction, xvar = individuals")


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

test = iNEXT(tArgyleSum, q = c(0), datatype = "abundance")
test

ggiNEXT(test, type = 3, facet.var = "Assemblage")

## SOBER ISLAND

soberTest = testIn %>%
  filter(facilityName == "Sober Island Oyster")

roundedDfS = round(soberTest[,12:ncol(soberTest)])







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









