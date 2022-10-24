#### Rarefaction Curves

# Just experimenting with creating rarefaction curves and all the different possible outputs

# This has all the plankton data with counts for each file
source("Figures/nmdsSymbols.R")

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
  filter(facilityName == "Sober Island Oyster")

roundedDf = round(argyleTest[,12:ncol(argyleTest)])


S <- specnumber(roundedDf) # observed number of species
(raremax <- min(rowSums(roundedDf)))
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




plot(specaccum(roundedDf, "rarefaction"), xvar = "individuals")
