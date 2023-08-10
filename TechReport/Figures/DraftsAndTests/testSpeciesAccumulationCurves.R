################################################################################

# Here's a study that uses weights with the random method to account for differences in water volume:
# https://onlinelibrary.wiley.com/doi/full/10.1002/edn3.74

# See here for some good examples
# https://royalsocietypublishing.org/doi/10.1098/rspb.2020.0248
# Has extrapolations and asymptotes

# https://www.neonscience.org/resources/learning-hub/tutorials/aquatic-diversity-algae


################################################################################

# Remember to use code from 

# Use code from here which gives true counts per sample, not abundance which has been corrected for sample volumes
source("Figures/DraftsAndTests/uncorrectedCounts.R")

# Get St. Peters data and convert it to wide format
stPWide = gulfMerge %>% 
  filter(facetFactor == "St. Peters") %>%
  pivot_wider(names_from = newName, values_from = count) %>%
  mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0

stPspp = as.matrix(stPeters[,which(colnames(stPeters)== "Acartia spp. (civ-vi)"): ncol(stPeters)])

# Load in libraries
library("Hmisc") # for making a plot with error bars
library("vegan") # for specaccum function and dune dataset

# Use the dune data as an example
data(dune)

# Make example reproducible
set.seed(13)

# Make species accumulation curve with random method
mod.orig = specaccum(dune, "random")

# Second example of species accumulation curve with weights
# Assign the 20 sites random areas ranging from 1-100 square meters
dune$area = sample(1:100, 20) 

# Run model with weights as the areas
mod.w = specaccum(dune, "random", w = area)

# Model gives NA value for first entry. Remove this value
mod.w.richness = mod.w$richness[-1] # richness
mod.w.sites = mod.w$sites[-1] # sites
mod.w.sd = mod.w$sd[-1] # standard deviation
mod.w.effort = mod.w$effort[-1]

# Make plot of weighted model in red. Bars represent 95% confidence intervals
errbar(mod.w.sites, mod.w.richness, mod.w.richness-2*mod.w.sd, mod.w.richness+ 2* mod.w.sd, type = "b", col = "red", errbar.col = "red", ylab = "Richness", xlab = "Sites")
# Add the original (non-weighted) results over top in black
plot(mod.orig, add = T)
legend("bottomright", legend = c("Weighted", "Normal"), col = c("red", "black"), lty = 1)


plot(mod.w.effort, mod.w.richness)




#stPWide$weight = stPWide$PercSampleCleaned * stPWide$PercZooIdentified * stPWide$waterVolume






stPspp = as.matrix(stPWide[,which(colnames(stPWide)== "Acartia spp. (civ-vi)"): ncol(stPWide)])

plot(mod.Weights, na.rm = T)

stP.rich = data.frame(t(data.frame(replicate(999, specaccum(stPspp, "random", w = stPWide$weight)$richness))))
stP.sd = data.frame(t(data.frame(replicate(999, specaccum(stPspp, "random", w = stPWide$weight)$sd))))

x = stPWide$PercSampleCleaned * stPWide$PercZooIdentified * stPWide$waterVolume

hi = specaccum(stPspp, "random", w = x)

hi.test = hi$richness


plot(hi)

stP.meanRich = colMeans(stP.rich)
stP.meanSd = colMeans(stP.sd)

stPdata = data.frame(t(rbind(stP.meanRich, stP.meanSd)))
stPdata$ind = c(1:28)


stP.chao1 = specpool(stPspp)$chao


ggplot()+
  geom_ribbon(data = stPdata, aes(x = ind, ymin = (stP.meanRich-2*stP.meanSd), ymax = (stP.meanRich+2*stP.meanSd)), alpha = 0.6, fill = "lightblue")+
  geom_line(data = stPdata, aes(x = ind, y = stP.meanRich))+
  xlab("Zooplankton tows")+
  ylab("Taxa richness")+
  geom_hline(yintercept = stP.chao1, linetype = "dashed", col = "gray40")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13.5)
  )




install.packages("gtools")
library("gtools")

num_perm <- function( n, r )  # compute the number of permutations
{                             # of n things taken r at aa time
  if( r == 0 )                #  special case if r=0
  {return(1)}
  p <- 1                      # initialize the product
  for (i in 1:r)              # go throgh the r factors
  { p <- p*n
  n <- n-1
  }
  return(p)                   # send back the result
}

num_perm(10, 2)


