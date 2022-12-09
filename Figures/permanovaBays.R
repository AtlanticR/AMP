# of unique permutations 

# See here for sample questions
# #https://github.com/vegandevs/vegan/issues/308

source("DataProcessing/bayBreakdown.R")


################################################################################
## Argyle

# Test this without the Mid-Rising data (n=2) for now
argMinusMR = argyle %>%
  subset(tidePhase != "Mid-Rising")



dataTest = argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)]

dataAll = argyle[,which(colnames(argyle)== "Acartia spp."):ncol(argyle)]
allArg = vegdist(sqrt(dataAll))

brayArg = vegdist(sqrt(dataTest))

adonis2(brayArg~tidePhase+myLabel, data = argMinusMR, perm = 9999, by = "margin")

adonis2(brayArg~tidePhase, data = argMinusMR, perm = 9999)
adonis2(brayArg~myLabel, data = argMinusMR, perm = 9999)


adonis2(brayArg~tidePhase+myLabel+tidePhase:myLabel, data = argMinusMR, perm = 9999)

adonis2(brayArg~tidePhase*myLabel, data = argMinusMR, perm = 9999)


adonis2(allArg~tidePhase*myLabel, data = argyle)



adonis2(sqrt(dataTest)~ tidePhase, data = argMinusMR, method = "bray", perm = 9999)
adonis2(sqrt(dataTest)~ tidePhase, data = argMinusMR, method = "bray", perm = 9999, sqrt.dist = F)
adonis2(dataTest~ tidePhase, sqrt.dist = T, data = argMinusMR, method = "bray", perm = 9999)


adonis2(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)]~
          as.factor(argMinusMR$tidePhase), method="bray", sqrt.dist = T, perm = 9999)



adonis2(vegdist(sqrt(dataTest))~ myLabel, data = argMinusMR, perm = 9999)



adonis2(dataTest~ tidePhase, method = "bray", sqrt.dist = T, data = argMinusMR, perm = 9999)


x = vegdist(sqrt(dataTest))

adonis2(x~ tidePhase, data = argMinusMR, perm = 9999)



adonis2(sqrt(dataTest)~ tidePhase, data = argMinusMR, perm = 9999, method = "bray", sqrt.dist = F)
adonis2(dataTest~ tidePhase, data = argMinusMR, perm = 9999, method = "bray", sqrt.dist = T)

adonis2(dataTest~ tidePhase, data = argMinusMR, method = "bray", sqrt.dist = T, perm = 999)






gulfDisp = betadisper(sqrt(vegdist(argMinusMR[,which(colnames(argMinusMR)== "Acartia spp."):ncol(argMinusMR)])), as.factor(argMinusMR$tidePhase), type = "centroid")

pairGulfDisp = permutest(gulfDisp, pairwise = T, permutations = 9999, perm = 9999, set.seed(13))




# Do NMDS ordination but only include species data
ord = metaMDS(dataTest, distance = "bray", autotransform = F)



plot(ord)

# Get NMDS coordinates from plot
ordCoords = as.data.frame(scores(ord, display="sites"))%>%
  mutate(tidePhase = argMinusMR$tidePhase) %>%
  mutate(loc = argMinusMR$myLabel)

# Create the ggplot
ggplot() + 
  geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, col = loc), size = 5)
                                   

dataPac = 






















metadata <- cbind.data.frame(SampleID = seq(from = 1, to = 18, by = 1),
                             Diet = rep(c("Ref", "Soy"), each = 9),
                             Tank = rep(c("T1", "T2", "T3", "T4", "T5", "T6"), each = 3))

# Make feature table
low <- data.frame(replicate(50,sample(0:10,9,rep=TRUE)))

high <- data.frame(replicate(50,sample(0:100,9,rep=TRUE)))

table <- rbind(low, high)

# Define permutation scheme for running nested PERMANOVA
perm <- how(complete = TRUE, 
            within   = Within(type = "none"),
            plots    = with(metadata, Plots(strata = Diet, type = "free")))    

# Nested PERMANOVA
x = adonis2(table ~ Diet, data = metadata, permutations = perm, method="bray")



adonis2(gulfPN[,which(colnames(gulfPN)== "Acartia spp."):ncol(gulfPN)]~
          as.factor(gulfPN$facetFactor), method="bray", sqrt.dist = T, set.seed(13))


pp = marPN[,which(colnames(marPN)== "Acartia spp."):ncol(marPN)]


pp = as.data.frame(pacPN[1:4,])

data = pp[,which(colnames(pp)== "Acartia spp."):ncol(pp)]



pp$hi = c("no", "no", "yes", "yes")


# Are there differences between bays (facetFactor)
adonis2(data~
          as.factor(facility), method="euclidean", sqrt.dist = T, perm = 9999)




## permutation design --- see ?how
ctrl <- how() ## defaults to freely exchangeable

## vector input
v <- 1:10
(obs <- nobs(v))
numPerms(v, control = ctrl)

## integer input
len <- length(v)
(obs <- nobs(len))
numPerms(len, control = ctrl)

## new design, objects are a time series
ctrl <- how(within = Within(type = "series"))
numPerms(v, control = ctrl)
## number of permutations possible drastically reduced...
## ...turn on mirroring
ctrl <- how(within = Within(type = "series", mirror = TRUE))
numPerms(v, control = ctrl)

## Try blocking --- 2 groups of 5
bl <- numPerms(v, control = how(blocks = gl(2,5)))
bl

## should be same as
pl <- numPerms(v, control = how(plots = Plots(strata = gl(2,2))))
pl
stopifnot(all.equal(bl, pl))
