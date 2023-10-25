################################################################################
### PERMANOVAS

# Run PERMANOVAs to see if community structure is different between Flowcam (FC)
# and microscopy (QA) samples 


################################################################################
## Read in the relevant data

# Run code that has NMDS ordinations
# This is where most of the data gets prepared 
source("QuantitativeAssessment/figuresAndStats/nmdsQA.R")




permNL2020Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "NL 2020")

permGulf2020Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "Gulf 2020")


x = adonis2(permNL2020Data[,which(colnames(permNL2020Data)== "Acartia spp."):ncol(permNL2020Data)]~type, data = permNL2020Data, method = "bray", permutations = 9999)

x = adonis2(permGulf2020Data[,which(colnames(permGulf2020Data)== "Acartia spp."):ncol(permGulf2020Data)]~type, data = permGulf2020Data, method = "bray", permutations = 9999)

