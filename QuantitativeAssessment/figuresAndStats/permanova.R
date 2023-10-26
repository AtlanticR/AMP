################################################################################
### PERMANOVAS

# Run PERMANOVAs to see if community structure is different between Flowcam (FC)
# and microscopy (QA) samples 


################################################################################
## Read in the relevant data

# Run code that has NMDS ordinations
# This is where most of the data gets prepared 
source("QuantitativeAssessment/figuresAndStats/nmdsQA.R")


# Separate out each of the datasets
permGulf2020Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "Gulf 2020")

permPac2021Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "Pac 21")

permNL2020Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "NL 2020")

permNL2021Data = ordPrepWideRelAbund %>% 
  filter(regionYear == "NL 2021")


################################################################################
## Run the tests

# Using Bray-Curtis dissimilarities on the relative abundance data
# Need to specify which data to look at: only include species data (starts at "Acartia spp., ends at the last column)

# GULF 2020
permGulf2020Result = adonis2(permGulf2020Data[,which(colnames(permGulf2020Data)== "Acartia spp."):ncol(permGulf2020Data)]~type, data = permGulf2020Data, method = "bray", permutations = 9999) %>%
  # Multiply R2 by 100 to convert to percentages
  mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
  # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
  mutate(MS = SumOfSqs/Df, .before = R2) %>%
  # Round most columns to 2 decimal places
  mutate(across(c(SumOfSqs, MS, R2, F), round, 3)) %>%
  mutate(dataset = "Gulf 2020", .before = Df)

# PACIFIC 2021
permPac2021Result = adonis2(permPac2021Data[,which(colnames(permPac2021Data)== "Acartia spp."):ncol(permPac2021Data)]~type, data = permPac2021Data, method = "bray", permutations = 9999) %>%
  # Multiply R2 by 100 to convert to percentages
  mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
  # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
  mutate(MS = SumOfSqs/Df, .before = R2) %>%
  # Round most columns to 2 decimal places
  mutate(across(c(SumOfSqs, MS, R2, F), round, 3))%>%
  mutate(dataset = "Pacific 2021", .before = Df)

# NEWFOUNDLAND 2020
permNL2020Result = adonis2(permNL2020Data[,which(colnames(permNL2020Data)== "Acartia spp."):ncol(permNL2020Data)]~type, data = permNL2020Data, method = "bray", permutations = 9999) %>%
  # Multiply R2 by 100 to convert to percentages
  mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
  # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
  mutate(MS = SumOfSqs/Df, .before = R2) %>%
  # Round most columns to 2 decimal places
  mutate(across(c(SumOfSqs, MS, R2, F), round, 3))%>%
  mutate(dataset = "Newfoundland 2020", .before = Df)

# NEWFOUNDLAND 2021
permNL2021Result = adonis2(permNL2021Data[,which(colnames(permNL2021Data)== "Acartia spp."):ncol(permNL2021Data)]~type, data = permNL2021Data, method = "bray", permutations = 9999) %>%
  # Multiply R2 by 100 to convert to percentages
  mutate_at(vars(c(R2)), .funs = funs(.*100)) %>% 
  # Add mean sum of squares (MS) column. Note: this adds a cell in the "total" row. Will need to manually remove this.
  mutate(MS = SumOfSqs/Df, .before = R2) %>%
  # Round most columns to 2 decimal places
  mutate(across(c(SumOfSqs, MS, R2, F), round, 3))%>%
  mutate(dataset = "Newfoundland 2021", .before = Df)

# Put all results together to be exported
allPNresults = bind_rows(permGulf2020Result, permPac2021Result, permNL2020Result, permNL2021Result)

# Write results to csv
# write.csv(allPNresults, "allPNresults.csv")
