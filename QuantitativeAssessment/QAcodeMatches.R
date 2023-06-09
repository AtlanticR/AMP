################################################################################
################################################################################
##### QUANTITATIVE ASSESSMENT

# Created by Stephen Finnis
# May 2023

################################################################################
#### GOALS AND BACKGROUND

# For AMP, taxonomists have also ID'd zooplankton by traditional microscopy
# This is so we can make comparisons between FlowCAM and microscopy

# We call call these comparisons the "Quantitative Analysis" (QA)
# Alternatively, samples for microscopy are called the QA samples

# Recall that in the field, a zooplankton tow was obtained in many regions,
# sites, stations, etc.
# Each tow was then split in 4: 
# 1 subsample run through FlowCam
# 1 subsample for quantitative analysis (microscopy)
# 1 subsample as a backup
# 1 subsample as... I forget!

# Note that not every single FlowCAM sample has a corresponding microscopy sample
# But there are for most!!
# In a few cases, several samples were combined (i.e., microscopy sample includes
# the plankton from 3 different samples combined together) 

# This code is meant to read in (just) the IDs from the QA and then match them 
# To the FlowCAM codes (i.e., how the samples are labelled in the files from the 
# FlowCAM outputs). 
# Then, these FlowCAM codes will be matched to the sampleCodes from the metadata
# (which are also sometimes different)

################################################################################
#### NOTES ABOUT THE QA SAMPLES

### Newfoundland

## 2020
# No issues. All 10 Sept 2020 samples have matches

## 2021
# There are 20 QA samples
# Not sure how they decided which samples to choose (they have >20 for the year)

# One QA sample has the wrong time in the formatting of the ID
# Written as 21-7-7-NL-S1-Z17-1506-250. Time is 1056 (not 1506)

### Gulf

## 2020 
# All 26 St Peters samples have matches
# All 3 Malpeque samples have matches

## 2021
# All 6 Cocagne samples have matches

### Maritimes

## 2021 (no data collected in 2020)
# S01: Sober Island, S02: Argyle, S03: Whitehead, S04: Country Harbour

# Some S03/S04 had direct matches (i.e., one QA sample for one FlowCam sample)
# These were easy to decipher from listed times and QA IDs
# For S01 stations, they combined 3 samples different stations
# e.g., combined Z01/Z02/Z03 into a single QA sample, Z04/05/06, Z07/08/09
# There was also a "21-08-MAR-S02-LOW" sample and "21-08-MAR-S02-HIGH"
# I guessed that these contained Z01-03 samples and matched based on those and S02. 
# This meant there are 9 S02 (Argyle) samples from the Maritimes not in the QA. After elimination, they do not have matches because:
# 6 are from September (not August)
# 2 are mid-rising tides (not high or low)
# 1 is Aug 30 (not Aug 31 like the others)

# Lastly, there are 6 S03 (Whitehead) stations from Maritimes without matches. All other S03s had direct matches.
# There are 4 S04 (Country Harbour) stations without matches. All other S04s had direct matches

### Pacific

## 2020
# There are only 2 samples with QA data
# Written as "High Tide 20-073-001" and "Low Tide 20-073-002"
# I am still trying to figure out which samples those refer to. TBD

## 2021
# Only have QA data for June 2021. Only for June 9 and 10 (not 11)
# Be careful when matching these, because tide phase and date were not included in QA IDs
# They were added as separate columns. Need to be included

################################################################################
#### SETUP

# Read in all required R packages
source("DataProcessing/metadataProcessing.R") 

# Read in data of FlowCam Counts
# Note that this is basically the same as ZooplanktonCount.R from previous project
# But I've just made a few minor adjustments to only give COUNTS, not abundances
# Also I don't want to mess up that old script! 
source("QuantitativeAssessment/flowcamCountQA.R")

# Remove scientific notation
options(scipen=999)

# Read in the Excel spreadsheet with the QA IDs
# Read in spreadsheet with adjustments to taxa names

# Files are read relative to my R Project (within "AMP"), which is separate from where the data files are stored (AMPDataFiles)
# ".." means "go back a directory"

# Read in the file I created that matches the QA samples (and the IDs used by the taxonomists) with the FlowCam IDs
# Note the FlowCAM IDs are NOT the same as the sampleCodes in the metadata
# Those will have to be obtained by joining/merging the IDs again with separate files
qaID = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/QuantAssessPairings.xlsx")

# Read in the file that has the adjustments to the QA taxa names (to make sure they're consistent)
qaTaxaChanges = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/quantAssess_taxaChanges.xlsx")


# Note that I have renamed the QA Excel file names from the originals
# File names are self explanatory. I did not change the sheet names

# Gulf, Maritimes and NL samples were done by Huntsman. Their format is slightly different
# I am using the term "countTot" to refer to the total count in the entire sample!
# I will use "abundance" when it is converted to individuals in seawater
qaGulf2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/Gulf_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  # renaming is "new" = "old"
  rename("countTot" = `Abundance in total sample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaMar2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/Mar_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  rename("countTot" = `Abundance in total sample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaNL2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/NL_2021_QA_Zooplankton_AMP.xlsx", sheet = "Quantitative Format (Raw data)") %>%
  rename("countTot" = `Abundance in total sample`,
         "qaSampleID" = `DFO Sample ID`) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
qaNLGulf2020 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/NL_Gulf_2020_QA_Zooplankton_AMP.xlsx", sheet = "ID raw data") %>%
  rename("countTot" = `Abundance in total sample`,
         "qaSampleID" = `Unique sample name`,
         "Taxon" = "Species") %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))


# Pacific samples (done by Biologica) have a slightly different format
qaPac2020 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/PAC_2020_QA_Zooplankton_AMP.xlsx", sheet = "3. Data-Long") %>%
  rename("countTot" = "Count") %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))
  
qaPac2021 = read_xlsx("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/PAC_2021_QA_Zooplankton_AMP.xlsx", sheet = "4. Biologica Data-Long", skip = 5) %>% # ignore first 5 lines with background info
  rename("countTot" = `Total Abundance`) %>%
  filter(countTot != "n/a") %>% # remove this NA where ctenophora fragments were found, but no counts provided
  mutate(countTot = as.numeric(countTot), # now I can make the value a numeric
         # IDs for Pacific samples have to be concatenated with Tide and Date info, or they can't be distinguished
    qaSampleID =  paste(`Client Sample ID`, Tide, `Date Sampled`, sep = "_"), .before = Fraction) %>%
  mutate("taxStage" = ifelse(is.na(Stage), Taxon, paste(Taxon, Stage)))


# Combine all dataframes (except qaPac2020)
allData = bind_rows(qaGulf2021, qaMar2021, qaNL2021, qaNLGulf2020, qaPac2021, qaPac2020) %>%
  select(qaSampleID, countTot, Taxon, taxStage) %>%
  left_join(qaID, by = "qaSampleID")


test = allData %>%
  group_by(taxStage) %>%
  dplyr::summarize(countUnique = sum(countTot))

# write.csv(test, "test.csv")

unique(allData$Taxon)

################################################################################
################################################################################
## Do some basic plotting

# Make stacked bar charts of counts for each sample in each of the datasets
# Switch out the dataframe to view (first entry in geom_bar) to view other datasets
ggplot()+
  geom_bar(qaMar2021, mapping = aes(x = qaSampleID, y = countTot, fill = Taxon), stat = "identity")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
    )


################################################################################
## Make plots of FlowCam counts vs Microscopy counts

# Many papers do a similar analysis (see my Google Doc for examples)
# Some then perform a correction to get this on a 1:1 relationship
# See https://www.eeer.org/upload/eer-2018-266.pdf and https://link.springer.com/article/10.1007/s10750-019-03980-w
# for examples of how to do this
# I am still in testing mode, so not doing a full description of methods

# Get the FlowCam data put together
# NOTE dataForQA comes from flowCountQA.R, my test script for reading in count data for this project
# It's the same as zooplanktonCounts.R, but with a few adjustments for this project
# Need to join with qaID to match up the FlowCam codes from the quantitative assessment to what Julie put together
flowCamData = qaID %>%
  left_join(dataForQA, by = c("FlowCamID" = "flowcamCode")) %>%
  #filter(regionYear != "Maritimes 21") %>%
  #group_by(FlowCamID) %>%
  #mutate(FCentireSampleCount = sum(taxaCountPerSample)) %>%
  #select(FCentireSampleCount, FlowCamID) %>%
  #distinct() %>%
  #left_join(datafor) %>%
  group_by(qaSampleID) %>%
  mutate(FCentireSampleCount = sum(taxaCountPerSample)) %>%
  select(FCentireSampleCount, qaSampleID, waterVolume, monthStart) %>%
  distinct() %>%
  group_by(qaSampleID) %>%
  mutate(totVol = sum(waterVolume)) %>%
  select(-waterVolume) %>%
  distinct()

# Get total microscopy counts for each sample
qaDataPerSample = allData %>%
  group_by(FlowCamID) %>%
  mutate(QAentireSampleCount = sum(countTot)) %>%
  ungroup() %>%
  select(QAentireSampleCount, qaSampleID, site)%>%
  distinct()

# Join the flowcam data with the microscopy data based on the FlowCamID
fcAndMicro = flowCamData %>%
  left_join(qaDataPerSample)

# Plot them both to see what it looks like
plot(fcAndMicro$FCentireSampleCount, fcAndMicro$QAentireSampleCount)

x = lm(fcAndMicro$FCentireSampleCount ~ fcAndMicro$QAentireSampleCount)
summary(x)


microAbund = fcAndMicro$QAentireSampleCount*4 / fcAndMicro$totVol
fcAbund = fcAndMicro$FCentireSampleCount*4/fcAndMicro$totVol

plot(microAbund, fcAbund)

summary(lm(microAbund ~fcAbund))
summary(lm(fcAbund~microAbund))

summary(lm(fcAndMicro$FCentireSampleCount~fcAndMicro$QAentireSampleCount))


# Now try log transforming it
# I think I will need to do this (based on the literature, and also because the data are skewed)
plot(log10(fcAndMicro$FCentireSampleCount), log10(fcAndMicro$QAentireSampleCount))

# Run a model 2 regression (need to do this because both variables are independent)
mod = lmodel2(log10(fcAndMicro$FCentireSampleCount) ~ log10(fcAndMicro$QAentireSampleCount))

# Now "correct" the flowcam data
# To do this, divide by the slope, and subtract the intercept. This turns it into a 1:1 relationship (see links above for more background)
flowcamCorLog10 = (log10(fcAndMicro$FCentireSampleCount) - mod$regression.results$Intercept[1])* (1/mod$regression.results$Slope[1])

# Now store all the data in a dataframe
# Note that I'm log transforming everything

flowcamOrig = fcAndMicro$FCentireSampleCount
micro = fcAndMicro$QAentireSampleCount

flowcamOrigLog10 = log10(fcAndMicro$FCentireSampleCount)
microLog10 = log10(fcAndMicro$QAentireSampleCount)

dat = data.frame(flowcamCorLog10, flowcamOrig, micro, flowcamOrigLog10, microLog10)


datMeltLog = melt(data = dat,
               id.vars = c("microLog10"),
               measure.vars = c("flowcamCorLog10", "flowcamOrigLog10"),
               variable.name = "flowcamType",
               value.name = "value")

# datOrig = data.frame(
#   type = c(rep("Micro", length(micro)), rep("flowcam", length(flowcamOrig))),
#   value = c(micro, flowcamOrig)
# )

datOrig = data.frame(
  micro, flowcamOrig
)


ggplot()+
  geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
  geom_abline(slope = mod$regression.results$Slope[1], intercept = mod$regression.results$Intercept[1], col = "#00BFC4", linetype = "dotdash", linewidth = 0.8)+
  geom_point(data = datMeltLog, aes(x = microLog10, y = value, fill = flowcamType, pch = flowcamType), size = 4, alpha = 0.9)+
  scale_shape_manual(values = c(21, 22), labels = c("Corrected", "Original"))+
  scale_fill_manual(values = c("white", "#00BFC4"), labels = c("Corrected", "Original"))+
  labs(x = expression(log[10]~"[Microscopy abundance (ind."~ (m^-3) ~"]"),
       y = expression(log[10]~"[FlowCam abundance (ind."~ (m^-3) ~"]")) +
  theme_bw()+
  theme(legend.title = element_blank())


# Run model 2 regression for non-transformed data

modOrig = lmodel2(dat$flowcamOrig ~ dat$micro)


ggplot()+
  geom_abline(slope=1, intercept=0, linetype = "dashed", col = "#F8766D", linewidth = 0.8)+
  geom_abline(slope = modOrig$regression.results$Slope[1], intercept = modOrig$regression.results$Intercept[1], col = "lightblue", linetype = "dotdash", linewidth = 0.8)+
  
  geom_point(data = datOrig, aes(x = micro, y = flowcamOrig), pch = 21, size = 4, alpha = 0.9, fill = "lightblue")+

  labs(x = "Microscopy abundance (ind."~ (m^-3),
       y = "FlowCam abundance (ind."~ (m^-3)) +
  theme_bw()+
  theme(legend.title = element_blank())





# also see
# https://www.eeer.org/upload/eer-2018-266.pdf for more bias corrections



################################################################################
# Test comparison between microscopy and flowcam
# Perform a correction to the count data from flowcam


# Create fake microscopy data
microscopy = runif(50, min = 0, max = 10)
# Create fake flowcam data based on the microscopy data
# Make it obviously different 
flowcam = microscopy*2 + rnorm(50, mean = 0, sd = 1) + 5

# Plot them both to see how it looks
plot(microscopy, flowcam)

# Run a model 2 regression 
model = lmodel2(flowcam ~ microscopy, data = data.frame(microscopy, flowcam))

# Get the slope and intercept (note: not sure which of the options I'm supposed to use)
# For now, just use the first model output i.e., [1]
intercept = model$regression.results$Intercept[1]
slope = model$regression.results$Slope[1]

# Now, correct the flowcam data! 
# I want the flowcam data to now be at a 1:1 relationship with the microscopy data
# following y = mx+b, I need to divide by the slope, then subtract the intercept!
correction_factor = 1/slope
cor_flowcam = (flowcam - intercept) * correction_factor

# Combine the data
combData = data.frame(flowcam, microscopy, cor_flowcam)

# Plot the data
ggplot()+
  geom_point(data = test, aes(x = microscopy, y = flowcam))+ # uncorrected
  geom_point(data = test, aes(x = microscopy, y = cor_flowcam), col = "red")+ # corrected
  geom_abline(slope=1, intercept=0)+ 
  xlim(0, 10)+
  ylim(0,30)

################################################################################
# Test interactive plotting to see what the problem samples are 
x=
  ggplot()+
  geom_point_interactive(data = fcAndMicro, aes(x = QAentireSampleCount, y = FCentireSampleCount), data_id = fcAndMicro$qaSampleID, tooltip = fcAndMicro$qaSampleID, onclick = fcAndMicro$qaSampleID, size = 5)
girafe(ggobj = x)



microAbund = fcAndMicro$QAentireSampleCount*4 / fcAndMicro$totVol
fcAbund = fcAndMicro$FCentireSampleCount*4/fcAndMicro$totVol

x=
  ggplot()+
  geom_point_interactive(data = fcAndMicro, aes(x = microAbund, y = fcAbund), data_id = fcAndMicro$qaSampleID, tooltip = fcAndMicro$qaSampleID, onclick = fcAndMicro$qaSampleID, size = 5)
girafe(ggobj = x)













