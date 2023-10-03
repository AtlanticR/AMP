################################################################################
## Make summarizes of the particle types within each sample

# For the samples used in our analysis (40 total) we want a breakdown of all the 
# particle types (e.g., Zooplankton, Debris, Fragments, etc.)

# There were two different ways taxonomists analyzed the data, so as a result, the
# file structures are a bit different. I created separate scripts to read in the different
# files.

# 1. Cleaned List --> zooplankton were separated out, and ID'd at a later date
# This was created in countsCleanedList.R

# 2. Zooplankton Data --> zooplankton were ID'd at the same time other particles were identified
# This was created in countsForLaval.R
# NOTE: Some zooplankton may be removed from analysis (e.g., Invertebrate eggs)
# I am not sure if these should be included in the list
# Make sure to use the correct file for specifying if these should be included or not
# i.e., this is the taxaFix file


################################################################################

# Read this in just to make sure I have the right R packages
source("TechReport/DataProcessing/rPackages.R")

# Also need to have QAcodeMatches.R to get waterVolSamples data

# Remove scientific notation
options(scipen=999)

################################################################################
## Make some adjustments to the data files

# These are the datasets not from the "clean list" folder
particleData = rbind(gulfMerge, nlMerge, pacMerge) %>% 
  left_join(qaID, by = c("flowcamCode" = "FlowCamID")) %>%
  ungroup() %>%
  select(sampleCode, flowcamCode, class, newName, count, regionYear, selectForAnalysis) %>%
  filter(selectForAnalysis == "Yes") %>%
  # Fix typos, etc.
  mutate(class = if_else(class == "0-250um", "0-250um Length", class),
         class = if_else(class == "Clumped Zooplanlton/Debris", "Clumped Zooplankton/Debris", class),
         class = if_else(class == "Clumped_zooplankton", "Clumped Zooplankton", class),
         class = if_else(class == "Clumped_Zooplankton", "Clumped Zooplankton", class),
         class = if_else(class == "Fragments_of_zooplankton", "Fragments of Zooplankton", class),
         class = if_else(newName != "Remove", "Zooplankton", class),
         class = if_else(is.na(class), "Zooplankton", class)) %>%
  filter(count != 0) %>%
  # Get total counts for each class within each sample
  group_by(sampleCode, flowcamCode, class, regionYear) %>%
  summarize(total_class_count = sum(count)) %>%
  # Get total count of all particle types for entire sample
  group_by(sampleCode) %>%
  mutate(totalSample = sum(total_class_count)) %>%
  
  # But note: the classes labelled 1-10 contain particles that were not actually counted
  # Make these NAs. Add this as an extra column 
  # (I could have deleted them, but it is easier to just include them for now!)
  mutate(classCountNoNum = ifelse(class %in% as.character(0:10), NA, total_class_count))%>%
  # Now recalculate the total counts without these NA values
  group_by(sampleCode) %>%
  mutate(totalSampleNoNum = sum(classCountNoNum, na.rm = T))

grouped_data_zooList = particleData %>%
  split(.$regionYear)

# Then write it out
# write_xlsx(grouped_data_zooList, "zooList.xlsx")


## Clean List Data
# This data comes from countsAllParticles.R. It is from the Clean List which includes bubbles, debris, etc.
# Only the Gulf and NL 2021 data from the analyzed datasets had these types of lists

# Make some corrections to the data first
particleDataCleanList = rbind(gulfCleanList, nl21CleanList) %>% 
  left_join(qaID, by = c("flowcamCode" = "FlowCamID")) %>%
  #mutate(regionYear =paste(regionYear, "clean list")) %>%
  ungroup() %>%
  select(sampleCode, flowcamCode, class, newName, count, regionYear, selectForAnalysis) %>%
  filter(selectForAnalysis == "Yes") %>%
  # Fix typos, etc.
  mutate(class = if_else(class == "0-250um", "0-250um Length", class),
         class = if_else(class == "Clumped Zooplanlton/Debris", "Clumped Zooplankton/Debris", class),
         class = if_else(class == "Clumped_zooplankton", "Clumped Zooplankton", class),
         class = if_else(class == "Clumped_Zooplankton", "Clumped Zooplankton", class),
         class = if_else(class == "Fragments_of_zooplankton", "Fragments of Zooplankton", class),
         class = if_else(newName != "Remove", "Zooplankton", class),
         class = if_else(is.na(class), "Zooplankton", class)) %>%
  filter(count != 0) %>%
  # Get total counts for each class within each sample
  group_by(sampleCode, flowcamCode, class, regionYear) %>%
  summarize(total_class_count = sum(count)) %>%
  # Get total count of all particle types for entire sample
  group_by(sampleCode) %>%
  mutate(totalSample = sum(total_class_count)) %>%
  # But note: the classes labelled 1-10 contain particles that were not actually counted
  # Make these NAs. Add this as an extra column 
  # (I could have deleted them, but it is easier to just include them for now!)
  mutate(classCountNoNum = ifelse(class %in% as.character(0:10), NA, total_class_count))%>%
  # Now recalculate the total counts without these NA values
  group_by(sampleCode) %>%
  mutate(totalSampleNoNum = sum(classCountNoNum, na.rm = T)) 

# Write the data to an excel file where every regionYear is a different sheet
# First split the data based on regionYear 
grouped_data_cleanList = particleDataCleanList %>%
  split(.$regionYear)

# Then write it out
# write_xlsx(grouped_data_cleanList, "cleanList.xlsx")

################################################################################
### Make some comparisons

## Read in 

# This contains Julie's spreadsheets that were put together
julieData = read_excel("../AMPDataFiles/QuantitativeAssessment/GoodCopyDataFiles/sampleSummaries.xlsx", sheet = "originalCopy") %>%
  # Replace NAs (or "n/a" written in) with zeroes for the 5mm counts
  mutate(fiveMmCount = as.numeric(ifelse(is.na(fiveMmCount) | fiveMmCount == "n/a", 0, fiveMmCount)),
         chaetognathaNotes = as.numeric(ifelse(is.na(chaetognathaNotes) | str_detect(chaetognathaNotes, "^Taxa was"), 0, chaetognathaNotes)),
         propPlankton = zooCountPostCleaning / cleanedParticleCount) %>%
  right_join(waterVolSamples, by = c("FlowCam Sample Name" = "FlowCamID")) %>% # make sure to right join or I get all the extra data from samples not being used
  # Only want info for the 40 analyzed samples
  filter(usedForAnalysis == "Yes") %>%
  select(c("FlowCam Sample Name", sampleParticleCount, cleanedParticleCount, zooCountPostCleaning))

## Comparisons of the CLEAN LIST (i.e., Gulf 2020 and NL 2021 data)

# Now compare the particle counts to Julie's 

# Clean List
compareDatClean = particleDataCleanList %>%
  filter(class == "Zooplankton") %>%
  rename(myZooCount = total_class_count,
         myCleanedCount = totalSampleNoNum) %>%
  select(-c(totalSample, classCountNoNum)) %>%
  left_join(julieData, by = c(flowcamCode = "FlowCam Sample Name")) %>%
  mutate(zooDiff = zooCountPostCleaning - myZooCount,
         cleanDiff = cleanedParticleCount - myCleanedCount)

# Other list (not cleaned aka "zoo" list)
compareDatZoo = particleData %>%
  filter(class == "Zooplankton") %>%
  rename(myZooCount = total_class_count,
         myCleanedCount = totalSampleNoNum) %>%
  select(-c(totalSample, classCountNoNum)) %>%
  left_join(julieData, by = c(flowcamCode = "FlowCam Sample Name")) %>%
  mutate(zooDiff = zooCountPostCleaning - myZooCount,
         cleanDiff = cleanedParticleCount - myCleanedCount)

compareDatPrint = compareDatZoo %>%
  filter(regionYear == "NL 2020" | regionYear == "Pac 21") %>%
  bind_rows(compareDatClean)

write_xlsx(compareDatPrint, "compareDatPrint.xlsx")

# We know that some bubbles in the NL 2020 data were not included in the particle counts
# I THINK (BUT COULD BE WRONG) that the difference in counts in the files vs cleaned particle counts in Julie's spreadsheets should be the # of bubbles

# Extract just the NL 2020 data and make any difference in particle counts equal to the number of bubbles
nlBubbles = compareDatZoo %>%
  filter(regionYear == "NL 2020") %>%
  mutate(class = "Bubbles or Other(?)",
         # Then rename the columns for consistency with the original data frame
         total_class_count = cleanDiff,
         totalSample = sampleParticleCount,
         classCountNoNum = cleanDiff,
         totalSampleNoNum = sampleParticleCount) %>%
  select(sampleCode, flowcamCode, class, regionYear, total_class_count, totalSample, classCountNoNum, totalSampleNoNum)

# Add back in bubbles to the particleDat dataframe as a new class
dataWithBubbles = rbind(nlBubbles, particleData) %>%
  # Remove data from classes 1-10
  filter(!class %in% as.character(1:10)) %>%
  select(-c(totalSample, classCountNoNum)) 


# Now just create a new df with total particles counts, now that the bubbles have been added
toMerge = dataWithBubbles %>%
  select(flowcamCode, sampleCode, totalSampleNoNum) %>%
  rename(sampleCode2 = sampleCode, totalSample = totalSampleNoNum) %>%
  distinct()


dataWithBubblesTest = dataWithBubbles%>%
  ungroup() %>%
  # Then within each regionYear, need all possible particle types present to then have a value 
  # e.g., if there was Debris in one sample, but not the other, add "bubbles" with count of 0 as a row
  group_by(regionYear) %>%
  complete(flowcamCode, class, fill = list(total_class_count = 0)) %>%
  left_join(toMerge) %>%
  select(-c(sampleCode, totalSampleNoNum)) %>%
  rename(total_class_count_round2 = total_class_count,
         total_sample_round2 = totalSample,
         sampleCode = sampleCode2) %>%
  # Ok so then the original total particle counts for NL 2020 data still show up and should be switched
  # There are two rows of data. Remove the one with the original value (the lower value)
  # This means "group_by" all but the specified column
  group_by(across(-total_sample_round2)) %>%
  filter(total_sample_round2 == max(total_sample_round2)) %>%
  ungroup()
  #mutate(propParticle = total_class_count / totalSample)



sampleBreakdownBothTypes = dataWithBubblesTest %>%
  full_join(particleDataCleanList, by = c("regionYear", "class", "flowcamCode", "sampleCode")) %>%
  # Move the round2 stuff to the end of the dataframe 
  select(-total_class_count_round2, -total_sample_round2, everything(),
         total_class_count_round2, total_sample_round2) %>%
  select(-c(classCountNoNum, totalSampleNoNum)) %>%
  # There will be NAs introduced after merging. Just make these zero
  mutate(total_class_count = ifelse(regionYear %in% c("NL 2021", "Gulf 2020") & is.na(total_class_count), 0, total_class_count)) %>%
  mutate(total_class_count_round2 = ifelse(regionYear %in% c("NL 2021", "Gulf 2020") & is.na(total_class_count_round2), 0, total_class_count_round2)) %>%
  group_by(sampleCode) %>%
  # But make sure the totals don't become zero. These have to become the true totals (i.e., putting it as the max val from each sampleCode)
  mutate(total_sample_round2 = ifelse(regionYear %in% c("NL 2021", "Gulf 2020"), 
                                      ifelse(is.na(total_sample_round2), max(total_sample_round2, na.rm = T), total_sample_round2), total_sample_round2)) %>%
  mutate(totalSample = ifelse(regionYear %in% c("NL 2021", "Gulf 2020"), 
                                      ifelse(is.na(totalSample), max(totalSample, na.rm = T), totalSample), totalSample)) %>%
  ungroup() %>%
  mutate(propParticle = ifelse(regionYear %in% c("NL 2021", "Gulf 2020"), total_class_count/totalSample*100, total_class_count_round2/total_sample_round2*100))
  
write_xlsx(sampleBreakdownBothTypes, "sampleBreakdownBothTypes.xlsx")



summaryNL2021Gulf2020 = sampleBreakdownBothTypes %>%
  filter(regionYear == "NL 2021" | regionYear == "Gulf 2020") %>%
  group_by(regionYear, class) %>%
  summarize(
    min_value_prop = min(propParticle, na.rm = T),
    max_value_prop = max(propParticle, na.rm = T),
    sd_value_prop = sd(propParticle, na.rm = T),
    avg_value_prop = mean(propParticle, na.rm = T),
    
    
    min_value = min(total_class_count, na.rm = T),
    max_value = max(total_class_count, na.rm = T),
    sd_value = sd(total_class_count, na.rm = T),
    avg_value = mean(total_class_count, na.rm = T)
  )

summaryNL2020Pac2021 = sampleBreakdownBothTypes %>%
  filter(regionYear == "NL 2020" | regionYear == "Pac 21") %>%
  group_by(regionYear, class) %>%
  summarize(
    min_value_prop = min(propParticle, na.rm = T),
    max_value_prop = max(propParticle, na.rm = T),
    sd_value_prop = sd(propParticle, na.rm = T),
    avg_value_prop = mean(propParticle, na.rm = T),
    
    
    min_value = min(total_class_count_round2, na.rm = T),
    max_value = max(total_class_count_round2, na.rm = T),
    sd_value = sd(total_class_count_round2, na.rm = T),
    avg_value = mean(total_class_count_round2, na.rm = T)
  )

summaryRegions = summaryNL2021Gulf2020 %>%
  bind_rows(summaryNL2020Pac2021)


write_xlsx(summaryRegions, "summaryRegions.xlsx")


