################################################################################
#### Redistribute taxa

## Background

# There are a few taxa within both the FlowCam and Quantitative Assessment lists
# that need to be dealt with

# From the FlowCam list, these include:
# Zooplankton (unid), Calanoida (unid) and Copepoda (unid)
# These will be distributed among the possible taxa within each group

# I will be using the DPAC-S method described more in Cuffney et al. (2007):
# https://www.journals.uchicago.edu/doi/full/10.1899/0887-3593%282007%2926%5B286%3AATEOTC%5D2.0.CO%3B2
# This well help distribute the abundances between these 3 unid groups between the lower-level taxa
# e.g., if Acartia form 50% of Calanoida, then 50% of abundances from Calanoida (unid) in the sample
# will be distributed to them. See the paper above for more of an explanation.

# I also used this approach in the Tech Report under TechReport/DataProcessing/dividePlankton.R
# But the code was a bit messy so I'm fixing it up here

# There are also a few taxa from the Quantitative Assessment that need to be "fixed" in some capacity
# This includes:
# Convert all Decapoda to either "brachyura" or "non-brachyura". Many taxa in the Pacific (QA data) were to higher taxonomic
# levels and that gets too complicated. So switch ALL to that. Including the few from the FlowCam data that were also to higher levels 
# Check about Porcellanidae- but honestly I think just make it non-brachyura.
# Based on advice of taxonomist, "Damaged" taxa in QA dataset, was just classified into the adult taxa (not split into that vs ci-ciii)

################################################################################
### Set up

# Run the script that compiles the data
source("QuantitativeAssessment/dataProcessing/QAcodeMatches.R") 

### Initial data prep

# Make this a new variable for testing
fcQaDf.redist = fcQaDf

# Remove eggs from the dataset. These were counted slightly differently in the Pacific dataset.
# I named these "EGG CHECK" in the QA dataset. Or "Invertebrate (egg, trochophore larvae)
# Also removing Osteichthyes egg which may have been counted as "egg mass" in the Pacific data
fcQaDf.redist = fcQaDf.redist %>%
  filter(newName != "EGG CHECK" & newName != "Invertebrate (egg, trochophore larvae)" & newName != "Osteichthyes egg") %>%
  # Need to reclassify taxonomic level for consistency with  Cyril's dataset
  mutate(newName = ifelse(newName == "Hyperiidea spp. (juvenile)", "Amphipoda- epibenthic", newName))

# Note, there were several other changes that should be made to the overall dataset. However, they are not not in these samples. 
# Including below for reference, but commented out. These are applicable for the FlowCam dataset. QA data might be different.
# Fix these Amphipods and Isopods. Instructions came from taxonomist
  # mutate(newName = ifelse(newName=="Amphipoda" & dataset == "Gulf 2020", "Amphipoda- epibenthic", newName)) %>%
  # mutate(newName = ifelse(newName == "Isopoda" & dataset == "Gulf 2020", "Isopoda- epibenthic",
  #                           ifelse(newName == "Isopoda" & (dataset == "Gulf 2021" | dataset == "Newfoundland 2021"), "Isopoda (larvae)", newName))) 


################################################################################
### Create the function 

# Distribute some of the higher level taxa (i.e., "parent taxa"/"problem_taxa") 
# between lower level taxa (i.e., "child taxa" "target_taxa")
# Function accepts the dataframe with the counts, and specifies the "problem" taxa to be distributed, and the "target" taxa
# they should be distributed amongst
redistribute_abundances = function(df, problem_taxa, target_taxa){
  
  # Filter the data frame for the problem taxa
  df_problem = df %>%
    filter(newName == problem_taxa)
  
  # Calculate the total abundance of the problem taxa for each sample
  df_problem_total = df_problem %>%
    group_by(FlowCamID, type) %>%
    summarize(total_abund_problem = sum(abund))
  
  # Join the total abundance back to the original dataframe
  df = df %>%
    left_join(df_problem_total, by = c("FlowCamID", "type"))
  
  # Calculate the total abundance of the target taxa for each sample
  df_total = df %>%
    filter(newName %in% target_taxa) %>%
    group_by(FlowCamID, type) %>%
    summarize(total_abund_target = sum(abund))
  
  # Join the total abundance back to the original dataframe
  df = df %>%
    left_join(df_total, by = c("FlowCamID", "type"))

  # Calculate the relative abundance of the target taxa within each sample
  for (taxon in target_taxa) {
    df = df %>%
      mutate(!!paste0("prop_", taxon) := if_else(newName == taxon, abund / total_abund_target, 0))
  }
  
  # Calculate the redistributed abundances
  for (taxon in target_taxa) {
    df = df %>%
      mutate(abund = if_else(newName == taxon & !is.na(total_abund_problem),
                             abund + get(paste0("prop_", taxon)) * total_abund_problem, abund))
  }
  
  # Remove the problem taxa from the dataframe
  df = df %>% 
    filter(newName != problem_taxa)
  
  # Drop the unnecessary columns
  df = df %>% 
    select(-c(total_abund_problem, total_abund_target, starts_with("prop_")))

  
}

################################################################################
### Run the function

# Then redistribute the Decapoda that was unspecified
fcQaDf.redist = redistribute_abundances(fcQaDf.redist, "Decapoda- CHECK", c("Decapoda- brachyura (larvae)", "Decapoda- non-brachyura (larvae)"))

# Redistribute Cyclopoida
# From below, and from Tech report, I know there is one sample in this dataset that has no child taxa to distribute among
# I also know that all the child taxa for this regionYear are Oithona. So just change it now
fcQaDf.redist = fcQaDf.redist %>%
  mutate(newName = if_else(FlowCamID == "AMMP_Gulf_StPeters_3B_20200903_250UM" & type == "FC" & newName == "Cyclopoida", "Oithona spp.", newName))

# Then redistribute the rest
fcQaDf.redist = redistribute_abundances(fcQaDf.redist, "Cyclopoida (unid)", c("Cyclopoida ci-ciii", "Corycaeidae", "Oithona spp."))

# Redistribute Calanoida among all possible Calanoida taxa (I had to look all of these up)
fcQaDf.redist = redistribute_abundances(fcQaDf.redist, "Calanoida (unid)", c("Acartia spp.", "Calanoida ci-ciii", "Calanus spp.", "Centropages spp.", "Chiridius spp.", "Eurytemora spp.",
                                                           "Metridia spp.", "Microcalanus spp.", "Paracalanus spp.", "Pseudocalanus spp.", "Pseudodiaptomus spp.", "Temora spp.", "Tortanus spp."))

# Redistribute Copepoda among all possible Copepod taxa
fcQaDf.redist = redistribute_abundances(fcQaDf.redist, "Copepoda (unid)", c("Cyclopoida ci-ciii", "Corycaeidae", "Oithona spp.", "Harpacticoida- epibenthic", "Microsetella spp.", "Monstrillidae", "Acartia spp.", "Calanoida ci-ciii", "Calanus spp.", "Centropages spp.", "Chiridius spp.", "Eurytemora spp.",
                                                          "Metridia spp.", "Microcalanus spp.", "Paracalanus spp.", "Pseudocalanus spp.", "Pseudodiaptomus spp.", "Temora spp.", "Tortanus spp."))

# Get the names of all other taxa except Zooplankton (unid). This is what Zooplankton (unid) abundances will be split between
allButZoo = fcQaDf.redist %>%
  filter(newName != "Zooplankton (unid)") %>%
  ungroup() %>%
  select(newName) %>%
  distinct()

# Redistribute zooplankton (unid) among everything that isn't Zooplankton (unid)
fcQaDf.redist = redistribute_abundances(fcQaDf.redist, "Zooplankton (unid)", allButZoo$newName)

# Make sure that any taxa that have changed are now being added together so they aren't separate lines
fcQaDf.redist = fcQaDf.redist %>%
  group_by(newName, FlowCamID, regionYear, qaSampleID, type) %>%
  summarize(abund = sum(abund))


################################################################################
################################################################################
# Code for testing if there are samples with no child taxa to distribute parent taxa
# e.g., there are Cyclopoida, but no "Corycaeidae", or "Oithona spp."
# I tried making code to test this. It's not perfect. Ideally it should be incorporated
# within the code above. But it would take too much time to do, and wasn't working when I tried
# I am commenting this out, but the code is here for future use

# check_missing_child_taxa = function(df, problem_taxa, target_taxa) {
#   missing_samples = character(0)
#   
#   unique_flowcam_ids = unique(df$FlowCamID)
#   unique_types = unique(df$type)
#   
#   for (flowcam_id in unique_flowcam_ids) {
#     has_problem_taxa = any(df$newName[df$FlowCamID == flowcam_id] == problem_taxa)
#     
#     if (has_problem_taxa) {
#       for (type in unique_types) {
#         sample_data = df %>%
#           filter(FlowCamID == flowcam_id, type == type)
#         
#         target_abundance = sum(sample_data$abund[sample_data$newName %in% target_taxa])
#         
#         if ((is.na(target_abundance) || target_abundance == 0)) {
#           missing_samples = c(missing_samples, paste(flowcam_id, type, sep = " - "))
#         }
#       }
#     }
#   }
#   
#   if (length(missing_samples) > 0) {
#     cat("Missing target taxa in the following samples:\n")
#     cat(paste(missing_samples, collapse = "\n"), "\n")
#   } else {
#     cat("All samples have the required target taxa.\n")
#   }
# }
# 
# # Example usage:
# check_missing_child_taxa(fcQaDf, "Cyclopoida (unid)", c("Corycaeidae", "Oithona spp."))
# Note that this returns both FC and QA and I am struggling to fix it
# But from exploring the data, I know only the FC sample has no child taxa
# The sample being returned is AMMP_Gulf_StPeters_3B_20200903_250UM

# # Have a look at the sample
# noChildTaxaSample = fcQaDf %>%
#   filter(FlowCamID == "AMMP_Gulf_StPeters_3B_20200903_250UM")

### This agrees with our previous work from the Tech Report.
# From this work (in dividePlankton.R), we knew there were 3 problem samples:
# This one is included in the samples we are analyzing:
# 20_09_03_Gulf_S04_Z40_0942_250 aka	AMMP_Gulf_StPeters_3B_20200903_250UM 

# These two had issues but are not in our samples for this project:
# 20_09_01_Gulf_S04_Z38_1434_250 aka	AMMP_Gulf_StPeters_1_20200901LT_250UM
# AMMP_PA_S04_W01_20200830LT_236UM




