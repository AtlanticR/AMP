################################################################################
#### Redistribute taxa




################################################################################
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
test = fcQaDf

# Remove the "egg mass" taxa from the dataset. There are WAY too many of them in the Pacific dataset
# Asked the lead taxonomist. Said these could be "Invertebrate egg/trochophore" but I think they must
# have counted them differently. Also remove "Invertebrate egg/trochophore" for consistency
test = test %>%
  filter(newName != "EGG CHECK" & newName != "Invertebrate (egg, trochophore larvae)")

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
test = redistribute_abundances(test, "Decapoda- CHECK", c("Decapoda- brachyura (larvae)", "Decapoda- non-brachyura (larvae)"))


### MAKE SPECIFIC ADJUSTMENTS TO THE FLOWCAM DATASET

# Redistribute Cyclopoida
# NOTE: NEED TO MAKE A CHECK TO SEE WHAT HAPPENS IF NO CYLCOPOIDA CHILD
test = redistribute_abundances(test, "Cyclopoida", c("Cyclopoida ci-ciii", "Corycaeidae", "Oithona spp."))

test = redistribute_abundances(test, "Cyclopoida", c("HI", "DOG"))

# Redistribute Calanoida
test = redistribute_abundances(test, "Calanoida (unid)", c("Acartia spp.", "Calanoida ci-ciii", "Calanus spp.", "Centropages spp.", "Chiridius spp.", "Eurytemora spp.",
                                                           "Metridia spp.", "Microcalanus spp.", "Paracalanus spp.", "Pseudocalanus spp.", "Pseudodiaptomus spp.", "Temora spp.", "Tortanus spp."))

# Redistribute Copepoda among all possible MAKE THIS CLEARER
test = redistribute_abundances(test, "Copepoda (unid)", c("Cyclopoida ci-ciii", "Corycaeidae", "Oithona spp.", "Harpacticoida- epibenthic", "Microsetella spp.", "Monstrillidae", "Acartia spp.", "Calanoida ci-ciii", "Calanus spp.", "Centropages spp.", "Chiridius spp.", "Eurytemora spp.",
                                                          "Metridia spp.", "Microcalanus spp.", "Paracalanus spp.", "Pseudocalanus spp.", "Pseudodiaptomus spp.", "Temora spp.", "Tortanus spp."))


# Redistribute zooplankton (unid)
test = redistribute_abundances(test, "Zooplankton (unid)", unique(fcQaDf$newName[fcQaDf$newName != "Zooplankton (unid)"]))

test = test %>%
  group_by(newName, FlowCamID, regionYear, qaSampleID, type) %>%
  summarize(abund = sum(abund))


fcQaDf = test




######################
# Code for testing if there are samples with no child taxa to distribute parent taxa

check_missing_child_taxa = function(df, problem_taxa, target_taxa) {
  missing_samples <- character(0)
  
  for (flowcam_id in unique(df$FlowCamID)) {
    for (type in unique(df$type[df$FlowCamID == flowcam_id])) {
      sample_data <- df %>%
        filter(FlowCamID == flowcam_id, type == type)
      
      problem_abundance <- sum(sample_data$abund[sample_data$newName == problem_taxa])
      target_abundance <- sum(sample_data$abund[sample_data$newName %in% target_taxa])
      
      if (is.na(target_abundance) || target_abundance == 0) {
        missing_samples <- c(missing_samples, paste(flowcam_id, type, sep = " - "))
      }
    }
  }
  
  if (length(missing_samples) > 0) {
    cat("Missing target taxa in the following samples:\n")
    cat(paste(missing_samples, collapse = "\n"), "\n")
  } else {
    cat("All samples have the required target taxa.\n")
  }
  
}

# Example usage:
missing_samples <- check_missing_child_taxa(test, "Cyclopoida", c("Calanoida (unid)"))




x = check_missing_child_taxa(test, "Copepoda (unid)", c("Cyclopoida ci-ciii", "dog", "Oithona spp.", "Harpacticoida- epibenthic", "Microsetella spp.", "Monstrillidae", "Acartia spp.", "Calanoida ci-ciii", "Calanus spp.", "Centropages spp.", "Chiridius spp.", "Eurytemora spp.",
                                                          "Metridia spp.", "Microcalanus spp.", "Paracalanus spp.", "Pseudocalanus spp.", "Pseudodiaptomus spp.", "Temora spp.", "Tortanus spp."))











