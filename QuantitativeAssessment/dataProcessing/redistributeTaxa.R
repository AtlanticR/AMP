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

# I also used this approach in the Tech Report under TechReport/DataProcessing/dividePlankton.R
# But the code was a bit messy so I'm fixing it up here

# There are also a few taxa from the Quantitative Assessment that need to be "fixed" in some capacity

################################################################################
### Set up

# Run the script that compiles the data
source("QuantitativeAssessment/dataProcessing/QAcodeMatches.R") 


################################################################################


### Adjustments to the taxa list

# Need to redistribute some of the higher level taxa (i.e., "parent taxa"/"problem_taxa") 
# between lower level taxa (i.e., "child taxa" "target_taxa")
# This is based on the relative abundance of child taxa. e.g., if Acartia form 50% of Calanoida, then 50% of abundances
# will be distributed to them

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
  
  # Remove the damaged taxa from the dataframe
  df = df %>% 
    filter(newName != problem_taxa)
  
  # Drop the unnecessary columns
  df = df %>% 
    select(-c(total_abund_problem, total_abund_target, starts_with("prop_")))
  
  return(df)
}

# Make this a new variable for testing
test = fcQaDf

### MAKE SPECIFIC ADJUSTMENTS TO THE QUANTITATIVE ASSESSMENT DATASET

# Remove the EGG MASS taxa from the dataset
test = test %>%
  filter(newName != "EGG CHECK")

# Fix the damaged taxa. Distribute either among the adults of that taxa, or the ci-ciii
test = redistribute_abundances(test, "Acartia spp. DAMAGED", c("Acartia spp.", "Acartia spp. ci-ciii")) %>%
  # Then change Acartia ci-ciii to Calanoida ci-ciii to be consistent with rest of the dataset
  mutate(newName = ifelse(newName == "Acartia spp. ci-ciii", "Calanoida ci-ciii", newName))

# Then do the same with damaged Temora spp.
test = redistribute_abundances(test, "Temora spp. DAMAGED", c("Temora spp.", "Temora spp. ci-ciii")) %>%
  mutate(newName = ifelse(newName == "Temora spp. ci-ciii", "Calanoida ci-ciii", newName))

# Then redistribute the Decapoda that was unspecified
test = redistribute_abundances(test, "Decapoda- CHECK", c("Decapoda- brachyura (larvae)", "Decapoda- non-brachyura (larvae)", "Caridea (larvae)", "Paguroidea (larvae)", "Porcellanidea (larvae)"))


### MAKE SPECIFIC ADJUSTMENTS TO THE FLOWCAM DATASET

# Redistribute Cyclopoida
# NOTE: NEED TO MAKE A CHECK TO SEE WHAT HAPPENS IF NO CYLCOPOIDA CHILD
# test = redistribute_abundances(test, "Cyclopoida", c("Cyclopoida ci-ciii", "Corycaeidae", "Oithona spp."))

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