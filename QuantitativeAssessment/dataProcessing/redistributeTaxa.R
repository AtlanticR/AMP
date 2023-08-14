################################################################################
#### Redistribute taxa




################################################################################
## Background

# There are a few taxa within both the FlowCam and Quantitative Assessment lists
# that need to be dealt with

# From the FlowCam list, these include:
# 

################################################################################


########################################################
### Adjustments to the taxa list




redistribute_abundances = function(df, damaged_taxa, target_taxa){
  
  # Filter the data frame for the damaged taxa
  df_damaged = df %>%
    filter(newName == damaged_taxa)
  
  # Calculate the total abundance of the damaged taxa for each sample
  df_damaged_total = df_damaged %>%
    group_by(FlowCamID, type) %>%
    summarize(total_abund_damaged = sum(abund))
  
  # Join the total abundance back to the original dataframe
  df = df %>%
    left_join(df_damaged_total, by = c("FlowCamID", "type"))
  
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
      mutate(abund = if_else(newName == taxon & !is.na(total_abund_damaged),
                             abund + get(paste0("prop_", taxon)) * total_abund_damaged, abund))
  }
  
  # Remove the damaged taxa from the dataframe
  df = df %>% 
    filter(newName != damaged_taxa)
  
  # Drop the unnecessary columns
  df = df %>% 
    select(-c(total_abund_damaged, total_abund_target, starts_with("prop_")))
  
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