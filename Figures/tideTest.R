################################################################################
################################################################################
### Testing diversity indices between tides

# In this code, we're testing if diversity is different between high and low tide
# Specifically, if: abundance, richness, Shannon diversity and Simpson diversity are different
# We're only testing in sites where there are at least 3 samples collected at each tide, for specific stations

# Tests are therefore conducted in: 
# Outer, Mid and Inner stations for Lemmens Aug 2020
# Mid stations for Lemmens Jun 2021
# Outer stations for Sober Island
# Outer, Mid, Inner stations for St. Peters
# Outer stations for Southeast Arm Oct 2021

## Background info
# This paper is helpful for discussing t-tests with small sample sizes:
# https://scholarworks.umass.edu/pare/vol18/iss1/10/

# Hill numbers are a bit confusing. These have some more explanation:
# See here for an explanation of the confusing indices especially Simpson: http://www.countrysideinfo.co.uk/simpsons.htm
# This is also helpful for getting diversity from indices: https://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/

###############################################################################
###############################################################################
## Setup

# Read in the data 
# I'm using datasets that have already be broken up by site
source("DataProcessing/bayBreakdown.R")

# Get rid of that awful scientific notation!!
options(scipen = 999)

###############################################################################
## Run the tests

# Create a function to run the t-tests and organize all the info in a nice format
# Function passes in the species data frame for each site, the station being tested, and
# the name of the site (that will be included in the table)
tTestFun = function(specDF, station, site) {

    # Reorganize the data frame for HIGH TIDE samples
  dfHT = specDF %>%
    filter(tidePhase == "High" & myLabel == station) %>%
    # Remove the waterVolAnalyzed column (it's required for rarefaction)
    select(-c("waterVolAnalyzed")) %>%
    # Only look at data from Acartia (first alphabetically) to the end of specDF
    select(starts_with("Acartia"):last_col()) %>%
    mutate(abund = rowSums(.), # abundance
           rich = specnumber(.), # richness
           shan = exp(diversity(., index = "shannon")), # shannon DIVERSITY (exponential of shannon index)
           sim = diversity(., index = "invsimpson")) # simpson DIVERSITY
    
   # Reorganize the data for LOW TIDE samples  
  dfLT = specDF %>%
     filter(tidePhase == "Low" & myLabel == station) %>%
     select(-c("waterVolAnalyzed")) %>%
     select(starts_with("Acartia"):last_col()) %>%
     mutate(abund = rowSums(.),
            rich = specnumber(.),
            shan = exp(diversity(., index = "shannon")),
            sim = diversity(., index = "invsimpson"))
  
   # Run the two-tailed t-tests
   # setting var.equal = T means the Welch test is not done (recommended for small sample sizes, see paper in Background section)
   # Use the tidy() function to get results in a better format (tibble)
   tTestResults = bind_rows(
     tidy(t.test(dfHT$abund, dfLT$abund, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$rich, dfLT$rich, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$shan, dfLT$shan, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$sim, dfLT$sim, var.equal=T, alternative = "two.sided"))
   ) %>%
     
     # Reorganize created dataframe to get it in the format I want (e.g., select certain columns, round info, etc)
     # Note rows go: abundance, richness, shannon, simpson. I'll do this by hand later (it's easier)
     mutate(ci95 = paste0("[", round(conf.low,2), ", ", round(conf.high,2), "]")) %>%
     select(estimate1, estimate2, parameter, statistic, ci95, p.value) %>%
     mutate(stationName = station, .before = estimate1) %>%
     mutate(siteName = site, .before = stationName) %>%
     mutate(across(c(estimate1, estimate2, statistic), round, 2)) %>%
     mutate(across(c(p.value), round, 3)) 
     
}


# Run the t-tests for each site. Store the info in a data frame
tTestResults= rbind(
  tTestFun(pacAug2020, "Outer", "Lemmens Aug 2020"),
  tTestFun(pacAug2020, "Mid", "Lemmens Aug 2020"),
  tTestFun(pacAug2020, "Inner", "Lemmens Aug 2020"),
  tTestFun(pacJun2021, "Mid", "Lemmens Jun 2021"),
  tTestFun(sober, "Outer", "Sober Island"),
  tTestFun(stPeters, "Outer", "St. Peters"),
  tTestFun(stPeters, "Mid", "St. Peters"),
  tTestFun(stPeters, "Inner", "St. Peters"),
  tTestFun(seArm2021, "Outer", "Southeast Arm Oct 2021")
  )

# Write data to a csv
# write.csv(tTestResults, "tTestResults.csv")









