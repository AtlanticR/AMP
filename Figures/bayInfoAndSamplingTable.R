################################################################################
## Create table of information for Tech Report that characterized

source("Figures/nmdsRegions.R")

# The metadata files are: marMeta, nlMeta, pacMeta, gulfMeta

# Get the columns that are actually important to merge
test = allRegionsWide %>%
  # If I don't ungroup I get an "add missing grouping variables:..." warning
  # See here for more details: https://datacornering.com/dplyr-adding-missing-grouping-variable/
  ungroup() %>%
  select(region, facetFactor, yearStart, monthStart, tidePhase, myLabel) %>%
  mutate(monthLetters = month.abb[monthStart])

test2 = test %>%
  group_by(region, facetFactor, yearStart, monthStart, myLabel, tidePhase) %>%
  summarise(count = n())




allBays = dplyr::bind_rows(redMeta(gulfMerge), redMeta(nlMerge), redMeta(marMerge), redMeta(pacMerge))

z = marMeta %>%
  select(c(region, facilityName, yearStart, monthStart, dayStart, tidePhase, myLabel)) %>%
  mutate(monthLetters = month.abb[marMeta$monthStart]) %>%
  group_by(facilityName, tidePhase) %>%
  summarise(count = n())

z2 = marMeta %>%
  select(c(region, facilityName, yearStart, monthStart, dayStart, tidePhase, myLabel)) %>%
  mutate(monthLetters = month.abb[marMeta$monthStart]) %>%
  group_by(facilityName, myLabel) %>%
  summarise(count = n())


z3 = marMeta %>%
  select(c(region, facilityName, yearStart, monthStart, dayStart, tidePhase, myLabel)) %>%
  mutate(monthLetters = month.abb[monthStart]) %>%
  mutate(tideStn = paste(myLabel, tidePhase)) %>%
  group_by(facilityName, tideStn) %>%
  summarise(count = n())

q = merge(z, z2, by = "facilityName")
q2 = merge(q, z3)

# reduced metadata
red = marMeta %>%
  select(c(region, facilityName, yearStart, monthStart, dayStart, tidePhase, myLabel))

hi = merge(red, q2, by = "facilityName")
