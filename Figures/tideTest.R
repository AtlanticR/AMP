#create vectors to hold plant heights from each sample
group1 <- c(8, 8, 14)
group2 <- c(11, 12, 13)

# Perform two sample t-test
# If var.equal = T, then the Welch modification to the degrees of freedom is used
t.test(group1, group2, var.equal=T, alternative = "two.sided")


install.packages("lsr")
library("lsr")

cohensD(group1, group2)

install.packages("effsize")
library("effsize")

install.packages("rempsyc")
library("rempsyc")

cohen.d(group1, group2)

###############################################################################
## Which tides am I testing between

# Pacific:
# August 2020: high vs low at all stations

aug20HT.outer = pacAug2020 %>%
  filter(tidePhase == "High" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

aug20LT.outer = pacAug2020 %>%
  filter(tidePhase == "Low" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>% 
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(aug20HT.outer$abund, aug20LT.outer$abund, var.equal=T, alternative = "two.sided")
t.test(aug20HT.outer$rich, aug20LT.outer$rich, var.equal=T, alternative = "two.sided")
t.test(aug20HT.outer$shan, aug20LT.outer$shan, var.equal=T, alternative = "two.sided")
t.test(aug20HT.outer$sim, aug20LT.outer$sim, var.equal=T, alternative = "two.sided")

##
aug20HT.mid = pacAug2020 %>%
  filter(tidePhase == "High" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  # mutate(abund = rowSums())
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

aug20LT.mid = pacAug2020 %>%
  filter(tidePhase == "Low" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>% 
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(aug20HT.mid$sim, aug20LT.mid$sim, var.equal=T, alternative = "two.sided")


####
aug20HT.inner = pacAug2020 %>%
  filter(tidePhase == "High" & myLabel == "Inner") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

aug20LT.inner = pacAug2020 %>%
  filter(tidePhase == "Low" & myLabel == "Inner") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(aug20HT.inner$sim, aug20LT.inner$sim, var.equal=T, alternative = "two.sided")

# June 2021: high vs low at Mid stations
jun21HT.mid = pacJun2021 %>%
  filter(tidePhase == "High" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

jun21LT.mid = pacJun2021 %>%
  filter(tidePhase == "Low" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(jun21HT.mid$sim, jun21LT.mid$sim, var.equal=T, alternative = "two.sided")



# Maritimes: 
# Sober Island: Outer station (high vs low)

soberHT.outer = sober %>%
  filter(tidePhase == "High" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

soberLT.outer = sober %>%
  filter(tidePhase == "Low" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(soberHT.outer$sim, soberLT.outer$sim, var.equal=T, alternative = "two.sided")



# Gulf: 
# I think all stations, but note Idk because sample size is different?

# Outer 
stpHT.outer = stPeters %>%
  filter(tidePhase == "High" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

stpLT.outer = stPeters %>%
  filter(tidePhase == "Low" & myLabel == "Outer") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(stpHT.outer$sim, stpLT.outer$sim, var.equal=T, alternative = "two.sided")

## Mid
stpHT.mid = stPeters %>%
  filter(tidePhase == "High" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

stpLT.mid = stPeters %>%
  filter(tidePhase == "Low" & myLabel == "Mid") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(stpHT.mid$sim, stpLT.mid$sim, var.equal=T, alternative = "two.sided")

### Inner
stpHT.inner = stPeters %>%
  filter(tidePhase == "High" & myLabel == "Inner") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

stpLT.inner = stPeters %>%
  filter(tidePhase == "Low" & myLabel == "Inner") %>%
  select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(stpHT.inner$sim, stpLT.inner$sim, var.equal=T, alternative = "two.sided")

# Newfoundland: Outer High vs Low

nlHT.outer = nl %>%
  filter(monthStart == 10 & yearStart == 2021) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%  # replace NAs with 0
  filter(tidePhase == "High" & myLabel == "Outer") %>%
  #select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

nlLT.outer = nl %>%
  filter(monthStart == 10 & yearStart == 2021) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%  # replace NAs with 0
  filter(tidePhase == "Low" & myLabel == "Outer") %>%
  #select(-c("waterVolAnalyzed")) %>%
  select(starts_with("Acartia"):last_col()) %>%
  mutate(abund = rowSums(.),
         rich = specnumber(.),
         shan = exp(diversity(., index = "shannon")),
         sim = diversity(., index = "invsimpson")) %>%
  select(abund, rich, shan, sim)

t.test(nlHT.outer$sim, nlLT.outer$sim, var.equal=T, alternative = "two.sided")



data(DiversityData)

Diversity(DiversityData$Abu, "abundance", q = c(0, 0.5, 1, 1.5, 2))




