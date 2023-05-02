


install.packages("lsr")
library("lsr")

cohensD(group1, group2)

install.packages("effsize")
library("effsize")

install.packages("rempsyc")
library("rempsyc")

install.packages("broom")
library("broom")



###############################################################################
## Which tides am I testing between

# All the other data is prepped properly, I just have to get Newfoundland in the format I need
nlPrepT = nl %>%
  filter(monthStart == 10 & yearStart == 2021) %>%
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(waterVolAnalyzed = NA) # add this so it doesn't throw an error lol



tTestFun = function(specDF, station, site) {

    dfHT = specDF %>%
    filter(tidePhase == "High" & myLabel == station) %>%
    select(-c("waterVolAnalyzed")) %>%
    select(starts_with("Acartia"):last_col()) %>%
    mutate(abund = rowSums(.),
           rich = specnumber(.),
           shan = exp(diversity(., index = "shannon")),
           sim = diversity(., index = "invsimpson"))
    
   dfLT = specDF %>%
     filter(tidePhase == "Low" & myLabel == station) %>%
     select(-c("waterVolAnalyzed")) %>%
     select(starts_with("Acartia"):last_col()) %>%
     mutate(abund = rowSums(.),
            rich = specnumber(.),
            shan = exp(diversity(., index = "shannon")),
            sim = diversity(., index = "invsimpson"))
    
   tTestResults = bind_rows(
     tidy(t.test(dfHT$abund, dfLT$abund, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$rich, dfLT$rich, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$shan, dfLT$shan, var.equal=T, alternative = "two.sided")),
     tidy(t.test(dfHT$sim, dfLT$sim, var.equal=T, alternative = "two.sided"))
   ) %>%
     select(estimate1, estimate2, parameter, statistic, conf.low, conf.high, p.value) %>%
     mutate(stationName = station, .before = estimate1) %>%
     mutate(siteName = site, .before = stationName)
  
  
}



x= rbind(
  tTestFun(pacAug2020, "Outer", "Lemmens Aug 2020"),
  tTestFun(pacAug2020, "Mid", "Lemmens Aug 2020"),
  tTestFun(pacAug2020, "Inner", "Lemmens Aug 2020"),
  tTestFun(pacJun2021, "Mid", "Lemmens Jun 2021"),
  tTestFun(sober, "Outer", "Sober Island"),
  tTestFun(stPeters, "Outer", "St. Peters"),
  tTestFun(stPeters, "Mid", "St. Peters"),
  tTestFun(stPeters, "Inner", "St. Peters"),
  tTestFun(nlPrepT, "Outer", "St. Peters")
  )









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

tidy(t.test(aug20HT.outer$abund, aug20LT.outer$abund, var.equal=T, alternative = "two.sided"))
tidy(t.test(aug20HT.outer$rich, aug20LT.outer$rich, var.equal=T, alternative = "two.sided"))

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

x = t.test(aug20HT.inner$sim, aug20LT.inner$sim, var.equal=T, alternative = "two.sided")

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






