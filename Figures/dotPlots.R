################################################################################
## Make dotplots for different tide ranges within each bay

# The tide data look a bit unusual. Want to create dotplots for each bay to look
# At the 





################################################################################

# Get the processed metadata files
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/metadataProcessing.R")



## Looking at the tide data

# Maritimes
# UGHHHHH there are overlaps in the "High" and "Mid" for Sober Island Oyster
ggplot(data = marZoo, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=2.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))


# Gulf
ggplot(data = gulfZoo, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Pacific
# Need to convert tideLevel to numeric because there are NAs
# Since there are NAs, it's will create warning messages
# Also I want to create facets for March, June, August/Sept since the sampling might have varied slightly between months
tidePac = pacZoo %>%
  mutate(myMonth = monthStart) %>%
  mutate(myMonth = str_replace(myMonth, "3", "March")) %>%
  mutate(myMonth = str_replace(myMonth, "6", "June")) %>%
  mutate(myMonth = str_replace(myMonth, "8", "August/Sept")) %>%
  mutate(myMonth = str_replace(myMonth, "9", "August/Sept")) %>%
  filter(!tideLevel == "NA")

ggplot(data = tidePac, aes(x = tideRange, y = as.numeric(tideLevel),  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center")+
  facet_wrap(~myMonth, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Newfoundland
# Nvm there's no tide level data
ggplot()+
  geom_boxplot(data = nlZoo, aes(y = tideLevel, col=tideRange))+
  facet_wrap(~facilityName, scales = "free")
