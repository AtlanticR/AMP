################################################################################
## Make dotplots for different tide ranges within each bay

# The tide data look a bit unusual. Want to create dotplots for each bay to look
# at the distribution of values within each class (High, Mid, Low)
# Many sites do not actually have these divisions, so I am identifying unusual data


### This is just a script for making graphs for a meeting!
# These are not good copy graphs and the code/comments will not be beautiful sry

################################################################################

# Get the processed metadata files
source("DataProcessing/metadataProcessing.R")

################################################################################
## Make the plots. Make them separate for each region and then facet by bay

# Maritimes
# UGHHHHH there are overlaps in the "High" and "Mid" for Sober Island Oyster
ggplot(data = marMeta, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=2.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Gulf
ggplot(data = gulfMeta, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Pacific
# Need to convert tideLevel to numeric because there are NAs
# Since there are NAs, it's will create warning messages
# Also I want to create facets for March, June, August/Sept since the sampling might have varied slightly between months
tidePac = pacMeta %>%
  mutate(myMonth = monthStart) %>%
  mutate(myMonth = str_replace(myMonth, "3", "March 2021")) %>%
  mutate(myMonth = str_replace(myMonth, "6", "June 2021")) %>%
  mutate(myMonth = str_replace(myMonth, "9", "Sept 2021")) %>%
  filter(!tideLevel == "NA")

# Actually I also need to separate by Year since the 2020 data is different UGH!!
tidePac$myMonth = ifelse(tidePac$yearStart == "2020", "2020", tidePac$myMonth)

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


################################################################################
################################################################################
################################################################################
### I ALSO NEED TO DOUBLE CHECK FLOW CYTOMETRY DATA

processMetaFC = function(xlData) {
  dfProc = subset(xlData, sampleType == "C" & # only get Zooplankton data
                    yearStart != 2019) # do not want 2019 data
}  

marFC = processMeta(marMeta) # Maritimes zooplankton data
nlFC = processMeta(nlMeta) # Newfoundland
pacFC = processMeta(pacMeta) # Pacific
gulfFC = processMeta(gulfMeta) # Gulf


# Maritimes
# UGHHHHH there are overlaps in the "High" and "Mid" for Sober Island Oyster
ggplot(data = marFC, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=2.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Gulf
ggplot(data = gulfFC, aes(x = tideRange, y = tideLevel,  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1.5)+
  facet_wrap(~facilityName, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))

# Pacific
# Need to convert tideLevel to numeric because there are NAs
# Since there are NAs, it's will create warning messages
# Also I want to create facets for March, June, August/Sept since the sampling might have varied slightly between months
tidePacFC = pacFC %>%
  mutate(myMonth = monthStart) %>%
  mutate(myMonth = str_replace(myMonth, "3", "March")) %>%
  mutate(myMonth = str_replace(myMonth, "6", "June")) %>%
  mutate(myMonth = str_replace(myMonth, "8", "August/Sept")) %>%
  mutate(myMonth = str_replace(myMonth, "9", "August/Sept")) %>%
  filter(!tideLevel == "NA")

ggplot(data = tidePacFC, aes(x = tideRange, y = as.numeric(tideLevel),  fill=tideRange), pch = 21, col="black",)+
  geom_dotplot(binaxis = "y", stackdir = "center")+
  facet_wrap(~myMonth, scales = "free")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 15))


# Newfoundland
# Nvm there's no tide level data
ggplot()+
  geom_boxplot(data = nlFC, aes(y = tideLevel, col=tideRange))+
  facet_wrap(~facilityName, scales = "free")
