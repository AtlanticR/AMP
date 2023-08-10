###############################################################################
# I need to get the average distance between stations
# This is for the study area explanations

###############################################################################

# This has all the station info
source("DataProcessing/studyAreaDataPrep.R")

# The punctual stations include
# Maritimes: WhiteHead, Country Harbour
# Newfoundland: Southeast Arm
# Pacific: LEMMENS


# Get the x/y coordinates for each sample
# Change the source df based on location
getXYcoords = nlPunctualUTM %>%
  st_coordinates() %>%
  as.data.frame() %>%
  setNames(c("x", "y"))

# Get the average coordinate position for each station
avgCoord = cbind(nlPunctualUTM, getXYcoords) %>%
  group_by(facilityName, myLabel) %>%
  summarise(mean_x = mean(x), 
           mean_y = mean(y)) %>%
  mutate(dist = sqrt((mean_x))) %>%
  filter(facilityName == "WhiteHead") # filter for the bay I'm interested in. If Pacific, do not highlight this bit

# Get the distances between each points
# I really don't know what was happening. but the st_distance() and dist() functions are giving weird answers. Just calculate manually
df = avgCoord %>%
  as.data.frame() %>% # turn from sf object to dataframe
  mutate(row_num = row_number()) %>%
  inner_join(avgCoord, by = character()) %>% # rearrange df to do all comparisons
  filter(myLabel.x != myLabel.y) %>% # do not do e.g., inner vs inner
  mutate(dist = sqrt((mean_x.x - mean_x.y)^2 + (mean_y.x - mean_y.y) ^2)) %>% # calc distances
  distinct(dist, .keep_all = T) %>% # keep only unique comparisons
  select(myLabel.x, myLabel.y, dist) # keep only columns i care about
  

###############################################################################
### TEST WITH TRANSECTS

# Options are:
# Maritimes: Argyle, Sober Island Oyster
# Gulf: Cocagne, Malpeque, StPeters



# Get the average coordinate position for each station
avgCoord = gulfTransectUTM %>%
  mutate(avgX = (lat+latEnd)/2, # 
         avgY = (lon + lonEnd)/2) %>%
  group_by(facilityName, myLabel) %>%
  summarise(mean_x = mean(avgX), 
            mean_y = mean(avgY)) %>%
  filter(facilityName == "StPeters") # filter for the bay I'm interested in

df = avgCoord %>%
  as.data.frame() %>% # turn from sf object to dataframe
  mutate(row_num = row_number()) %>%
  inner_join(avgCoord, by = character()) %>% # rearrange df to do all comparisons
  filter(myLabel.x != myLabel.y) %>% # do not do e.g., inner vs inner
  mutate(dist = sqrt((mean_x.x - mean_x.y)^2 + (mean_y.x - mean_y.y) ^2)) %>% # calc distances
  distinct(dist, .keep_all = T) %>% # keep only unique comparisons
  select(myLabel.x, myLabel.y, dist) # keep only columns i care about











### KEEPING JUST IN CASE
# BUT THIS WASN"T WORKING FOR SO MANY SITES?????

# Calc the distances between each station
# distances = avgCoord %>%
#   st_distance() %>%
#   as.data.frame() 
# 
# # Rename the rows and columns so it's easier to tell which distance is which
# rownames(distances) = avgCoord$myLabel
# colnames(distances) = avgCoord$myLabel
# 
# test = c
# 
# stats::dist(avgCoord$mean_x)
# 
# arg = marTransectUTM %>%
#   filter(facilityName == "Argyle")
# 
# # write.csv(arg, "ArgyleCoords.csv")
# 
# 
# 



