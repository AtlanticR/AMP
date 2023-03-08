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
getXYcoords = nlPunctualUTM %>%
  st_coordinates() %>%
  as.data.frame() %>%
  setNames(c("x", "y"))

# Get the average coordinate position for each station
avgCoord = cbind(nlPunctualUTM, getXYcoords) %>%
  group_by(facilityName, myLabel) %>%
  summarise(mean_x = mean(x), 
           mean_y = mean(y)) %>%
  filter(facilityName == "Country Harbour") # filter for the bay I'm interested in

# Calc the distances between each station
distances = avgCoord %>%
  st_distance() %>%
  as.data.frame() 

# Rename the rows and columns so it's easier to tell which distance is which
rownames(distances) = avgCoord$myLabel
colnames(distances) = avgCoord$myLabel












