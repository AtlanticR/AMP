################################################################################
## Define the colour palettes and symbols for each bay/region
# This will be called for any figures where the colour scheme needs to be used (e.g., NMDS, rarefaction curves)

# Set the colours for data from each ocean
# Note, these are just the default ggplot colours when there are 4 items to be displayed
# Can get the colours from this function. Put the # of classes in brackets
hue_pal()(4)

#### Based on ocean
# Atlantic Ocean
atlColour = c("Gulf" = "#F8766D", 
              "Maritimes" = "#7CAE00", 
              "Newfoundland" = "#00BFC4")
# Pacific Ocean 
# Note this is the colour when only Pacific OCEAN data is displayed
# When Pacific (region) data is broken down by field season (below), 4 colours will be set
pacColourOne = c("Pacific" = "#C77CFF")

# Colour scheme for creating my study area maps
# Only the regions I want about should have colours. They are the same as above EXCEPT I
# Added 66 to the end of the colours to adjust the transparency (equivalent to alpha = 0.25)
# This makes the region colours look less aggressive
# See here for more info: https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4
regionMapCols = c("Newfoundland & Labrador" = "#00BFC466", "Pacific" = "#C77CFF66", "Ontario and Prairie" = "gray92", "Quebec" = "gray92",
                  "Arctic" = "gray92", "Arctic-Water" = "gray92", "Maritimes" = "#7CAE0066", "Gulf" = "#F8766D66", "Other" = "gray92")

#### Based on bay
# I just looked here and tried to find different shades of each colour set above:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# These are not final colours. Also I do not need to specify the bay name, but it helps to 
# have them written down somewhere 
marColours = c("Argyle" = "darkgreen", 
               "Country Harbour" = "green3", 
               "Sober Island" = "darkolivegreen2", 
               "Whitehead" = "mediumspringgreen")

nlColours = c("Southeast Arm 2020" = "#00BFC4")

gulfColours = c("Cocagne" = "red4", 
                "Malpeque" = "red2", 
                "St. Peters" = "lightpink")

pacColours = c("August 2020" = "plum1", 
               "June 2021" = "magenta4", 
               "March 2021" = "maroon", 
               "September 2021" = "maroon1")

### Also set the symbols for the tides
pchTide = c("High" = 21,
            "Low" = 22,
            "Mid-Falling" = 23,
            "Mid-Rising" = 24)


# Andrea recommended using this colour wheel website for selecting colours to maximize distinctness:
# https://www.canva.com/colors/color-wheel/
stationCol = c("Outer" = "#82C738",
             "Mid" = "#3882C7",
             "Inner" = "#C73882",
             
             "Inner-North" = "#C73882",
             "Inner-South" = "#3882C7",
             
             "North" = "#C73882",
             "Central" = "#3882C7",
             "South" = "#82C738",
             
             "East" = "#82C738",
             "West" = "#C73882",
             
             "Missing" = "white")

# Andrea's colour wheel doesn't go up to 5 colours. Instead use hue_pal? idk.
hue_pal()(5)
# Newfoundland needs own colour scheme since there are 4 stations, not 3
stationColNL = c("Outer" = "#F8766D",
                 "Mid-A" = "#A3A500",
                 "Mid-B" = "#00BF7D",
                 "Mid-C" = "#00B0F6",
                 "Inner" = "#E76BF3",
                 "Missing" = "white")




