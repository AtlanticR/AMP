## GET THE RELEVANT R PACKAGES

# I might change how I do this
# But for now I'm putting all R packages here and then calling this script at the beginning of every other script with source()

# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("broom", "cowplot", "devtools", "dplyr", "egg", "ggplot2", "ggrepel", "ggsignif", "ggsn", "ggspatial", "ggthemes", "ggh4x", "gridExtra", "iNEXT", "jcolors", "leaflet", "mapr", "mapview",
             "patchwork", "pkgcond", "purrr", "readxl", "remotes", "rgdal", "rnaturalearth", "rnaturalearthdata", "scales", "sf", "sp", "stringr", "tidyr", "tools", "useful", "vegan", "wbstats", "wpa")
ipak(packages)


# Add this to get rid of a warning message that comes up from grouping 
# More info here: https://statisticsglobe.com/dplyr-message-summarise-has-grouped-output-r
options(dplyr.summarise.inform = FALSE)

# I NEED TO LOOK INTO PLYR PACKAGE AND HOW TO LOAD ALONG WITH DPLYR
# I need plyr for one thing. but it also suppresses other function within dplyr
