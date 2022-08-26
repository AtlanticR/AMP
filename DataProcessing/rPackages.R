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
packages = c("dplyr", "ggplot2", "ggrepel", "ggthemes", "gridExtra", "jcolors", "leaflet", "mapr", "mapview",
             "plyr", "purrr", "readxl", "scales", "stringr", "tidyr", "tools", "useful", "vegan", "wbstats", "wpa")
ipak(packages)