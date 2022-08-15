# Function to load multiple packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Choose necessary packages
packages = c("dplyr", "ggplot2", "ggrepel", "ggthemes", "jcolors", "leaflet", "mapr", "mapview", "readxl", "stringr",
             "tidyr", "tools", "useful", "vegan", "wpa")
ipak(packages)