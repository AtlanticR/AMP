################################################################################
# AMP making different charts with the zooplankton data
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up

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

################################################################################
## Set up the data

# Define the directory where data need to be read from 
allFolders = "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data/"

# Get a list of all the folders that are within that directory
allDataNames =
  list.files(
    allFolders,
    full.names = T, # don't want full directory names
  )

# Want a list of csv files containing all the zooplankton counts
# There are slightly different file structures based on how the taxonomists did their work
# Some datasets do not have a "Zooplankton Identification Data" folder (they were "cleaned" while ID'ing)
# Therefore we need to check for files in two different directories (but both end in "Classification Summary")

# For reference, these datasets do NOT have a "Zooplankton Identification Data" folder:
# AAMP NL 2020 Zooplankton Data
# AAMP Pacific June 2021 Zooplankton Data
# AAMP Pacific March 2021 Zooplankton Data
# AAMP Pacific Sept 2021 Zooplankton Data

# In some cases I need the full directory names and sometimes I don't
# I found it easiest to just get these separately

## Get full directory names
# Initialize empty list
dirFull = list()

# Loop through every folder in allDataNames
for(i in 1:length(allDataNames)){
  dirFull[[i]] = list.files(
    # List the files! Some will have a "Zooplankton Identification Data" folder. Some won't.
    path = c(paste(allDataNames[i], "/Classification Summary", sep = ""),
      paste(allDataNames[i], "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = T, # Get full directory names
    pattern = ".csv")
}

## Get just the file names (i.e., not the full directory names)
# This is helpful for naming things and matching to the metadata spreadsheet
# Initialize empty list
dirShort = list()

for(i in 1:length(allDataNames)){
  dirShort[[i]] = list.files(
    path = c(paste(allDataNames[i], "/Classification Summary", sep = ""),
             paste(allDataNames[i], "/Zooplankton Identification Data/Classification Summary", sep = "")),
    full.names = F, # Just get the file names
    pattern = ".csv")
  # Remove the file extension
  dirShort[[i]] = sub('\\.csv$', '', dirShort[[i]])
}

################################################################################
## Create dataframes with data

# Create a function that will make dataframes with species counts for each dataset
# Pass in the list of files with full directory name (xl_dataFull) and just file names (xl_dataShort)
speciesDF = function(xlDataFull, xlDataShort) {
  
# Make an empty list to store the data
datalist = list()

# The data for Newfoundland 2020 has a different layout and needs to be processed separately!
# if statement checks if the list of data being passed in matches the Newfoundland 2020 data (the 4th element in the list)
# Does not compare the ENTIRE list of elements. Just checks if the FIRST element is the same (so there is only one TRUE instead of many)
if (compare.list(xlDataShort[1], dirShort[[4]][1])){
  
  # Loop through all the data and extract the class (plankton taxa), count (# of 
  # cells in the sample), and particles (# cells/ml)
  for(i in 1:length(xlDataFull)) {  
    # Read in the files (it's a list of lists)
    # Skip the first two lines of data
    df = read.csv(xlDataFull[i], skip = 2) %>% 
      # Keep everything before the "End Metadata Statistics" part (remove that and after)
      filter(row_number() < which(Name =='======== End Metadata Statistics ========')) %>%
      # Rename the columns to match the other data files
      # format: new = cold
      rename(class = Name, count = Count, particles = Particles...ml)
    # Add sample name as a column
    df$sample = xlDataShort[i]
    # add each element to a new list
    datalist[[i]] = df
  }
  
# For all other data (not NL 2020)
} else {

  # Loop through all the data and extract the class (plankton taxa), count (# of 
  # cells in the sample), and particles (# cells/ml)
  for(i in 1:length(xlDataFull)){

    # Read in the files
    data = read.csv(xlDataFull[i]) 
    
    # Extract full row of data which contain class, count and particle information 
    class = data[which(str_detect(data[,1], "Class$")), ] 
    count = data[which(str_detect(data[,1], "Count")), ]
    particles = data[which(str_detect(data[,1], "Particles")), ]
    
    # Combine this all into a dataframe
    # Don't want the full row, we only want the 2nd column with the actual data
    # Label it with the file name (without directory/extension)
    df = as.data.frame(cbind("sample" = xlDataShort[i],
      "class" = class[,2], "count" = count[,2], "particles" = particles[,2]))
    datalist[[i]] = df
  }
} 
  
# Bind together this list of dataframes into one big data frame
siteDf = dplyr::bind_rows(datalist)

# Convert counts to numeric
siteDf$count = as.numeric(siteDf$count)

# Fix generic typos/edit to ensure consistency between entries: 
# Remove all underscores and replace them with spaces
siteDf$class = us_to_space(siteDf$class)
# Make entire string lowercase
siteDf$class = str_to_lower(siteDf$class)
# Then capitalize the first letter
siteDf$class = str_to_sentence(siteDf$class)
# Remove instances of multiple spaces between words
siteDf$class = str_squish(siteDf$class)
siteDf$class = str_replace(siteDf$class,"spp", "spp.")
  
# Remove unwanted classes
# These do not contain relevant zooplankton data
# Explanations of these terms are in "Zooplankton Samples" xlsx for each site (in the "Data and Classes" sheet)
siteDf = siteDf %>%
  subset(!grepl("[0-9]", class) & # remove the "Class 1-9" data
           class != "Benthic" &
           class != "Bubbles" &
           class != "Clumped zooplankton" &
           class != "Clumped zooplankton/debris" &
           class != "Clumped zooplankton debris" &
           class != "Cut images" &
           class != "Debris" &
           class != "Debris or zooplankton" & 
           class != "Diatom" &
           class != "Duplicate images" &
           class != "Extra taxa" &
           class != "Fragments of zooplankton" &
           class != "Leftover" &
           class != "Leftovers") %>%
  
  # Fix typos in classes
  # Also ensure the classes are consistent between all locations
  mutate(class = replace(class, class == "Calananoida (unid)", "Calanoida (unid)")) %>%
  mutate(class = replace(class, class == "Calanoid civ-vi", "Calanoida civ-vi")) %>%
  mutate(class = replace(class, class == "Calanoid cv-vi", "Calanoida cv-vi")) %>%
  mutate(class = replace(class, class == "Centropages spp civ-vi", "Centropages civ-vi")) %>%
  mutate(class = replace(class, class == "Cirripedia nauplius", "Cirripedia nauplii")) %>%
  mutate(class = replace(class, class == "Ctenophora larva", "Ctenophora larvae")) %>%
  mutate(class = replace(class, class == "Cyclopoida spp.", "Cyclopoida")) %>%
  mutate(class = replace(class, class == "Cumacea juvenileadult", "Cumacea juvenile adult")) %>%
  mutate(class = replace(class, class == "Decapoda brachyura zoea larvae larvae", "Decapoda brachyura zoea larvae")) %>%
  mutate(class = replace(class, class == "Decapoda nonbrachyura zoea", "Decapoda non-brachyura zoea")) %>%
  mutate(class = replace(class, class == "Decapoda nonbrachyura zoea larvae", "Decapoda non-brachyura zoea larvae")) %>%
  mutate(class = replace(class, class == "Decpoda brachyura zoea", "Decapoda brachyura zoea")) %>%
  mutate(class = replace(class, class == "Gastropoda limacina spp. larvaeadult", "Gastropoda limacina spp. larvae adult")) %>%
  mutate(class = replace(class, class == "Monstrilloida", "Monstrilloida spp.")) %>%
  mutate(class = replace(class, class == "Mysidacea juvenileadult", "Mysidacea juvenile adult")) %>%
  mutate(class = replace(class, class == "Osteichthys egg", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Osteichthys eggs", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Osteichthys larvae", "Osteichthyes larvae")) %>%
  mutate(class = replace(class, class == "Osteichthyes eggs", " Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Ostheichthys eggs", "Osteichthyes egg")) %>%
  mutate(class = replace(class, class == "Ostracoda spp.", "Ostracoda")) %>%
  mutate(class = replace(class, class == "Platyhelmenthes nemertea larva", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Platyhelmenthes nemertrea larvae", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Platyhelminthes nemertea larvae", "Platyhelmenthes nemertea larvae")) %>%
  mutate(class = replace(class, class == "Unid zooplankton", "Zooplankton (unid)")) %>%
  mutate(class = replace(class, class == "Zooplankton", "Zooplankton (unid)")) %>%
  mutate(class = replace(class, class == "Zooplankton (unid))", "Zooplankton (unid)"))
  
# Return the final corrected dataframe!
# Will return a df with the sample name, class (taxa), count, particle (count/ml) as columns 
return(siteDf)

}

################################################################################
## Create data frames of each dataset

# Run the speciesDF function and create the dataframes for dataset
# This returns a dataframe with columns for sample, class, count, particles
# Note this might need to be turned into wide table format eventually
gulf20 = speciesDF(dirFull[[1]], dirShort[[1]])
gulf21 = speciesDF(dirFull[[2]], dirShort[[2]])
mar21 = speciesDF(dirFull[[3]], dirShort[[3]])
nl20 = speciesDF(dirFull[[4]], dirShort[[4]]) # This is the one in a different format
nl21 = speciesDF(dirFull[[5]], dirShort[[5]])
pac20 = speciesDF(dirFull[[6]], dirShort[[6]])
pacJun21 = speciesDF(dirFull[[7]], dirShort[[7]])
pacMar21 = speciesDF(dirFull[[8]], dirShort[[8]])
pacSep21 = speciesDF(dirFull[[9]], dirShort[[9]])

# Check for consistency between classes
checkClass = data.frame(unique(sort(c(gulf20$class, gulf21$class, mar21$class, nl21$class, pac20$class, pacJun21$class, pacMar21$class, pacSep21$class))))
      

################################################################################
## TEST SECTION: 

# Convert data into better format
# Each row is a station, each column is a class. Each cell represents the counts. 
mar21New = 
  mar21 %>%
  select(-particles) %>% # Remove this column
  # Modify the data frame into wide format
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  pivot_wider(names_from = class, values_from = count) %>%
  mutate_all(~replace(.,is.na(.), 0)) # replace NAs with 0

metaMarTest = 
  marZoo %>%
  select(facilityName, sampleCode, waterVolume, tideRange, yearStart)


mar21NewTest= 
  mar21New %>%
  select(sample)

x =full_join(mar21NewTest, metaMarTest, by=c("sample" = "sampleCode"))

y =full_join(metaMarTest, mar21NewTest, by=c("sampleCode" = "sample"))

#### I THINK THERE IS A LABELLING ISSUE
# in the metadata there's a station called 21_08_25_Mar_S03_Z01_1548_250 
# in the actual data files it's 21_08_25_Mar_S03_Z01_1538_250 (38 VS 48!!)



mar21New$sample=="21_08_25_Mar_S03_Z01_1538_250"
mar21New$sample=="21_08_25_Mar_S03_Z01_1548_250"

metaMarTest$sampleCode=="21_08_25_Mar_S03_Z01_1538_250"
metaMarTest$sampleCode=="21_08_25_Mar_S03_Z01_1548_250"


################################################################################
## Alter data format for creation of pie charts

piePrep = function(plotData) {

# Do some cleaning of the data
plotData = plotData %>% 
  
  # Want counts per taxa (class) for the whole bay, not by tow
  group_by(class) %>%
  summarize(countBay = sum(count)) %>%
  
  # Need to create an "Other" class so the pie charts don't have too many slices
  # Keep the 5 most abundant classes, name the rest "Other"
  mutate(rank = rank(-countBay), 
         classNew = ifelse(rank <= 5, class, 'Other')) %>%
  
  # Manipulate the dataframe to show the percentage of each class
  # This will condense the dataframe (it's easier to plot this way)
  group_by(classNew) %>%
  summarise(sumCount = sum(countBay)) %>%
  mutate(perc = sumCount / sum(sumCount)*100) %>%
  
  # Add a column called "pos" to get positions of where the percent labels should go
  # Code obtained from: https://r-charts.com/part-whole/pie-chart-labels-outside-ggplot2/
  mutate(csum = rev(cumsum(rev(perc))), 
        pos = perc/2 + lead(csum, 1),
        pos = if_else(is.na(pos), perc/2, pos))

return(plotData)
}

################################################################################
# Process data for pie charts
gulf20Plot = piePrep(gulf20)
gulf21Plot = piePrep(gulf21)
mar21Plot = piePrep(mar21)
nl20Plot = piePrep(nl20)
nl21Plot = piePrep(nl21)
pac20Plot = piePrep(pac20)
pacJun21Plot = piePrep(pacJun21)
pacMar21Plot = piePrep(pacMar21)
pacSep21Plot = piePrep(pacSep21)

################################################################################
# Create function for making pie charts!

piePlot = function(pieData) {
  
  # Make the pie chart
  ggplot(pieData, aes(x="", y=perc, fill=classNew)) +
    geom_bar(stat="identity") +
    geom_col(color = "black")+ # add black border around slices
    coord_polar("y", start=0)+ # make it a pie chart
    scale_fill_brewer(palette = "Set2")+
    # For labelling percents within pie slices:
    # geom_text(aes(label = round(perc, digits=2)),
    #           position = position_stack(vjust = 0.5),
    #           size = 3) +
    geom_label_repel(data = pieData,
                     aes(y = pos, label = paste0(round(perc,1), "%")),
                     size = 3, nudge_x = 1, show.legend = FALSE)+
    #ggtitle("Maritimes 2021")+ # fix this to be automated
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = element_blank(),
      title=element_blank())
}

# Plot these
piePlot(gulf20Plot)
piePlot(gulf21Plot)
piePlot(mar21Plot)
piePlot(nl20Plot)
piePlot(nl21Plot)
piePlot(pac20Plot)
piePlot(pacJun21Plot)
piePlot(pacMar21Plot)
piePlot(pacSep21Plot)



# Pie chart for one station

# Bar chart of one station
ggplot(gulf20, aes(x=class, y=sumCount, fill=class)) +
  geom_bar(stat="identity", width=1) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())






# Stacked bar chart
ggplot(marBothReduced, aes(x=sample, y=as.numeric(count), fill=class)) +
  geom_bar(stat="identity", width=1) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")




###### THINGS I WILL PROBABLY DELETE BUT I'M TOO SCARED TO DELETE


################################################################################
## Process the Newfoundland 2020 data separately
# For some reason the csvs are in a different format 

# Make an empty list to store the data
nl20Datalist = list()

# Loop through all the data and extract the class (plankton taxa), count (# of 
# cells in the sample), and particles (# cells/ml)
# The 4 refers to the Newfoundland 2020 data (it's 4th in the list)
for(i in 1:length(dirFull[[4]])) {
  
  # Read in the files (it's a list of lists)
  # 4 denotes Newfoundland data, and i denotes which file to read in
  data = read.csv(dirFull[[4]][i], skip = 2) %>% 
    # Keep everything before the "End Metadata Statistics" part (remove that and after)
    filter(row_number() < which(Name =='======== End Metadata Statistics ========')) %>%
    # Rename the columns to match the other data files
    # format: new = cold
    rename(class = Name, count = Count, particles = Particles...ml)
  
  # Add each file to the list. Each bit of data will be stored as a list (within the list)
  nl20Datalist[[i]] = data
}






# Get data from one station to just check graphs
oneStation = subset(marBothReduced, sample == "21_08_24_Mar_S04_Z01_1053_250")

# Graphs still try and show classes with 0 counts
# It's easiest to just remove these from the dataframe
# This part can actually be removed (but i'll keep it for later)
# oneStation = 
#   oneStation %>% 
#   dplyr::na_if(0) 
# oneStation = na.omit(oneStation)



## For doing NMDS/things where I need the water volume
marCounts = marBoth[,c(1:3)]
marParticles = marBoth[,c(1,2,4)]

#pivot the data frame into a wide format

# RENAME THESE AND ALSO ALPHABETIZE THE SPECIES LIST

x = marCounts %>% pivot_wider(names_from = class, values_from = count)
y = marParticles %>% pivot_wider(names_from = class, values_from = particles)

# names_from: The column whose values will be used as column names
# values_from: The column whose values will be used as cell values




#### convert particles/ml (flowcam sample) to ind m^-3 (seawater)

# density = (n)(Vs)/(Vm)
# n = average number of orgs in 1 ml subsample
# Vs = volume of plankton sample (ml) (i.e. bottle volume!!)
# Vm = volume of seawater sampled by the net (m3)

# double check: is there a l --> m^3 conversion for seawater?

marZooCondensed = marZoo[,c("sampleCode","tideRange", "waterVolume")] 



merge(marZooCondensed, marParticles, by = "sampleCode", all = TRUE)



# list all xlsx files in the directory
nl_data <-
  list.files(
    "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data",
    full.names = TRUE,
    pattern = ".xlsx"
  )

# Setup a blank dataframe to input data to
all_data <- read_xlsx(nl_data[1], n_max = 1) %>%
  top_n(0) %>%
  mutate(file = character()) %>%
  select(file, everything())

# loop through files, reading each one, adding a row of the file, and then joining to `all_data`
for (i in nl_data) {
  raw <- read_xlsx(i) %>%
    mutate(file = basename(i)) %>%
    select(file, everything()) %>%
    mutate(file = str_remove(file, ".xlsx"))
  
  message(paste(i, nrow(raw)))
  
  all_data <- raw %>%
    bind_rows(all_data)
}

write_csv(all_data,
          "C:/Users/kraskape/Desktop/zooplankton_allData.csv")

# Summary files
nl_summary <-
  list.files(
    "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data/AMMP Gulf 2020 Zooplankton Data/Zooplankton Identification Data/Classification Data",
    full.names = TRUE,
    pattern = ".csv"
  )

# Setup a blank dataframe to input data to
summary_file <- read.csv(nl_summary[1], col_names = FALSE)

# metadata statistics
all_metadataStatistics <- summary_file %>%
  slice((grep(pattern = "======== Metadata Statistics ========", x = .$...1) +
           3):grep(pattern = "======== End Metadata Statistics ========", x = .$...1) -
          1) %>%
  select(1:8)

colnames(all_metadataStatistics) <- all_metadataStatistics[1,]

all_metadataStatistics <- all_metadataStatistics %>%
  top_n(0) %>%
  mutate(file = character()) %>%
  select(file, everything())

# loop through files, reading each one, adding a row of the file, and then joining to `all_data`
for (i in nl_summary) {
  raw_met <- read_xlsx(i, col_names = FALSE) %>%
    slice((grep("======== Metadata Statistics ========", x = .$...1) + 2):(grep("======== End Metadata Statistics ========", x = .$...1) - 1)) %>%
    select(1:8) %>%
    slice(-1) %>%
    mutate(file = basename(i)) %>%
    select(file, everything()) %>%
    mutate(file = str_remove(file, ".xlsx"))
  
  colnames(raw_met) <- colnames(all_metadataStatistics)
  
  all_metadataStatistics <- all_metadataStatistics %>%
    bind_rows(raw_met)
}

write_csv(all_metadataStatistics, "C:/Users/kraskape/Desktop/all_metadataStatistics.csv")

# Particle statistics
all_particleStatistics <- summary_file %>%
  slice((
    grep(pattern = "======== Particle Property Statistics ========", x = .$...1) +
      2
  ):(
    grep(pattern = "======== End Particle Property Statistics ========", x = .$...1) - 1
  ))

colnames(all_particleStatistics) <- all_particleStatistics[1,]

all_particleStatistics <- all_particleStatistics %>%
  top_n(0) %>%
  mutate(file = character()) %>%
  select(file, everything())

# loop through files, reading each one, adding a row of the file, and then joining to `all_data`
for (i in nl_summary) {
  raw_par <- read_xlsx(i, col_names = FALSE) %>%
    slice((
      grep(pattern = "======== Particle Property Statistics ========", x = .$...1) +
        2
    ):(
      grep(pattern = "======== End Particle Property Statistics ========", x = .$...1) - 1
    )) %>%
    slice(-1) %>%
    mutate(file = basename(i)) %>%
    select(file, everything()) %>%
    mutate(file = str_remove(file, ".xlsx"))
  
  colnames(raw_par) <- colnames(all_particleStatistics)
  
  all_particleStatistics <- all_particleStatistics %>%
    bind_rows(raw_par)
}
#write_csv(all_particleStatistics, "C:/Users/kraskape/Desktop/all_particleStatistics.csv")
