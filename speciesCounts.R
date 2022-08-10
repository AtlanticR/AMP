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
             "tidyr", "tools", "vegan")
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
  
  # Add this to the list of dataframes (there are many alternative methods)
  datalist[[i]] = df
}

# Bind together this list of dataframes into one big data frame (both count/particles)
siteDf = dplyr::bind_rows(datalist)

# Convert counts to numeric
siteDf$count = as.numeric(siteDf$count)

return(siteDf)
}

# Run the function above and create the dataframes for each
gulf20 = speciesDF(dirFull[[1]], dirShort[[1]])
gulf21 = speciesDF(dirFull[[2]], dirShort[[2]])
mar21 = speciesDF(dirFull[[3]], dirShort[[3]])
nl20 = speciesDF(dirFull[[4]], dirShort[[4]])
nl21 = speciesDF(dirFull[[5]], dirShort[[5]])
pac20 = speciesDF(dirFull[[6]], dirShort[[6]])
pacJun21 = speciesDF(dirFull[[7]], dirShort[[7]])
pacMar21 = speciesDF(dirFull[[8]], dirShort[[8]])
pacSep21 = speciesDF(dirFull[[9]], dirShort[[9]])




#### DELETE A LOT OF THIS LATER:

# Bind together this list of dataframes into one big data frame (both count/particles)
marBoth = dplyr::bind_rows(datalist)

# Convert counts to numeric
marBoth$count = as.numeric(marBoth$count)


## DELETE ABOVE


# Do some cleaning of the data
marBoth = marBoth %>% 
  
  # Remove unnecessary classes 
  subset(!grepl("[0-9]", class) & # remove the "Class 1-9" data
           class != "Leftovers" &
           class != "Extra taxa" &
           class != "extra taxa" &
           class != "Extra Taxa") %>% # Remove "Leftovers" class (CHECK THIS)
    
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
  mutate(perc = sumCount / sum(sumCount)*100)





maritimes = speciesDF(dirFull[[1]], dirShort[[1]])
gulf = speciesDF(dirFull[[2]], dirShort[[2]])

gulf2020 = dirFull[[1]]
gulf2021 = dirFull[[2]]
mar2021 = dirFull[[3]]
nl2020 = dirFull[[4]]
nl2021 = dirFull[[5]]
pac2020 = dirFull[[6]]
pacJun2021 = dirFull[[7]]
pacMar2021 = dirFull[[8]]
pacSep2021 = dirFull[[9]]




# Get the positions for the labels
labelPositions = marBoth %>% 
  mutate(csum = rev(cumsum(rev(perc))), 
         pos = perc/2 + lead(csum, 1),
         pos = if_else(is.na(pos), perc/2, pos))

# Make the pie chart
ggplot(marBoth, aes(x="", y=perc, fill=classNew)) +
  geom_bar(stat="identity") +
  geom_col(color = "black")+ # add black border around slices
  coord_polar("y", start=0)+ # make it a pie chart
  scale_fill_brewer(palette = "Set2")+
  # For labelling percents within pie slices:
  # geom_text(aes(label = round(perc, digits=2)),
  #           position = position_stack(vjust = 0.5),
  #           size = 3) +
  geom_label_repel(data = labelPositions,
                   aes(y = pos, label = paste0(round(perc,1), "%")),
                   size = 3, nudge_x = 1, show.legend = FALSE)+
  ggtitle("Maritimes 2021")+ # fix this to be automated
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = element_blank(),
    title=element_blank())


# Pie chart for one station

# Bar chart of one station
ggplot(marBoth, aes(x=class, y=sumCount, fill=class)) +
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
