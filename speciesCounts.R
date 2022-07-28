################################################################################
# Finnis Testing File Stuff


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
packages = c("dplyr", "ggplot2", "leaflet", "mapr", "mapview", "readxl", "stringr",
             "tidyr", "tools")
ipak(packages)

################################################################################
## Set up the data

# List all csv files in the directory (full directory name)
marDataFull =
  list.files(
    "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data/AMMP Maritimes 2021 Zooplankton Data/Zooplankton Identification Data/Classification summary",
    full.names = T, # don't want full directory names
    pattern = ".csv"
  )

# List them all without the full directory name (it's useful for later)
marDataShort =
  list.files(
    "C:/Users/FINNISS/Desktop/AMMP FlowCam Zooplankton Data/AMMP Maritimes 2021 Zooplankton Data/Zooplankton Identification Data/Classification summary",
    full.names = F, # don't want full directory names
    pattern = ".csv"
  )

# Remove the file extension 
marDataShort = sub('\\.csv$', '', marDataShort) 

################################################################################
## Create dataframes with data

# Make an empty list to store the data
marDatalist = list()

# Loop through all the data and extract the class (plankton taxa), count (# of 
# cells in the sample), and particles (# cells/ml)
for(i in 1:length(marDataFull)){
  
  # Read in the files
  data = read.csv(marDataFull[i]) 
  
  # Extract full row of data which contain class, count and particle information 
  class = data[which(str_detect(data[,1], "Class$")), ] 
  count = data[which(str_detect(data[,1], "Count")), ]
  particles = data[which(str_detect(data[,1], "Particles")), ]
  
  # Combine this all into a dataframe
  # Don't want the full row, we only want the 2nd column with the actual data
  # Label it with the file name (without directory/extension)
  df = as.data.frame(cbind("sample" = marDataShort[i],
    "class" = class[,2], "count" = count[,2], "particles" = particles[,2]))
  
  # Add this to the list of dataframes (there are many alternative methods)
  marDatalist[[i]] = df
}

# Bind together this list of dataframes into one big data frame (both count/particles)
marboth = dplyr::bind_rows(marDatalist)

marCounts = marboth[,c(1:3)]
marParticles = marboth[,c(1,2,4)]

#pivot the data frame into a wide format

# RENAME THESE AND ALSO ALPHABETIZE THE SPECIES LIST

x = marCounts %>% pivot_wider(names_from = class, values_from = count)
y = marParticles %>% pivot_wider(names_from = class, values_from = particles)

# names_from: The column whose values will be used as column names
# values_from: The column whose values will be used as cell values











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
