################################################################################
# AMP making different charts with the zooplankton data
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################
## Alter data format for creation of pie charts

pieChart = function(plotData) {

  ### PROCESS THE DATA
  
  # For Maritimes & Gulf, I have abundances ("abund")! 
  # For all others, I do not. Just the adjusted count (/% cleaned etc)
  if("abund" %in% colnames(plotData))
  {
    plotData$count = plotData$abund
  } else {
    plotData$count = plotData$adjCount
  }
  
  # Test if it worked
  # print(plotData$count)
  
  # Do some cleaning of the data
  plotData = plotData %>% 
    
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(class) %>%
    summarize(countBay = sum(count)) %>%
    
    # Need to create an "Other" class so the pie charts don't have too many slices
    # Keep the 5 most abundant classes, name the rest "Other"
    mutate(rank = rank(-countBay), 
           classNew = ifelse(rank <= 7, class, 'Other')) %>%
    
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
  
  
  ### PLOT THE DATA
  g1 = 
    # Make the pie chart
    ggplot(plotData, aes(x="", y=perc, fill=classNew)) +
    geom_bar(stat="identity") +
    geom_col(color = "black")+ # add black border around slices
    coord_polar("y", start=0)+ # make it a pie chart
    scale_fill_brewer(palette = "Set2")+
    # For labelling percents within pie slices:
    # geom_text(aes(label = round(perc, digits=2)),
    #           position = position_stack(vjust = 0.5),
    #           size = 3) +
    geom_label_repel(data = plotData,
                     aes(y = pos, label = paste0(round(perc,1), "%")),
                     size = 5, nudge_x = 1, show.legend = FALSE)+
    #ggtitle("Maritimes 2021")+ # fix this to be automated
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.text=element_text(size = 14),
      #panel.border = element_rect(colour = "black", fill=NA, size=5),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = element_blank(),
      title=element_blank())

  # return as list so you get both the ggplot and processed data (for making other charts)
  returnList = list(g1, plotData)
  
# Return the ggplot (grob?)
return(returnList)
}

################################################################################
# Break up data by bay/inlet
# Maritimes: Argyle, Sober Island, Country Harbour, Whitehead
# Gulf: Cocagne, Malpeque, St. Peters
# Newfoundland: Southeast Arm
# Pacific: Lemmens

# For Maritimes and Gulf I have true abundances! Since they have been corrected by water volume


# Maritimes
argyle = marMerge %>%
  subset(facilityName=="Argyle")

sober = marMerge %>%
  subset(facilityName == "Sober Island Oyster")

whitehead = marMerge %>%
  subset(facilityName == "WhiteHead")

cHarbour = marMerge %>%
  subset(facilityName == "Country Harbour")

# Gulf
malpeque = gulfMerge %>%
  subset(facilityName == "Malpeque")

stPeters = gulfMerge %>%
  subset(facilityName == "StPeters")

cocagne = gulfMerge %>%
  subset(facilityName == "Cocagne")

# Newfoundland
seArm = rbind(nl20Adj, nl21Adj)

# Pacific (here, I'm combining all of it)
lemmens = rbind(pac20Adj, pacMar21Adj, pacJun21Adj, pacSept21Adj)


################################################################################
# Make the pie charts! These won't show up until you type the variable name
argylePieChart = pieChart(argyle)
soberPieChart = pieChart(sober)
whiteheadPieChart = pieChart(whitehead)
cHarbourPieChart = pieChart(cHarbour)
malpequePieChart = pieChart(malpeque)
stPetersPieChart = pieChart(stPeters)
cocagnePieChart = pieChart(cocagne)
seArmPieChart = pieChart(seArm)
lemmensPieChart = pieChart(lemmens)

################################################################################

first = grid.arrange(argylePieChart[[1]], soberPieChart[[1]], ncol = 2)
second = grid.arrange(cHarbourPieChart[[1]], whiteheadPieChart[[1]], ncol = 2)
third = grid.arrange(cocagnePieChart[[1]], malpequePieChart[[1]], ncol = 2)
fourth = grid.arrange(stPetersPieChart[[1]], seArmPieChart[[1]], ncol = 2)
fifth = grid.arrange(lemmensPieChart[[1]], lemmensPieChart[[1]], ncol = 2)

# ggsave("first.png", first)
# ggsave("second.png", second)
# ggsave("third.png", third)
# ggsave("fourth.png", fourth)
# ggsave("fifth.png",fifth)

###### THINGS I WILL PROBABLY DELETE BUT I'M TOO SCARED TO DELETE

argylePie[2]

ggplot(argylePieChart[[2]], aes(x="", y=perc, fill=classNew)) +
  geom_bar(stat="identity")

# Pie chart for one station

# Bar chart of one station
ggplot(gulf20, aes(x=class, y=count, fill=class)) +
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

# RESTRUCTURING DATASETS:
# Convert data into better format
# Each row is a station, each column is a class. Each cell represents the counts. 
mar21New = 
  mar21Adj %>%
  select(-c(count, particles, PercSampleCleaned, PercZooIdentified)) %>% # Remove this column
  # Modify the data frame into wide format
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  # COMMENT THESE OUT BUT USE LATER:
  # pivot_wider(names_from = class, values_from = adjCount) %>%
  # mutate_all(~replace(.,is.na(.), 0)) # replace NAs with 0
  

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
