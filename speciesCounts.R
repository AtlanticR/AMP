# Finnis Testing File Stuff


library(dplyr)
library(stringr)


file = read.csv("C:/Users/FINNISS/Desktop/TEST_AMMP_Gulf_Malpeque_475326_20200929_250UM_R2.csv", header=F)

class = file[which(str_detect(file[,1], "Class$")), ] 
count = file[which(str_detect(file[,1], "Count")), ]
particles = file[which(str_detect(file[,1], "Particles")), ]

test = as.data.frame(cbind("class" = class[,2], "count" = count[,2], "particles" = particles[,2]))








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
