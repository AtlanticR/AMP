################################################################################
# Stacked bar charts
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up

source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################
## Alter data format for creation of pie charts

stackedBarChart = function(plotData) {
  
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
  
  # Return the ggplot (grob?)
  return(plotData)
}

################################################################################
### I THINK I SHOULD MOVE THIS TO ITS OWN FILE OR ADD TO ZOOPLANKTONCOUNTS.R
# Just keeping this here for now so I don't mess it up!

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


ggplot(sober, aes(x=sample, y=abund, fill=class)) +
  geom_bar(stat="identity")
