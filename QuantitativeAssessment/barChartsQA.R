################################################################################
################################################################################
##### Bar charts

# Create bar charts that compare the FlowCam and Quantitative Assessment data
# For now, I'm making both stacked bar charts and relative abundance charts

# Created by Stephen Finnis

################################################################################

# Read in the script that puts together the QA (microscopy) and Flowcam data
source("QuantitativeAssessment/QAcodeMatches.R")


################################################################################

# Create a function that creates stacked bar charts and relative abundance charts
# A data with the data for each region/year (i.e., 10 samples) is passed in 

# Will create 4 plots total:
# 1. Upper left: QA stacked bar chart
# 2. Upper right: QA rel abundance chart
# 3. Lower left: FC stacked bar chart
# 4. Lower right: FC rel abundance

# It was easiest to create all 4 in one function and then use ggarrange to put them together
barChart = function(qaData, fcData){


  ### Quantitative Assessment data
  
  # Find the __ most abundant taxa. Label all others as "Other"
  # Otherwise there are too many legend items
  datRankedQA = qaData %>%
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(newName) %>%
    summarize(countTotals = sum(count)) %>%
    mutate(rank = rank(-countTotals),
           # Keep 5 most abundant classes, make the rest "Other"
           classRanks = ifelse(rank <=8, newName, "Other")) %>%
    mutate(relAbund = countTotals/sum(countTotals)) # if i want the relative abundance
  
  # Add this these new classes as a column in the original data frame for plotting
  datPlotQA = datRankedQA %>%
    left_join(qaData) %>% 
    group_by(classRanks, FlowCamID) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(count))
  
  # Create stacked bar chart
  stackedBarQA = 
    ggplot()+
    geom_bar(datPlotQA, mapping = aes(x = FlowCamID, y = sumCount, fill = classRanks), col = "black", linewidth = 0.05, stat = "identity")+
    scale_y_continuous(name = "Count")+
    scale_x_discrete(name = "Sample")+
    scale_fill_brewer(palette = "Set3", name = "")+
    labs(title = "Quantitative Assessment Data")+
    theme_bw()+
    theme(
      axis.text.x = element_blank(),
      # axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank())
  
  # Relative abundance chart
  relAbundQA =
    ggplot() +
    geom_bar(datPlotQA, mapping = aes(x=FlowCamID, y=sumCount, fill=classRanks), position = "fill", stat = "identity", col = "black", linewidth = 0.05) +
    scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
    scale_x_discrete(name = "Sample")+
    scale_fill_brewer(palette = "Set3", name = "")+
    theme_bw()+
    theme(
      axis.text.x = element_blank(),
      # axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      #panel.grid.major.y = element_blank(),
    )

  
  ### FlowCam Data!
  
  # Find the __ most abundant taxa. Label all others as "Other"
  # Otherwise there are too many legend items
  datRankedFC = fcData %>%
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(newName) %>%
    summarize(countTotals = sum(count)) %>%
    mutate(rank = rank(-countTotals),
           # Keep 5 most abundant classes, make the rest "Other"
           classRanks = ifelse(rank <=8, newName, "Other")) %>%
    mutate(relAbund = countTotals/sum(countTotals)) # if i want the relative abundance
  
  # Add this these new classes as a column in the original data frame for plotting
  datPlotFC = datRankedFC %>%
    left_join(fcData) %>% 
    group_by(classRanks, FlowCamID) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(count))
  
  # Create stacked bar chart
  stackedBarFC = 
    ggplot()+
    geom_bar(datPlotFC, mapping = aes(x = FlowCamID, y = sumCount, fill = classRanks), col = "black", linewidth = 0.05, stat = "identity")+
    scale_y_continuous(name = "Count")+
    scale_x_discrete(name = "Sample")+
    scale_fill_brewer(palette = "Set3", name = "")+
    ggtitle("FlowCam Data")+
    theme_bw()+
    theme(
      axis.text.x = element_blank(),
      # axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank())
  
  # Relative abundance chart
  relAbundFC =
    ggplot() +
    geom_bar(datPlotFC, mapping = aes(x=FlowCamID, y=sumCount, fill=classRanks), position = "fill", stat = "identity", col = "black", linewidth = 0.05) +
    scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
    scale_x_discrete(name = "Sample")+
    scale_fill_brewer(palette = "Set3", name = "")+
    theme_bw()+
    theme(
      axis.text.x = element_blank(),
      
      # axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      #panel.grid.major.y = element_blank(),
    )
  

  # Put everything together
  # ggarrange makes plots line up even when legends are different sizes
  ggarrange(stackedBarQA, relAbundQA, stackedBarFC, relAbundFC, ncol = 2)
  # test = list(stackedBar, relAbund)
}


# Create the charts. Pass in QA data (first item) and then Flowcam data (second)
gulf20Charts = barChart(allQAData %>% subset(regionYear == "Gulf 2020"), flowCamData %>% subset(regionYear == "Gulf 2020"))
pac21Charts = barChart(allQAData %>% subset(regionYear == "Pac 21"), flowCamData %>% subset(regionYear == "Pac 21"))
nl20Charts = barChart(allQAData %>% subset(regionYear == "NL 2020"), flowCamData %>% subset(regionYear == "NL 2020"))
nl21Charts = barChart(allQAData %>% subset(regionYear == "NL 2021"), flowCamData %>% subset(regionYear == "NL 2021"))

