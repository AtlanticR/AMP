################################################################################
################################################################################
##### Bar chart figures

# Create the code for different bar charts of FlowCam (FC) /microscopy 
# (QA = quantitative assessment) comparisons

# The first ones are comparisons of every sample between the FC/QA data
# Shows relative abundance and stacked bar charts with breakdown of plankton within each sample

# Next ones show side by side bar charts of each taxa (as average relative abundance), 
# and how those differ between regions.

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
barChart = function(comboData){
  
  ### FlowCam Data!
  
  # Find the __ most abundant taxa. Label all others as "Other"
  # Otherwise there are too many legend items
  datRanked = comboData %>%
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(newName) %>%
    summarize(countTotals = sum(count)) %>%
    mutate(rank = rank(-countTotals),
           # Keep 5 most abundant classes, make the rest "Other"
           classRanks = ifelse(rank <=11, newName, "Other")) %>%
    mutate(relAbund = countTotals/sum(countTotals)) # if i want the relative abundance
  
  # Add this these new classes as a column in the original data frame for plotting
  datPlot = datRanked %>%
    left_join(comboData) %>% 
    group_by(classRanks, FlowCamID, type) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(count),
              abund = sum(abund),
              countTot = sum(countTot))
  
  datPlot$FlowCamID = as.factor(datPlot$FlowCamID)
  levels(datPlot$FlowCamID) = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")

  
  # Create stacked bar chart
  stackedBar =
    ggplot()+
    geom_bar(datPlot, mapping = aes(x = type, y = sumCount, fill = classRanks), col = "black", linewidth = 0.05, stat = "identity")+
    facet_grid(.~FlowCamID)+
    scale_y_continuous(name = "Count")+
    scale_x_discrete(name = "Sample")+
    scale_fill_brewer(palette = "Set3", name = "")+
    ggtitle("Number of individuals ID'd by taxonomists")+
    theme_bw()+
    theme(
      axis.title.x = element_blank()
      )+
    guides(fill = guide_legend(ncol=2))
  
  # Relative abundance chart
  relAbund =
    ggplot() +
    geom_bar(datPlot, mapping = aes(x=type, y=abund, fill=classRanks), position = "fill", stat = "identity", col = "black", linewidth = 0.05) +
    facet_grid(.~FlowCamID)+
    scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
    scale_x_discrete(name = "Sample")+
    ggtitle("Relative abundance")+
    scale_fill_brewer(palette = "Set3", name = "")+
    theme_bw()+
    theme(
      axis.title.x = element_blank()
    )+
    guides(fill = guide_legend(ncol=2))
  
  
  abundPlot = 
    ggplot()+
    geom_col(data = datPlot, aes(x=type, y=abund, fill=classRanks), col = "black", linewidth = 0.05) +
    facet_grid(.~FlowCamID)+
    scale_fill_brewer(palette = "Set3", name = "")+
    ggtitle("Abundance in seawater")+
    scale_x_discrete(name = "Sample")+
    theme_bw()+
    theme(
      axis.title.x = element_blank()
      
      
    )+
    guides(fill = guide_legend(ncol=2))
  
  countTotPlot = 
    ggplot()+
    geom_col(data = datPlot, aes(x=type, y= countTot, fill=classRanks), col = "black", linewidth = 0.05) +
    facet_grid(.~FlowCamID)+
    ggtitle("Number of individuals in subsample (1/4 of tow)")+
    scale_y_continuous(name = "Count")+
    scale_fill_brewer(palette = "Set3", name = "")+
    #scale_x_discrete(name = "Sample")+
    theme_bw()+
    theme(
      axis.title.x = element_blank()
      )+
    guides(fill = guide_legend(ncol=2))
  
  
  # Put everything together
  # ggarrange makes plots line up even when legends are different sizes
  ggarrange(stackedBar, countTotPlot, relAbund)

}

comboDat = rbind(allQAData, flowCamData)

#levels(as.factor(comboDat$FlowCamID))


# Create the charts. Pass in QA data (first item) and then Flowcam data (second)
gulf20Charts = barChart(comboDat %>% subset(regionYear == "Gulf 2020"))
pac21Charts = barChart(comboDat %>% subset(regionYear == "Pac 21"))
nl20Charts = barChart(comboDat %>% subset(regionYear == "NL 2020"))
nl21Charts = barChart(comboDat %>% subset(regionYear == "NL 2021"))

################################################################################
################################################################################
# Now compare abundances in seawater


test =  allQAData %>% subset(regionYear == "Pac 21")
test2 = flowCamData %>% subset(regionYear == "Gulf 2020")



test3 = rbind(test, test2)



g1 = 
ggplot()+
  geom_col(data = test3, aes(x=type, y=abund, fill=newName)) + 
  facet_grid(.~FlowCamID)

g2 = 
  ggplot()+
  geom_col(data = test3, aes(x=type, y=countTot, fill=newName)) + 
  facet_grid(.~FlowCamID)



ggarrange(g1, g2)


################################################################################
################################################################################
## Plots of taxa comparisons
# Create bar charts that show the average relative abundance for each taxa
# Have FlowCam data beside the Microscopy data

# Combine the flowcam and microscopy datasets. Label each row as QA or FC
fcQaData = rbind(allQAData %>% mutate(type = "QA"), flowCamData %>% mutate(type = "FC"))

# Get summary stats for plotting
# I want the average relative abundance for each sample in each region
sampleSummary = fcQaData %>%
  # Get the relative abundance for each sample
  group_by(FlowCamID, type) %>%
  mutate(relAbund = count / sum(count)*100) %>%
  # Now get the AVERAGE relative abundance for each sample. and the standard deviation
  group_by(newName, type, regionYear) %>%
  mutate(meanAbund = mean(relAbund),
         sdAbund = sd(relAbund)) %>%
  select(newName, regionYear, meanAbund, sdAbund, type) %>%
  distinct() 

# I want to know, for each regionYear, which taxa are found: in both flowcam and microsopy, flowcam only, and microscopy only
# It is very annoying to do lol
sampleSummaryAdj = sampleSummary %>%
  # First need to expand the list, so it has ALL comparisons 
  # Because previously, only taxa that are present are recorded for each regionYear/type
  # But if, e.g., Calanoida (unid) only present in FlowCam but not in microscopy, I need a new line of data for that too
  ungroup() %>%
  complete(newName, regionYear, type, fill = list(meanAbund =0, sdAbund = 0)) %>%

  # Now do the comparisons.
  group_by(newName, regionYear) %>%
  mutate(presence = case_when(
    all(meanAbund == 0) ~ "neither", # Not present in either
    any(meanAbund >0) & all(meanAbund >0) ~ "Both", # present in both
    meanAbund[type == "QA"] == 0 & meanAbund[type == "FC"] > 0 ~ "FC Only", # present in flowcam only
    meanAbund[type == "QA"] > 0 & meanAbund[type == "FC"] == 0 ~ "MC Only" # present in microscopy only
  )) 



# Create a function to make bar chart comparisons for each region
barChartComparison = function(relData, regionYear, plotName) {
  
  # Don't include the data that are present in neither
  # That would include taxa that are present in other region years, but not these ones
  relData = relData %>%
    filter(presence != "neither")
  
  # Make a plot that has 3 panels: present in both, present in flowcam only, present in microscopy only
  ggplot(relData, aes(x = meanAbund, y = newName, fill = type))+
    # geom_errorbar(aes(xmin=meanAbund, xmax=meanAbund+sdAbund), width=.2,
    #               position=position_dodge(.8), col = "black")+
    geom_col(position = position_dodge2(preserve = "single"), width = 0.8)+
    #facet_wrap(~presence, scales = "free_y", ncol = 1)+
    # facet_grid seems better for getting bar width the same, but panel sizes to be different
    facet_grid(presence~., scales = "free", space = "free", switch = "y")+ 
    xlab("Average relative abundance (%)")+
    ggtitle(plotName)+
    scale_fill_discrete(labels=c('FlowCam', 'Microscopy'))+
    scale_color_discrete(labels = c("FlowCam", "Microscopy"))+
    theme_bw()+
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title = element_blank(),
      strip.text.y = element_text(size = 10)
      )
  
}
  
# Create the bar charts for each region separately
barChartComparison(sampleSummaryAdj %>% filter(regionYear == "NL 2020"), "NL 2020", "Newfoundland 2020")
barChartComparison(sampleSummaryAdj %>% filter(regionYear == "NL 2021"), "NL 2021", "Newfoundland 2021")
barChartComparison(sampleSummaryAdj %>% filter(regionYear == "Pac 21"), "Pac 21", "Pacific 2021") 
barChartComparison(sampleSummaryAdj %>% filter(regionYear == "Gulf 2020"), "Gulf 2020", "Gulf 2020") 


