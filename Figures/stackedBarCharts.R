################################################################################
# Stacked bar charts
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up
source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################


stackedBarChart = function(bayData){
  
  # Find the __ most abundant taxa. Label all others as "Other"
  # Otherwise there are too many legend items
  bayOther = bayData %>%
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(class) %>%
    summarize(countPerClass = sum(abund)) %>%
    mutate(rank = rank(-countPerClass),
           # Keep 4 most abundant classes, make the rest "Other"
           classNew = ifelse(rank <=4, class, "Other"))
  
  # Add this these new classes as a column in the original dataframe
  bayPlotDf = bayData %>%
    left_join(bayOther %>%
                # Might be a better way, but I don't want to join the ENTIRE dataframe
                select(classNew, class, countPerClass), by = c("class" = "class")) %>%
    group_by(classNew, sample, myLabel) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(abund))
  
  
  # Make ggplot for a Stacked Bar Chart
  # Write the y-axis label outside of ggplot so I don't get (as) lost in brackets
  yLabelStacked = expression(paste("Abundance (ind ", m^{-3}, ")"))
  
  stackedGGPlot =
    ggplot(bayPlotDf, aes(x=sample, y=sumCount, fill=classNew)) +
      geom_bar(stat="identity", color = "black")+
      facet_grid(cols = vars(myLabel), scales = "free_x", space = "free_x")+
      scale_x_discrete(name = "Station")+
      scale_y_continuous(labels = scales::comma, name = yLabelStacked) +
      scale_fill_brewer(palette = "Accent", name = "Zooplankton Class")+
      #theme_minimal(base_family = "Roboto Condensed") +
      theme_bw()+
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
            plot.title = element_text(size = 15, face = "bold"),
            #strip.text.x = element_text(angle = 270, face = "bold"),
            strip.placement = "outside",
            #axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
            #axis.title.y = element_blank(),
            #axis.text.x = element_text(angle = 90),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 11),
            axis.text = element_text(size = 14),
            #legend.position = "none",
            legend.title = element_text(size=13),
            panel.grid.major.y = element_blank(),
            panel.spacing = unit(0.5, "cm"),
            strip.text.x = element_text(size = 15)
      )
  
  # Make ggplot for a relative abundance chart
  relGGPlot = 
    ggplot(bayPlotDf, aes(x=sample, y=sumCount, fill=classNew)) +
      geom_bar(stat = "identity", position = "fill", col = "black") +
      scale_y_continuous(labels = percent_format(), name = "Relative Abundance")+
      facet_grid(cols = vars(myLabel), scales = "free_x", space = "free_x")+
      scale_x_discrete(name = "Station")+
      scale_fill_brewer(palette = "Accent", name = "Zooplankton Class")+
      #theme_minimal(base_family = "Roboto Condensed") +
      theme_bw()+
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
            plot.title = element_text(size = 15, face = "bold"),
            #strip.text.x = element_text(angle = 270, face = "bold"),
            strip.placement = "outside",
            #axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
            #axis.title.y = element_blank(),
            #axis.text.x = element_text(angle = 90),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 11),
            axis.text = element_text(size = 14),
            #legend.position = "none",
            legend.title = element_text(size=13),
            panel.grid.major.y = element_blank(),
            panel.spacing = unit(0.5, "cm"),
            strip.text.x = element_text(size = 15)
      )
  
  # return as list so you get both the ggplot and processed data
  returnList = list(bayPlotDf, stackedGGPlot, relGGPlot)
  
  return(returnList)
  
}

################################################################################


# Process them
# Maritimes
argyleProcess = stackedBarChart(marMerge %>% subset(facilityName == "Argyle"))
soberProcess = stackedBarChart(marMerge %>% subset(facilityName == "Sober Island Oyster"))
whiteheadProcess = stackedBarChart(marMerge %>% subset(facilityName == "WhiteHead"))
cHarbourProcess = stackedBarChart(marMerge %>% subset(facilityName == "CountryHarbour"))

# Gulf
malpequeProcess = stackedBarChart(gulfMerge %>% subset(facilityName=="Malpeque"))
cocagneProcess = stackedBarChart(gulfMerge %>% subset(facilityName=="Cocagne"))
stPetersProcess = stackedBarChart(gulfMerge %>% subset(facilityName=="StPeters"))

# Newfoundland (only one bay)
seArmProcess = stackedBarChart(nlMerge)

# Pacific (only one bay, but separate by dataset instead)
lemmens20Process = stackedBarChart(pacMerge %>% subset(dataset == "Pacific 2020"))
lemmensMar21Process = stackedBarChart(pacMerge %>% subset(dataset == "Pacific March 2021"))
lemmensJun21Process = stackedBarChart(pacMerge %>% subset(dataset == "Pacific June 2021"))
lemmensSept21Process = stackedBarChart(pacMerge %>% subset(dataset == "Pacific September 2021"))


################################################################################


## View stacked bar charts!
# Maritimes
argyleProcess[[2]]
soberProcess[[2]]
whiteheadProcess[[2]]
cHarbourProcess[[2]]

# Gulf. Note: these have unusually hi/lo values?
malpequeProcess[[2]]
cocagneProcess[[2]]
stPetersProcess[[2]]

# Newfoundland
seArmProcess[[2]]

# Pacific
lemmens20Process[[2]]
lemmensMar21Process[[2]]
lemmensJun21Process[[2]]
lemmensSept21Process[[2]]

################################################################################

## View relative abundance charts
# Maritimes
argyleProcess[[3]]
soberProcess[[3]]
whiteheadProcess[[3]]
cHarbourProcess[[3]]

# Gulf
malpequeProcess[[3]]
cocagneProcess[[3]]
stPetersProcess[[3]]
