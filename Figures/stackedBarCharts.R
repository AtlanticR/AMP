################################################################################
# Stacked bar charts
# Created by Stephen Finnis 2022
################################################################################

## Get things set up
source("DataProcessing/zooplanktonCounts.R")

# facet_nested is breaking for people? See here:
# https://github.com/teunbrand/ggh4x/issues/81
devtools::install_github("teunbrand/ggh4x")
library("ggh4x")

################################################################################
## Function to create stacked bar charts and relative abundance charts
# Takes the count data for each bay and returns:
# 1. Dataframe with 5 most abundant species and their counts
# 2. Stacked bar chart of this info (ggplot object)
# 3. Relative abundance chart of this info (ggplot object)

stackedBarChart = function(bayData, plotTitle){
  
  # Find the __ most abundant taxa. Label all others as "Other"
  # Otherwise there are too many legend items
  bayOther = bayData %>%
    # Want counts per taxa (class) for the whole bay, not by tow
    group_by(class) %>%
    summarize(countPerClass = sum(abund)) %>%
    mutate(rank = rank(-countPerClass),
           # Keep 5 most abundant classes, make the rest "Other"
           classNew = ifelse(rank <=7, class, "Other"))

  # Add this these new classes as a column in the original dataframe
  bayPlotDf = bayData %>%
    left_join(bayOther %>%
                # Might be a better way, but I don't want to join the ENTIRE dataframe
                select(classNew, class, countPerClass), by = c("class" = "class")) %>%
    group_by(classNew, sampleCode, myLabel, tidePhase) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(abund))

  # day 
  #bayData$sampleCode[7:8]
  
  # Rename a mid-rising/mid-falling so it shows up better
  bayPlotDf = bayPlotDf %>%
    #subset(tidePhase != "Mid-Rising" & tidePhase != "Mid-Falling") %>%
    mutate(tidePhase = replace(tidePhase, tidePhase == "Mid-Rising", "M-R")) %>%
    mutate(tidePhase = replace(tidePhase, tidePhase == "Mid-Falling", "M-F"))
  
  # Make ggplot for a relative abundance chart
  relGGPlot =
    ggplot(bayPlotDf, aes(x=sampleCode, y=sumCount, fill=classNew)) +
    geom_bar(stat = "identity", position = "fill", col = "black", linewidth = 0.05) +
    scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
    
    facet_nested(. ~myLabel + tidePhase, scales = "free_x", space = "free_x")+
    # facet_grid(cols = vars(myLabel), scales = "free_x", space = "free_x")+
    scale_x_discrete(name = "Site")+
    scale_fill_brewer(palette = "Set3", name = "Zooplankton Class")+
    ggtitle(plotTitle)+
    #theme_minimal(base_family = "Roboto Condensed") +
    theme_bw()+
    theme(
      #axis.text.x = element_text(angle = 90, size = 8), # use this if want station labels
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 13),
      axis.ticks.x = element_blank(),
      axis.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size=13),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(0.2, "cm"), # changes spacing between facets
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 13),
      strip.placement = "outside",
    )+
    guides(fill=guide_legend(ncol=2))
  
  return(relGGPlot)
  
}

################################################################################
## Process each dataset

# Break up the data by region. And also by bay.
# Need to type variable name to get plot to show up

# Maritimes
argyleProcess = stackedBarChart(marMerge %>% subset(facetFactor == "Argyle"), "(A) Argyle")
cHarbourProcess = stackedBarChart(marMerge %>% subset(facetFactor == "Country Harbour"), "(B) Country Harbour")
soberProcess = stackedBarChart(marMerge %>% subset(facetFactor == "Sober Island"), "(C) Sober Island")
whiteheadProcess = stackedBarChart(marMerge %>% subset(facetFactor == "Whitehead"), "(D) Whitehead")

ggarrange(argyleProcess, cHarbourProcess, soberProcess, whiteheadProcess, ncol = 1)


# Gulf
cocagneProcess = stackedBarChart(gulfMerge %>% subset(facetFactor=="Cocagne"), "(A) Cocagne")
malpequeProcess = stackedBarChart(gulfMerge %>% subset(facetFactor=="Malpeque"), "(B) Malpeque")
stPetersProcess = stackedBarChart(gulfMerge %>% subset(facetFactor=="St. Peters"), "(C) St. Peters")

# Will add dummy grobs to fill the extra space. Want all plots for all regions to be approx. same size
ggarrange(cocagneProcess, cocagneProcess, malpequeProcess, stPetersProcess, ncol = 1)

# Newfoundland (only one bay)
seArmSept2020 = stackedBarChart(nlMerge %>% subset(facetFactor == "Sept 2020"), "Sept 2020")
seArmOct2021 = stackedBarChart(nlMerge %>% subset(facetFactor == "Oct 2021"), "Oct 2021")
ggarrange(seArmSept2020, seArmOct2021, seArmOct2021, seArmOct2021, ncol = 1)


# Pacific (only one bay, but separate by facetFactor instead)
lemmens20Process = stackedBarChart(pacMerge %>% subset(facetFactor == "August 2020"), "(A) August 2020")
lemmensMar21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "March 2021"), "(B) March 2021")
lemmensJun21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "June 2021"), "(C) June 2021")
lemmensSept21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "September 2021"), "(D) September 2021")

ggarrange(lemmens20Process, lemmensMar21Process, lemmensJun21Process, lemmensSept21Process, ncol = 1)


################################################################################
### Newfoundland
# I want one large rel abundance chart that shows monthly sampling in order from Jun 2021 --> Jul 2022 

# For now, I am removing 2020 data since it is so "different". Also it only has one time period (Sept)
nlMinus2020 = nlMerge %>%
  filter(yearStart != 2020)

# Find the 7 most abundant taxa. Keep those names
# Call the rest of them "other"
bayOther = nlMinus2020 %>%
  # Want counts per taxa (class) for the whole bay, not by tow
  group_by(class) %>%
  summarize(countPerClass = sum(abund)) %>%
  mutate(rank = rank(-countPerClass),
         # Keep 5 most abundant classes, make the rest "Other"
         classNew = ifelse(rank <=7, class, "Other"))

# Add this these new classes as a column in the original dataframe
bayPlotDf = nlMinus2020 %>%
  left_join(bayOther, by = c("class" = "class")) %>%
              # Might be a better way, but I don't want to join the ENTIRE dataframe
             # select(classNew, class, countPerClass), by = c("class" = "class", "monthStart" = "monthStart", "yearStart" = "yearStart")) %>%
  group_by(classNew, sampleCode, myLabel, tidePhase, monthStart, yearStart) %>%
  # If you don't recompute counts, the "Other" class will have a bunch of black lines
  # if you set the outline colour to black in geom_bar
  summarise(sumCount = sum(abund))

# Dealing with months is tough. Use the numbers to facet them, so they are displayed in the correct order
# But make a list here of what each number refers to so the labels can be changed
monthFix = c(
  `6` = "Jun",
  `7` = "Jul",
  `8` = "Aug",
  `9` = "Sep",
  `10` = "Oct",
  `11` = "Nov",
  `12` = "Dec",
  `2` = "Feb",
  `3` = "Mar",
  `4` = "Apr",
  `5` = "May"
)

ggplot(bayPlotDf, aes(x=sampleCode, y=sumCount, fill=classNew)) +
  geom_bar(stat = "identity", position = "fill", col = "black") +
  scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+

  # Have the top panel be year, then the next subpanel is months. Use labeller to adjust month names  
  facet_nested(. ~yearStart + monthStart, scales = "free_x", space = "free_x", labeller = labeller(monthStart = monthFix))+
  scale_x_discrete(name = "Site")+
  scale_fill_brewer(palette = "Set3", name = "Zooplankton Class")+
  theme_bw()+
  theme(
    #axis.text.x = element_text(angle = 90, size = 8), # use this if want station labels
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(0.2, "cm"), # changes spacing between facets
    plot.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    strip.placement = "outside",
  )+
  guides(fill=guide_legend(ncol=2)) # Break the legend into 2 columns







