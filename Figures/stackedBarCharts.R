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
           classNew = ifelse(rank <=5, class, "Other"))

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
  
  
  # Make ggplot for a Stacked Bar Chart
  # Write the y-axis label outside of ggplot so I don't get (as) lost in brackets
  yLabelStacked = expression(paste("Abundance (ind ", m^{-3}, ")"))
  
  stackedGGPlot =
    ggplot(bayPlotDf, aes(x=sampleCode, y=sumCount, fill=classNew)) +
      geom_bar(stat="identity", color = "black")+
      facet_nested(. ~myLabel + tidePhase, scales = "free_x", space = "free_x")+
      #facet_grid(cols = vars(myLabel), scales = "free_x", space = "free_x")+
      scale_x_discrete(name = "Station")+
      scale_y_continuous(labels = scales::comma, name = yLabelStacked) +
      scale_fill_brewer(palette = "Accent", name = "Zooplankton Class")+
      #theme_minimal(base_family = "Roboto Condensed") +
      theme_bw()+
      theme(
        #axis.text.x = element_text(angle = 90, size = 8), # use this if want station labels
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(size=13),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "cm"), # changes spacing between facets
        #plot.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 13),
        strip.placement = "outside",
      )
  
  # For the relative abundance plots I DON'T want "Mid" tide data
  # I just want to compare High/Low. So remove the mid-rising and mid-falling tides
  # CHECK THIS: but I think "Mid" and "Inner" can be combined when looking at tide effect
  # Since it's supposed to be most pronounced at the mouth of the bay ("Outer")
  # Therefore, combine Mid/Inner location into one category
  # bayPlotDf = bayPlotDf %>%
  #   subset(tidePhase != "Mid-Rising" & tidePhase != "Mid-Falling") %>%
  #   mutate(myLabel = replace(myLabel, myLabel == "Mid", "Mid/Inner")) %>%
  #   mutate(myLabel = replace(myLabel, myLabel == "Inner", "Mid/Inner"))

  # Make ggplot for a relative abundance chart
  relGGPlot = 
    ggplot(bayPlotDf, aes(x=sampleCode, y=sumCount, fill=classNew)) +
      geom_bar(stat = "identity", position = "fill", col = "black") +
      scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
      facet_nested(. ~myLabel + tidePhase, scales = "free_x", space = "free_x")+
      # facet_grid(cols = vars(myLabel), scales = "free_x", space = "free_x")+
      scale_x_discrete(name = "Station")+
      scale_fill_brewer(palette = "Accent", name = "Zooplankton Class")+
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
      )
  
  bothPlots = plot_grid(stackedGGPlot, relGGPlot, ncol = 1, align = "v", axis = "l")
  
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
ggarrange(cocagneProcess, malpequeProcess, stPetersProcess, ncol = 1)

# Newfoundland (only one bay)
seArmProcess = stackedBarChart(nlMerge, "",)
ggarrange(cocagneProcess, cocagneProcess, cocagneProcess, seArmProcess, ncol = 1)


# Pacific (only one bay, but separate by facetFactor instead)
lemmens20Process = stackedBarChart(pacMerge %>% subset(facetFactor == "August 2020"), "(A) August 2020")
lemmensMar21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "March 2021"), "(B) March 2021")
lemmensJun21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "June 2021"), "(C) June 2021")
lemmensSept21Process = stackedBarChart(pacMerge %>% subset(facetFactor == "September 2021"), "(D) September 2021")

ggarrange(lemmens20Process, lemmensMar21Process, lemmensJun21Process, lemmensSept21Process, ncol = 1)



################################################################################
## For analysis later
# Some of these will show more of an effect than others


# High Risk Sites!!!
soberHighRisk = stackedBarChart(marMerge %>% subset(facetFactor == "Sober Island") %>%
                                                      subset(myLabel == "Outer"))

soberHighRiskInner = stackedBarChart(marMerge %>% subset(facetFactor == "Sober Island") %>%
                                  subset(myLabel == "Inner"))

whHighRisk = stackedBarChart(marMerge %>% subset(facetFactor == "Whitehead") %>%
                                            subset(myLabel == "Mid"))

spHighRisk = stackedBarChart(gulfMerge %>% subset(facetFactor == "St. Peters") %>%
                               subset(myLabel == "Mid"))












