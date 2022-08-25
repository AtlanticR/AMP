################################################################################
# Stacked bar charts
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up

source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################
## Alter data format for creation of pie charts

# Something is majorly off with these counts though they're too high

# Maritimes

# Just get data from one bay
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
    group_by(classNew, sample, tideRange, location) %>%
    # If you don't recompute counts, the "Other" class will have a bunch of black lines
    # if you set the outline colour to black in geom_bar
    summarise(sumCount = sum(abund))
  
  
  # Make ggplot
  # Write the y-axis label outside of ggplot so I don't get (as) lost in brackets
  yLabel = expression(paste("Abundance (ind", m^{-3}, ")"))
  
  
  stackedGGPlot =
    ggplot(bayPlotDf, aes(x=sample, y=sumCount, fill=classNew)) +
      geom_bar(stat="identity", color = "black")+
      facet_grid(cols = vars(location), scales = "free_x", space = "free_x")+
      scale_x_discrete(name = "Station")+
      scale_y_continuous(labels = scales::comma, name = yLabel) +
      scale_fill_brewer(palette = "Accent", name = "Zooplankton Class")+
      #theme_minimal(base_family = "Roboto Condensed") +
      theme_bw()+
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
            plot.title = element_text(size = 15, face = "bold"),
            #strip.text.x = element_text(angle = 270, face = "bold"),
            strip.placement = "outside",
            #axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
            #axis.title.y = element_blank(),
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
  returnList = list(bayPlotDf, stackedGGPlot)
  
  return(returnList)
  
}


# Process them
argyleProcess = stackedBarChart(argyle)
soberProcess = stackedBarChart(sober)
whiteheadProcess = stackedBarChart(whitehead)
cHarbourProcess = stackedBarChart(whitehead)

malpequeProcess = stackedBarChart(malpeque)
cocagneProcess = stackedBarChart(cocagne)
stPetersProcess = stackedBarChart(stPeters)

# View plot
argyleProcess[[2]]
soberProcess[[2]]
whiteheadProcess[[2]]
cHarbourProcess[[2]]

malpequeProcess[[2]]
cocagneProcess[[2]]
stPetersProcess[[2]]

# But think about this... is there a faster way to do this? Should I split by bay in the function?

################################################################################

#update_geom_font_defaults(font_rc_light)

# Colours from Quentin
rrColorPalette<- c("#009E73", "#E69F00", "#0072B2", "#CC79A7", "#F0E442","#D55E00", "#56B4E9","#999999")
show_col(rrColorPalette)
