################################################################################
# Stacked bar charts
# Created by Stephen Finnis 2022
# 
################################################################################

## Get things set up

source("C:/Users/FINNISS/Desktop/AMPcode/DataProcessing/zooplanktonCounts.R")

################################################################################
## Alter data format for creation of pie charts



# Maritimes
argyle = marMerge %>%
  subset(facilityName=="Argyle")

argyleCondense = argyle %>%
  # Want counts per taxa (class) for the whole bay, not by tow
  group_by(class) %>%
  summarize(countBay = sum(abund)) %>%
  mutate(rank = rank(-countBay),
         classNew = ifelse(rank <=7, class, "Other"))
  
argyle = argyle %>%
  left_join(argyleCondense %>%
              select(classNew, class), by = c("class" = "class"))


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
library(wbstats)
library(scales)

#update_geom_font_defaults(font_rc_light)


ggplot(argyle, aes(x=sample, y=abund, fill=classNew)) +
  geom_bar(stat="identity")+
  facet_grid(cols = vars(tideRange), scales = "free_x", space = "free_x")+
  #facet_wrap(~tideRange, scales = "free_x")+
  #theme_minimal(base_family = "Roboto Condensed") +
 # theme_minimal()+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    #strip.text.x = element_text(angle = 270, face = "bold"),
    strip.placement = "outside",
    #axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    #axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text = element_text(size = 10),
    #legend.position = "none",
    panel.grid.major.y = element_blank()
  )
