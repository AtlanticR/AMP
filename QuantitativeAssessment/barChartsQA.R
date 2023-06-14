################################################################################
################################################################################
##### Bar charts

# Created by Stephen Finnis


################################################################################

# Read in the script that puts together the QA (microscopy) and Flowcam data
source("QuantitativeAssessment/QAcodeMatches.R")



################################################################################


# Find the __ most abundant taxa. Label all others as "Other"
# Otherwise there are too many legend items
bayOther = allQAData %>%
  # Want counts per taxa (class) for the whole bay, not by tow
  group_by(newName) %>%
  summarize(countTotals = sum(count)) %>%
  mutate(rank = rank(-countTotals),
         # Keep 5 most abundant classes, make the rest "Other"
         classRanks = ifelse(rank <=8, newName, "Other")) %>%
  mutate(relAbund = countTotals/sum(countTotals)) # if i want the relative abundance

# Add this these new classes as a column in the original dataframe
bayPlotDf = bayOther %>%
  left_join(allQAData) %>% 
  # Might be a better way, but I don't want to join the ENTIRE dataframe
  # select(newName, class, countTotals), by = c("newName" = "newName")) %>%
  group_by(classRanks, FlowCamID) %>%
  # If you don't recompute counts, the "Other" class will have a bunch of black lines
  # if you set the outline colour to black in geom_bar
  summarise(sumCount = sum(count))

hi = 
  ggplot()+
  geom_bar(bayPlotDf, mapping = aes(x = FlowCamID, y = sumCount, fill = classRanks), col = "black", linewidth = 0.05, stat = "identity")+
  # Break up by region/year so it displays the 4 groups of interest
  #facet_wrap(~regionYear, scales = "free")+
  #scale_fill_manual(values=as.vector(alphabet(10)))+
  scale_fill_brewer(palette = "Set3", name = "Taxa")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

hi2=
  ggplot() +
  geom_bar(bayPlotDf, mapping = aes(x=FlowCamID, y=sumCount, fill=classRanks), position = "fill", stat = "identity", col = "black", linewidth = 0.05) +
  scale_y_continuous(labels = scales::percent_format(), name = "Relative Abundance")+
  scale_fill_brewer(palette = "Set3", name = "Taxa")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_blank(),
  )

ggarrange(hi, hi2)

