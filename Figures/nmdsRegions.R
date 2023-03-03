################################################################################
################################################################################
### NMDS Ordinations for regional FlowCam data
# But in this script I'm going to add symbols to distinguish each plot

# This makes NMDS ordinations when data are displayed by REGIONS
# Likely, not all plots will be used in Tech Report
# There is code to make ordinations for:
# All regions together, just the Atlantic Ocean, and each region separately
# (with data by bays identified)

################################################################################
## Read in other scripts

# This has all the plankton data with counts for each file
# source("DataProcessing/zooplanktonCounts.R")

source("DataProcessing/dividePlankton.R")
# This sets the colours schemes and symbology for bays, regions, etc
source("Figures/colourPchSchemes.R")

#################################################################################
#################################################################################
## Create NMDS for all data
# Including Pacific and Atlantic data

# Plot is a bit more complicated because I need 2 legend items: Atlantic Ocean (and DFO regions listed underneath)
# and Pacific Ocean (with DFO region listed underneath)
# This has to be done by making 3 ggplots to:
# 1. Get the legend (only, not the actual plot) from Atlantic data
# 2. Get the Legend (only) from Pacific data
# 3. Plot (with no legend) of both Pacific and Atlantic data
# All three will then be combined with grid.arrange()


# This will display each DFO region (Gulf, Maritimes, Newfoundland, Pacific) with a different colour
# Possibly different symbols for each ocean (Pacific, Atlantic). TBD how I handle this.

# Combine all the data together
allRegions = rbind(marMerge, nlMerge, pacMerge, gulfMerge)

# Convert it to wide format
allRegionsWide = allRegions %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0))  # replace NAs with 0 
  
  
# For NMDS calculations, must only include species data from dataframe
# I will constantly be removing columns, adding columns etc. 
# Instead define as the index where there's Acartia species (first species colum in dataframe) to the end (final column)
beginNMDSAll = which(colnames(allRegionsWide)== "Acartia spp. (civ-vi)")
endNMDSAll = ncol(allRegionsWide)

# Do NMDS ordination but only include species data
ordAll = metaMDS(sqrt(allRegionsWide[,c(beginNMDSAll:endNMDSAll)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot
ordCoordsAll = as.data.frame(scores(ordAll, display="sites"))
# Add NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressAll = paste("2D Stress: ", format(round(ordAll$stress, digits=2), nsmall=2))

# Get the number of facets there should be (either # of bays, or # of sampling months (Pacific))
# create array for pch symbols. e.g., if 4 factors will give: 21, 22, 23, 24
oceanArray = c(21:(20+length(unique(allRegions$ocean)))) 
regionArray = c(21:(20+length(unique(allRegions$region)))) 

# Add region and ocean onto this data frame (it's easier for plotting in aes for ggplot)
ordCoordsAll = ordCoordsAll %>%
  mutate(region = allRegionsWide$region, ocean = allRegionsWide$ocean) %>%
  mutate(month = allRegionsWide$monthStart)


# Compute the group centroids
centOcean = aggregate(cbind(NMDS1, NMDS2)~ocean, data = ordCoordsAll, FUN = mean) # centroid of the oceans
centRegion = aggregate(cbind(NMDS1, NMDS2)~region, data =ordCoordsAll, FUN = mean) # centroid of the regions

# Add these centroids by merging with ordCoordsAll. Rename the centroids to 'oNMDS1' and 'oNMDS2' to represent NMDS coordinate centroids
segs = merge(ordCoordsAll, setNames(centOcean, c('ocean','oNMDS1','oNMDS2')),
              by = 'ocean', sort = FALSE)

# Now merge the region coordinates
segs2 = merge(segs, setNames(centRegion, c('region', 'rNMDS1', 'rNMDS2')),
              by = "region", sort = FALSE)

# Dealing with months is tough. Use the numbers set the fill, so they are displayed in the correct order
# But make a list here of what each number refers to so the labels can be changed
monthFix = c(`6` = "Jun", `7` = "Jul", `8` = "Aug", `9` = "Sep", `10` = "Oct", `11` = "Nov", `12` = "Dec", `2` = "Feb", `3` = "Mar", `4` = "Apr", `5` = "May")

# Specify the colour palette that I will be using for 12 months of data
# Note that I don't have any data for January (in either 2021 or 2022), so that will need to be removed when specifying the values
colPal = hue_pal()(12)

# NMDS where each region is a different colour and symbol
ggRegs = ggplot() + 
  #geom_segment(data = segs, mapping = aes(x = NMDS1, xend = oNMDS1, y = NMDS2, yend = oNMDS2), col = "grey49")+ # map segments for ocean
  geom_segment(data = segs2, mapping = aes(x = NMDS1, xend = rNMDS1, y = NMDS2, yend = rNMDS2), col = "grey49")+ # map segments for regions
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, fill = allRegionsWide$region, pch = allRegionsWide$region), alpha= 0.9, size = 6)+
  scale_shape_manual(values=c(21:24), name = "Region")+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_fill_discrete(name = "Region")+
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4.5, hjust=1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14),
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        # Margin written top, right, bottom, left
        # Increase the spacing on the right so this plot and the next have more space in between
        plot.margin=unit(c(0.1, 1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

# NMDS where each region is a different symbol and each month is a different colour
# Each month as a different colour
ggRegMonths = ggplot() + 
  geom_point(data = ordCoordsAll, aes(x=NMDS1, y=NMDS2, pch = allRegionsWide$region, fill = as.factor(month)), size = 6)+
  # Don't need to define colours. These just show up as default ggplot colours for 4 elements
  scale_shape_manual(values = regionArray, name = "Region")+ 
  scale_fill_manual(name = "Month", values = colPal[-1], labels = monthFix)+
  annotate("text", x = max(ordCoordsAll$NMDS1), y=max(ordCoordsAll$NMDS2), label = ordStressAll, size=4.5, hjust=1)+
  # ggtitle("Atlantic and Pacific")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "none",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        # Margin written top, right, bottom, left
        # Increase the spacing on the left so this plot and the next have more space in between
        # Make sure margin adjustments are equal in both plots, so one isn't distorted more than the other
        plot.margin=unit(c(0.1, 0.1, 0.1, 1),"cm"),
        plot.title = element_text(size=18))+
  guides(fill = guide_legend(override.aes = list(shape=21)))

ggarrange(ggRegs, ggRegMonths, ncol = 1)


#################################################################################
#################################################################################
## Now do the same thing for Atlantic ocean

# I need to create 3 legend items for:
# 1. Gulf as title (subpoints are Cocagne, Malpeque, StPeters)
# 2. Maritimes (Argyle, Country Harbour, Malpeque, Sober Island Oyster)
# 3. Newfoundland (rename as Southeast Arm)

# The legend is difficult to line up properly and my solution is pretty hacky
# I fixed it by adding extra spaces to "Gulf Region" to get it to be ~same length of characters as longest entry in legend (Country Harbour)
# Also, added extra padding to legend margin for Newfoundland to raise the legend "up" for Newfoundland, otherwise the spacing between all 3 legends is uneven

# Get only the Atlantic Ocean data
atlOnly = allRegionsWide %>%
  filter(ocean == "Atlantic") 

# Set the start/stop column indices for NMDS to run on
beginNMDSAtl = which(colnames(atlOnly)== "Acartia spp. (civ-vi)")
endNMDSAtl = ncol(atlOnly)

# Do NMDS ordination
ordAtl = metaMDS(sqrt(atlOnly[,c(beginNMDSAtl:endNMDSAtl)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot and add back in certain columns (easier for aes commands)
ordCoordsAtl = as.data.frame(scores(ordAtl, display="sites")) %>%
  mutate(tidePhase = atlOnly$tidePhase) %>%
  mutate(facetFactor = atlOnly$facetFactor) %>%
  mutate(myLabel = atlOnly$myLabel) %>%
  mutate(region = atlOnly$region)

# Get NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressAtl = paste("2D Stress: ", format(round(ordAtl$stress, digits=2), nsmall=2))

# Gulf
ggGulf = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Gulf"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 21, size = 6)+
  scale_fill_manual(values = c("red4", "red2", "lightpink"), name = "Gulf Region          ")+
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1,1))+ 
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin=margin(c(0,0,5,0)),
        legend.text=element_text(size = 12),
        legend.title = element_text(size = 14))

# Maritimes
ggMaritimes = ggplot()+
  geom_point(data = ordCoordsAtl %>% filter(region == "Maritimes"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 22, size = 6)+
  scale_fill_manual(values = c("darkgreen", "green3", "darkolivegreen2", "mediumspringgreen"), name = "Maritimes Region")+
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1,1))+ 
  theme_bw()+
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin=margin(c(0,0,0,0)),
        legend.text=element_text(size = 12),
        legend.title = element_text(size = 14))

# Newfoundland
# Add extra padding to top of 
# ggNewfoundland = ggplot()+
#   geom_point(data = ordCoordsAtl %>% filter(region == "Newfoundland"), aes(x = NMDS1, y = NMDS2, fill = facetFactor), pch = 24, size = 6)+
#   scale_fill_manual(values = c("#00BFC4"), name = "Newfoundland", labels = "Southeast Arm  ")+
#   coord_cartesian(xlim = c(-1, 1), ylim = c(-1,1))+ 
#   theme_bw()+
#   theme(legend.key.size = unit(0.2, "cm"),
#         legend.margin=margin(c(0,0, 30,0)), # top, right, bottom, left. Add extra space to "bottom" margin to make legend move up
#         legend.text=element_text(size = 12),
#         legend.title = element_text(size = 14))

# Now make a plot of everything (without the legend)
ggAtlanticOnly = ggplot()+
  geom_point(data = ordCoordsAtl, aes(x = NMDS1, y = NMDS2, fill = facetFactor, pch = region), size = 6)+
  scale_shape_manual(values=c(21, 22, 24), name = "Bay")+
  # Note that fill is alphabetical by facetFactor. Use sort(unique(ordCoordsAtl$facetFactor)) to determine order
  scale_fill_manual(values = c("darkgreen", "red4", "green3", "red2", "darkolivegreen2", "#00BFC4", "lightpink", "mediumspringgreen"), name = "Region")+
  annotate("text", x = max(ordCoordsAtl$NMDS1), y=max(ordCoordsAtl$NMDS2), label = ordStressAtl, size=4, hjust=1)+
  ggtitle("Atlantic")+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))

# Get (just) the legends
gulfLegend = as_grob(get_legend(ggGulf))
marLegend = as_grob(get_legend(ggMaritimes))
# nlLegend = as_grob(get_legend(ggNewfoundland))
# 
# # Plot it all together
# grid.arrange(ggAtlanticOnly, gulfLegend, marLegend, nlLegend, nrow=2, ncol = 2,
#              layout_matrix = rbind(c(1,1,1,NA), 
#                                    c(1,1,1,2),
#                                    c(1,1,1,3),
#                                    c(1,1,1,4),
#                                    c(1,1,1,NA)))

#################################################################################
# Actually I think what I want more than Maritimes, Gulf and Newfoundland is just Maritimes & Gulf
# Here is the code for putting just those two together
# Note that I don't need to make new legend items for Mar & Gulf because that was done above

# Get only the Maritimes and Gulf data
gulfMarOnly = allRegionsWide %>%
  filter(region == "Maritimes" | region == "Gulf") 

# Set the start/stop column indices for NMDS to run on
beginNMDSGulfMar = which(colnames(gulfMarOnly)== "Acartia spp. (civ-vi)")
endNMDSGulfMar = ncol(gulfMarOnly)

# Do NMDS ordination
ordGulfMar = metaMDS(sqrt(gulfMarOnly[,c(beginNMDSGulfMar:endNMDSGulfMar)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot and add back in certain columns (easier for aes commands)
ordCoordsGulfMar = as.data.frame(scores(ordGulfMar, display="sites")) %>%
  mutate(facetFactor = gulfMarOnly$facetFactor) %>%
  mutate(myLabel = gulfMarOnly$myLabel) %>%
  mutate(region = gulfMarOnly$region)

# Get NMDS stress
# Note that round() includes UP TO 2 decimal places. Does not include 0s 
ordStressGulfMar = paste("2D Stress: ", format(round(ordGulfMar$stress, digits=2), nsmall=2))

# Now make a plot of everything (without the legend)
ggGulfMar = ggplot()+
  geom_point(data = ordCoordsGulfMar, aes(x = NMDS1, y = NMDS2, fill = facetFactor, pch = region), size = 6)+
  scale_shape_manual(values=c(21, 22), name = "Bay")+
  # Note that fill is alphabetical by facetFactor. Use sort(unique(ordCoordsGulfMar$facetFactor)) to determine order
  # Argyle, Cocagne, Country Harbour, Malpeque, Sober Island, St. Peters, Whitehead
  scale_fill_manual(values = c("darkgreen", "red4", "green3", "red2", "darkolivegreen2", "lightpink", "mediumspringgreen"), name = "Region")+
  annotate("text", x = min(ordCoordsGulfMar$NMDS1), y=max(ordCoordsGulfMar$NMDS2), label = ordStressGulfMar, size=5, hjust = -0.01)+
  #ggtitle("Atlantic")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 13),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"),
        plot.title = element_text(size=18))


# Plot it all together
# Note that I didn't need to use new legend items
# But I did need to adjust the spacing between them. This is a hideous method, but it works!
grid.arrange(ggGulfMar, gulfLegend, marLegend, nrow=2, ncol = 2,
             layout_matrix = rbind(c(1,1,1,NA), 
                                   c(1,1,1,NA),
                                   c(1,1,1,NA),
                                   c(1,1,1,2),
                                   c(1,1,1,NA),
                                   c(1,1,1,3),
                                   c(1,1,1,NA),
                                   c(1,1,1,NA),
                                   c(1,1,1,NA)))


#################################################################################
## Now Plot each bay separately
# This is easier because there is only one legend item
# Run as a function and pass in the various  


nmdsPrep = function(mergeData, bayColours, breakVals) {
  # alter the dataframe so it is in appropriate format for NMDS
  # names_from: The column whose values will be used as column names
  # values_from: The column whose values will be used as cell values
  mergeData = mergeData %>% 
    pivot_wider(names_from = class, values_from = abund) %>%
    mutate_all(~replace(., is.na(.), 0)) # replace NAs with 0
  
  # For NMDS calculations, must only include species data from dataframe
  # I will constantly be removing columns, adding columns etc. 
  # Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
  beginNMDS = which(colnames(mergeData)== "Acartia spp. (civ-vi)")
  endNMDS = ncol(mergeData)
  
  # Do NMDS ordination but only include species data
  ord = metaMDS(sqrt(mergeData[,c(beginNMDS:endNMDS)]), distance = "bray", autotransform=FALSE)
  
  # Get NMDS coordinates from plot
  ordCoords = as.data.frame(scores(ord, display="sites")) %>%
    mutate(tidePhase = mergeData$tidePhase) %>%
    mutate(facetFactor = mergeData$facetFactor) %>%
    mutate(myLabel = mergeData$myLabel)
  
  # Add NMDS stress
  # Note that round() includes UP TO 2 decimal places. Does not include 0s 
  ordStress = paste("2D Stress: ", format(round(ord$stress, digits=2), nsmall=2))
  

  # Need to get the legend title for each plot
  # For Maritimes/Newfoundland/Gulf, it's "Bay"
  # For Pacific, they only sampled one bay (Lemmens) but had multiple field seasons that will be displayed instead
  legendTitle = ifelse(mergeData$region[1] == "Pacific", # only need to check first entry
                              "Field Season", 
                              "Bay")
  
  # Get centroid of each bay (facetFactor) to be plotted on NMDS as lines
  centFacet = aggregate(cbind(NMDS1, NMDS2)~facetFactor, data =ordCoords, FUN = mean) # centroid of the regions
  # Now merge the region coordinates. These now form "segments" from centroid to actual coordinates
  segs = merge(ordCoords, setNames(centFacet, c('facetFactor', 'fNMDS1', 'fNMDS2')),
               by = "facetFactor", sort = FALSE) %>%
    mutate(region = mergeData$region)
  
  # Make the ggplot item for each DFO region
  ggBay =
    ggplot() + 
    geom_segment(data = segs, mapping = aes(x = NMDS1, xend = fNMDS1, y = NMDS2, yend = fNMDS2), col = "grey49")+ # map segments for distance to centroid
    geom_point(data = ordCoords, aes(x=NMDS1, y=NMDS2, fill = facetFactor, pch = facetFactor), alpha = 0.9, size = 5)+ # Use pch=21 to get black outline circles
    # I couldn't get my stupid ifelse() statement to work. Instead, just pass in the specified break values 
    scale_shape_manual(values = c(21:24), name = legendTitle, breaks = breakVals)+
    scale_fill_manual(name = legendTitle, values = bayColours, breaks = breakVals)+
    ggtitle(mergeData$region)+
    #annotate("text", x = max(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4.5, hjust=1)+ # for all others
    annotate("text", x = min(ordCoords$NMDS1), y=max(ordCoords$NMDS2), label = ordStress, size=4.5, hjust = -0.01)+ # for Maritimes (otherwise 2D stress gets blocked)
    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title = element_text(size = 12), # don't want 
          axis.ticks = element_blank(),
          legend.text=element_text(size = 13),
          legend.title = element_text(size = 14),
          panel.border=element_rect(color="black", size=1), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"),
          plot.title = element_text(size=16))

  return(ggBay)
  
}


# Last argument is the "breaks" which changes the order of the legend items
# The default is alphabetical order, which is correct for all but Pacific!
# For Pacific, need to rearrange chronologically, so specify the order needed
# waiver() is what you enter for breaks if you just want the default value
marNMDS = nmdsPrep(mar, marColours, waiver())
pacNMDS = nmdsPrep(pac, pacColours, c("August 2020", "March 2021", "June 2021", "September 2021"))
gulfNMDS = nmdsPrep(gulf, gulfColours, waiver())
# I actually don't want Newfoundland data to be displayed this way due to the sampling design.
# NMDS ordinations are instead shown below.
# nlNMDS = nmdsPrep(nlMerge, nlColours, waiver())

# This works better than grid.arrange! It aligns all the plots with unequal legends
# plot_grid(marNMDS, nlNMDS, pacNMDS, gulfNMDS, align = "v")
plot_grid(marNMDS, gulfNMDS, ncol = 1, align = "v")

########################################################################################################################
########################################################################################################################
## Make NMDS of Newfoundland 
# EXPLANATION:
# Newfoundland had a slightly different sampling design
# They sampled a lot in Oct 2020. Then they sampled Jun 2021 --> July 2022, every month (except Jan 2022) with ~ 3 samples/month
# They took many samples in Sept 2021 to study tide effects a bit more

# Here, I am showing all data. Shape is year, colour represents months.
# Below, I am making 2 NMDS ordinations: one with all data (2020, 2021, 2022)
# Because 2020 data is quite distinct (and there's a large temporal sampling gap), I am making a second ordination of just 2021--> 2022 data
# In the other NMDS of 2021 --> 2022 I am including the centroid of each sampling month, and connecting them with arrows.
# This helps show how zooplankton composition changed over time

# Note that in nmdsBay.R, I am creating two NMDS for NL data: one of Oct 2020 and the other of Sept 2021. For those, symbols are tide phases and colours are stations.

########################################################################################################################
# NL 2021 and 2022 only NMDS
# This is the one that has the arrows connecting between sampling month centroids

####### I NEED TO FIX THIS SO VARIABLES WITH THE TWO PLOTS DO NOT HAVE THE SAME NAME
# H/E THAT'S A LOT OF WORK. AND I THINK I WILL BE REMOVING ONE OF THE PLOTS
# LEAVE IT FOR NOW AND COME BACK TO IT

# Switch format of Newfoundland data so each row represents a sample
nlMergeWide = nl %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% # replace NAs with 0
  # Remove year 2020: it is too different from 2021 and 2022
  # DON'T run this command for the second NMDS below
  filter(yearStart != 2020)
  
# For NMDS calculations, must only include species data from dataframe
# I will constantly be removing columns, adding columns etc. 
# Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
beginNMDS.nl = which(colnames(nlMergeWide)== "Acartia spp. (civ-vi)")
endNMDS.nl = ncol(nlMergeWide)

# Do NMDS ordination but only include species data
ord.nl = metaMDS(sqrt(nlMergeWide[,c(beginNMDS.nl:endNMDS.nl)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot. Add back in certain columns that might be important for labelling
ordCoords.nl = as.data.frame(scores(ord.nl, display="sites")) %>%
  mutate(tidePhase = nlMergeWide$tidePhase) %>%
  mutate(facetFactor = nlMergeWide$facetFactor) %>%
  mutate(myLabel = nlMergeWide$myLabel) %>%
  mutate(month = nlMergeWide$monthStart) %>%
  mutate(year = as.factor(nlMergeWide$yearStart)) 
# Calculate the 2D stress of the ordination. Round to 2 decimal places and make sure 2 decimal places show up is second is a zero
ordStress.nl = paste("2D Stress: ", format(round(ord.nl$stress, digits=2), nsmall=2))

# Compute the group centroids for each month of each year (i.e., centroid of July 2021 is different from centroid of July 2022)
centMonth = aggregate(cbind(NMDS1, NMDS2)~month+year, data = ordCoords.nl, FUN = mean) %>%  # centroid of the oceans
  # Add a column called "year" and make each entry equal "centroid". I will use this to filter out what to plot on the ggplot 
  mutate(year = "centroid") %>%
  mutate(monthAbb = month.abb[month]) %>%
  # Add index that shows the order of data collection
  mutate(orderCollect = 1:nrow(centMonth)) %>%
  # Not great programming, but it's the fastest way to do this. The first 7 entries are from 2021, the rest are from 2022
  # I need these to create labels for the NMDS
  mutate(yearLabel = ifelse(row_number() <= 7, "2021", "2022")) %>%
  mutate(monYear = paste(monthAbb, yearLabel))

# Combine the coordinates of the centroids with the coordinates for the NMDS
# The centroids will basically be added as new rows of data at the bottom of the dataframe
ordCoordsJoin = ordCoords.nl %>%
  full_join(centMonth) 

# I want to draw arrows as geom_segment that connects the centroid of each month
# I am following these instructions and adapting to my own code:
# https://stackoverflow.com/questions/68754148/how-do-i-draw-directed-arrows-based-on-one-ordered-list-in-r

# Need to use tidyverse to get tibble(?) function
library(tidyverse)

# I need to specify the "start" of my route as the first month in the df of centroids
# I will be drawing segments, that go from a "start" coordinate to "end"
# The "start" is the centroid position of the first month. It will "end" at the centroid of the second month
# For the next segment, that "end" because the new start, and it will connect to the next centroid
centroidSegs = tibble(start = centMonth$orderCollect, end = centMonth$orderCollect[c(2:length(centMonth$orderCollect), 1)]) %>% 
  filter(start != end) %>%
  # Join with the centroid data so it will gain the NMDS coordinate positions
  left_join(centMonth, by = c("start" = "orderCollect")) %>%
  left_join(centMonth,  by = c("end" = "orderCollect"), suffix = c("_start", "_end")) %>%
  # Remove the final row because I don't want the last sampling event to connect to the first
  filter(row_number() <= n()-1) 

# Make the ggplot
ggNL2122 = 
ggplot() + 
  # Add the first set of points: actual NMDS data for each sample, but remove the centroid data. Symbol will be year, fill is month
  geom_point(data = ordCoordsJoin %>% filter(year != "centroid"), aes(x=NMDS1, y=NMDS2, fill = as.factor(month), pch = as.factor(year)), alpha = 0.9, size = 5)+ # Use pch=21 to get black outline circles
  scale_shape_manual(values = c(22, 23), name = "Year")+
  # Set the colour scheme as previously specified, but remove the first value because there is no "January" data
  scale_fill_manual(name = "Month", values = colPal[-1])+ 
  # Add the centroids as asterisks
  geom_point(data = ordCoordsJoin %>% filter(year == "centroid"), aes(x=NMDS1, y=NMDS2, col = as.factor(month)), pch = 8, alpha = 0.9, size = 5)+
  # Make the colours of the asterisks be the same as the NMDS points
  scale_color_manual(values = colPal[-1])+
  # Draw arrows between segments
  geom_segment(data = centroidSegs, aes(x = NMDS1_start, y = NMDS2_start, xend = NMDS1_end, yend = NMDS2_end), arrow = arrow(length = unit(0.2, "inches")), linewidth = 0.7)+
  # Add labels for each month of data and adjust transparency: may need to remove this
  geom_label_repel(data = ordCoordsJoin %>% filter(year == "centroid"), aes(x=NMDS1, y=NMDS2, label= monYear), colour = "black", size = 5, alpha = 0.7)+ # Use pch=21 to get black outline circles
  ggtitle("(A) Newfoundland (2021-2022)")+
  annotate("text", x = max(ordCoords.nl$NMDS1), y=max(ordCoords.nl$NMDS2), label = ordStress.nl, size=5.5, hjust=1)+ # for all others
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14),
        #legend.position = "none", # May add legend back in. TBD.
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"),
        plot.title = element_text(size=16))+
  #guides(fill = guide_legend(override.aes = list(shape=c(21)))) # This is to get the legend to work properly. But I might remove the legend
  guides(colour = "none", fill = "none")

########################################################################################################################
# NMDS of 2020, 2021 and 2022 data

# This is not the best way to code it.
# But, rerun everything above and don't include the filter(yearStart != 2020) part of the code


# Switch format of Newfoundland data so each row represents a sample
nlMergeWide = nl %>% 
  pivot_wider(names_from = class, values_from = abund) %>%
  mutate_all(~replace(., is.na(.), 0)) 

# For NMDS calculations, must only include species data from dataframe
# I will constantly be removing columns, adding columns etc. 
# Instead define as the index where there's Acartia species (first species column in dataframe) to the end (final column)
beginNMDS.nl = which(colnames(nlMergeWide)== "Acartia spp. (civ-vi)")
endNMDS.nl = ncol(nlMergeWide)

# Do NMDS ordination but only include species data
ord.nl = metaMDS(sqrt(nlMergeWide[,c(beginNMDS.nl:endNMDS.nl)]), distance = "bray", autotransform=FALSE)

# Get NMDS coordinates from plot. Add back in certain columns that might be important for labelling
ordCoords.nl = as.data.frame(scores(ord.nl, display="sites")) %>%
  mutate(tidePhase = nlMergeWide$tidePhase) %>%
  mutate(facetFactor = nlMergeWide$facetFactor) %>%
  mutate(myLabel = nlMergeWide$myLabel) %>%
  mutate(month = nlMergeWide$monthStart) %>%
  mutate(year = as.factor(nlMergeWide$yearStart)) 
# Calculate the 2D stress of the ordination. Round to 2 decimal places and make sure 2 decimal places show up is second is a zero
ordStress.nl = paste("2D Stress: ", format(round(ord.nl$stress, digits=2), nsmall=2))



# Compute the group centroids for each month of each year (i.e., centroid of July 2021 is different from centroid of July 2022)
centMonth = aggregate(cbind(NMDS1, NMDS2)~month, data = ordCoords.nl, FUN = mean) %>%  # centroid of the oceans
  # Add a column called "year" and make each entry equal "centroid". I will use this to filter out what to plot on the ggplot 
  mutate(centroid = "centroid") %>%
  mutate(monthAbb = month.abb[month]) %>%
  # Add index that shows the order of data collection
  mutate(orderCollect = 1:nrow(centMonth)) %>%
  # Not great programming, but it's the fastest way to do this. The first 7 entries are from 2021, the rest are from 2022
  # I need these to create labels for the NMDS
  mutate(yearLabel = ifelse(row_number() <= 7, "2021", "2022")) %>%
  mutate(monYear = paste(monthAbb, yearLabel))

# Combine the coordinates of the centroids with the coordinates for the NMDS
# The centroids will basically be added as new rows of data at the bottom of the dataframe
ordCoordsJoin = ordCoords.nl %>%
  full_join(centMonth) 

# I want to draw arrows as geom_segment that connects the centroid of each month
# I am following these instructions and adapting to my own code:
# https://stackoverflow.com/questions/68754148/how-do-i-draw-directed-arrows-based-on-one-ordered-list-in-r

# I need to specify the "start" of my route as the first month in the df of centroids
# I will be drawing segments, that go from a "start" coordinate to "end"
# The "start" is the centroid position of the first month. It will "end" at the centroid of the second month
# For the next segment, that "end" because the new start, and it will connect to the next centroid
centroidSegs = tibble(start = centMonth$orderCollect, end = centMonth$orderCollect[c(2:length(centMonth$orderCollect), 1)]) %>% 
  filter(start != end) %>%
  # Join with the centroid data so it will gain the NMDS coordinate positions
  left_join(centMonth, by = c("start" = "orderCollect")) %>%
  left_join(centMonth,  by = c("end" = "orderCollect"), suffix = c("_start", "_end")) #%>%
  # Remove the final row because I don't want the last sampling event to connect to the first
  #filter(row_number() <= n()-1) 


ggNLAllYears = 
ggplot() + 
  # Add the first set of points: actual NMDS data for each sample, but remove the centroid data. Symbol will be year, fill is month
  geom_point(data = ordCoords.nl, aes(x=NMDS1, y=NMDS2, fill = as.factor(month), pch = as.factor(year)), alpha = 0.9, size = 5)+ # Use pch=21 to get black outline circles
  scale_shape_manual(values = c(21, 22, 23), name = "Year")+
  # Set the colour scheme as previously specified, but remove the first value because there is no "January" data
  scale_fill_manual(name = "Month", values = colPal[-1])+ 
  # Add the centroids as asterisks
  geom_point(data = ordCoordsJoin %>% filter(!is.na(centroid)), aes(x=NMDS1, y=NMDS2, col = as.factor(month)), pch = 8, alpha = 0.9, size = 5)+
  # Make the colours of the asterisks be the same as the NMDS points
  scale_color_manual(values = colPal[-1])+
  # Draw arrows between segments
  geom_segment(data = centroidSegs, aes(x = NMDS1_start, y = NMDS2_start, xend = NMDS1_end, yend = NMDS2_end), arrow = arrow(length = unit(0.2, "inches")), linewidth = 0.7)+
  # Add labels for each month of data and adjust transparency: may need to remove this
  geom_label_repel(data = ordCoordsJoin %>% filter(!is.na(centroid)), aes(x=NMDS1, y=NMDS2, label= monthAbb), colour = "black", size = 5, alpha = 0.7)+ 
  ggtitle("(B) Newfoundland (2020-2022)")+
  annotate("text", x = max(ordCoords.nl$NMDS1), y=max(ordCoords.nl$NMDS2), label = ordStress.nl, size=5.5, hjust=1)+ # for all others
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.text=element_text(size = 13),
        legend.title = element_text(size = 14),
        #legend.position = "none", # May add legend back in. TBD.
        panel.border=element_rect(color="black", size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm"),
        plot.title = element_text(size=16))+
  #guides(fill = guide_legend(override.aes = list(shape=c(21)))) # This is to get the legend to work properly. But I might remove the legend
  guides(colour = "none", fill = "none")

ggarrange(ggNL2122, ggNLAllYears, ncol = 1)

