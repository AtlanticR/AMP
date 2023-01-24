###############################################################################
#### Add legends for NMDS by bays
# I want these to show tide phases by shape and stations by colour
# But I want the legend to show that as a grid, not separate items
# e.g., see Anderson (2007) Fig. 3b for an example:
# https://onlinelibrary.wiley.com/doi/full/10.1002/9781118445112.stat07841

# I don't see an easy way of doing this in ggplot
# Instead, create the legend as its own plot and place it beside the NMDS

# I also don't see an easy way of doing this within a for loop.
# Each bay was sampled at different tide phases and stations and legend needs to be 
# adapted to reflect that. 

# Data from all bays within a region will be included together in a plot, with the
# their own separate legend (e.g, (A) Argyle, (B) Country Harbour, etc.)
# Each bay will have own custom plot, except Pacific which followed the same sampling
# protocol for each field season

###############################################################################
## Run script to get each NMDS ordinations for each bay
# This returns 4 important items:
# marNMDSbays, gulfNMDSbays, nlNMDSbays, pacNMDSbays
# Access each individual using the command (e.g.):
# marNMDSbays[[1]] # gets Argyle
# This also reads in colour schemes/symbology for tide phases & stations
source("Figures/nmdsBays.R")

###############################################################################
###############################################################################
### MARITIMES

# Need to construct a data frame that shows tide and station info
# It's easiest to plot this as a ggplot geom_point in a grid

## Argyle

# x-coordinates for plot. This will show Tide Phases (as symbols).
arg.xcoord = c(1, 2, 3, 
            1, 2, 3, 
            1, 2, 3)

# y-coordinates for plot. This will show stations (as colours)
arg.ycoord = c(3, 3, 3,
               2, 2, 2, 
               1, 1, 1)

# Corresponding station and tide information
# Remember that (1,1) is first on plot (bottom left). Will therefore be "Inner" station, "Low" tide.
arg.myLabel = c("North", "North", "North", "Central", "Missing", "Central", "South", "South", "South")
arg.tidePhase = c("Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High")

# Put all of this in a data frame. Take a look at data frame to see how info is related.
arg.legDf = data.frame(arg.xcoord, arg.ycoord, arg.myLabel, arg.tidePhase)

# Plot it!
arg.ggLeg = ggplot()+
  geom_point(data = arg.legDf, aes(x = as.factor(arg.xcoord), y = as.factor(arg.ycoord), pch = arg.tidePhase, fill = arg.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  # Rename the labels to be a bit shorter so they all fit in a grid
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MR", "HT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c(3, 2, 1), labels = c("N", "Cen", "S"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))

# Make a plot of Argyle NMDS with legend beside it
# This is not beautiful code but it works! 
# This makes a 6 row, 4 column grid. First 3 columns are filled with Argyle NMDS info (the "1" within rbind below)
# And then the legend fills the 4th column, but only in the 3rd and 4th rows (plot "2")
arg.withLeg = grid.arrange(marNMDSbays[[1]], arg.ggLeg, nrow=6, ncol = 4,
                  layout_matrix = rbind(c(1,1,1, NA), 
                                        c(1,1,1, NA),
                                        c(1,1,1, 2),
                                        c(1,1,1, 2),
                                        c(1,1,1, NA),
                                        c(1,1,1, NA)))

###############################################################################
## Country Harbour

# I'm not going to comment the rest of the code as descriptively as Argyle's
# Refer to that for a better description (process is the same)

# Note that CH matrix is different because no "Mid-Rising" or "Mid-Falling" tide data
ch.xcoord = c(1,2,
              1,2,
              1,2)

ch.ycoord = c(3, 3,
              2, 2, 
              1, 1)

ch.myLabel = c("Inner", "Inner", "Mid", "Mid", "Outer", "Outer")
ch.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

ch.leg = data.frame(ch.xcoord, ch.ycoord, ch.myLabel, ch.tidePhase)

ch.ggLeg = ggplot()+
  geom_point(data = ch.leg, aes(x = as.factor(ch.xcoord), y = as.factor(ch.ycoord), pch = ch.tidePhase, fill = ch.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c(3, 2, 1), labels = c("In", "Mid", "Out"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))

ch.withLeg = grid.arrange(marNMDSbays[[2]], ch.ggLeg, nrow=6, ncol = 4,
                  layout_matrix = rbind(c(1,1,1, NA), 
                                        c(1,1,1, NA),
                                        c(1,1,1, 2),
                                        c(1,1,1, 2),
                                        c(1,1,1, NA),
                                        c(1,1,1, NA)))


###############################################################################
## Sober Island

sob.xcoord = c(1,2,3,
              1,2,3,
              1,2,3)

sob.ycoord = c(3, 3, 3,
              2, 2, 2, 
              1, 1, 1)

# Only the "Outer" stations were sampled at High Tide and Low Tide
# Inner and Mid stations sampled at "Mid-Rising" tide
# Mid and In therefore blank for HT/LT (shapes not filled in). Same with "MR" for "Outer" station
# "Missing" stations are shown in white
# This helps show the imbalance in sampling 
sob.myLabel = c("Missing", "Inner-North", "Missing", "Missing", "Inner-South", "Missing", "Outer", "Missing", "Outer")

sob.tidePhase = c("Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High")

sob.leg = data.frame(sob.xcoord, sob.ycoord, sob.myLabel, sob.tidePhase)

sob.ggLeg = ggplot()+
  geom_point(data = sob.leg, aes(x = as.factor(sob.xcoord), y = as.factor(sob.ycoord), pch = sob.tidePhase, fill = sob.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MR", "HT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c(3, 2, 1), labels = c("I-N", "I-S", "Out"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))

sob.withLeg = grid.arrange(marNMDSbays[[3]], sob.ggLeg, nrow=6, ncol = 4,
                  layout_matrix = rbind(c(1,1,1, NA), 
                                        c(1,1,1, NA),
                                        c(1,1,1, 2),
                                        c(1,1,1, 2),
                                        c(1,1,1, NA),
                                        c(1,1,1, NA)))

###############################################################################
## Whitehead

wh.xcoord = c(1, 2, 3, 
            1, 2, 3, 
            1, 2, 3)

wh.ycoord = c(3, 3, 3,
              2, 2, 2, 
              1, 1, 1)

wh.myLabel = c("Inner", "Inner", "Inner", "Mid", "Mid", "Mid", "Outer", "Outer", "Outer")
wh.tidePhase = c("Low", "Mid-Falling", "High", "Low", "Mid-Falling", "High", "Low", "Mid-Falling", "High")

wh.legDf = data.frame(wh.xcoord, wh.ycoord, wh.myLabel, wh.tidePhase)

wh.ggLeg = ggplot()+
  geom_point(data = wh.legDf, aes(x = as.factor(wh.xcoord), y = as.factor(wh.ycoord), pch = wh.tidePhase, fill = wh.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MF", "HT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c(1,2,3), labels = c("Out", "Mid", "In"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))


wh.withLeg = grid.arrange(marNMDSbays[[4]], wh.ggLeg, nrow=6, ncol = 4,
                  layout_matrix = rbind(c(1,1,1, NA), 
                                        c(1,1,1, NA),
                                        c(1,1,1, 2),
                                        c(1,1,1, 2),
                                        c(1,1,1, NA),
                                        c(1,1,1, NA)))

grid.arrange(arg.withLeg, ch.withLeg, sob.withLeg, wh.withLeg)


###############################################################################
###############################################################################
### GULF

## Cocagne

# x-axis is tide phase (symbol)
coc.xcoord = c(1, 2, 
               1, 2, 
               1, 2)
               
# y-axis is station (colour)
coc.ycoord = c(1, 1,
               2, 2, 
               3, 3)

coc.myLabel = c("South", "South", "Central", "Central", "North", "North")
coc.tidePhase = c("Low", "Mid-Rising", "Low", "Mid-Rising", "Low", "Mid-Rising")

coc.legDf = data.frame(coc.xcoord, coc.ycoord, coc.myLabel, coc.tidePhase)

coc.ggLeg = ggplot()+
  geom_point(data = coc.legDf, aes(x = as.factor(coc.xcoord), y = as.factor(coc.ycoord), pch = coc.tidePhase, fill = coc.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "MR"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("South", "Central", "North"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))


coc.withLeg = grid.arrange(gulfNMDSbays[[1]], coc.ggLeg, nrow=6, ncol = 4,
                          layout_matrix = rbind(c(1,1,1, NA), 
                                                c(1,1,1, NA),
                                                c(1,1,1, 2),
                                                c(1,1,1, 2),
                                                c(1,1,1, NA),
                                                c(1,1,1, NA)))

###############################################################################
## Malpeque

# x-axis is tide phase (symbol)
mal.xcoord = c(1, 
               1, 
               1)

# y-axis is station (colour)
mal.ycoord = c(1,
               2, 
               3)

mal.myLabel = c("South", "Central", "North")
mal.tidePhase = c("Low", "Low", "Low")

mal.legDf = data.frame(mal.xcoord, mal.ycoord, mal.myLabel, mal.tidePhase)

mal.ggLeg = ggplot()+
  geom_point(data = mal.legDf, aes(x = as.factor(mal.xcoord), y = as.factor(mal.ycoord), pch = mal.tidePhase, fill = mal.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1), labels = c("LT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("South", "Central", "North"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))


mal.withLeg = grid.arrange(gulfNMDSbays[[2]], mal.ggLeg, nrow=6, ncol = 4,
                          layout_matrix = rbind(c(1,1,1, NA), 
                                                c(1,1,1, NA),
                                                c(1,1,1, 2),
                                                c(1,1,1, 2),
                                                c(1,1,1, NA),
                                                c(1,1,1, NA)))

###############################################################################
# St. Peters

# x-axis is tide phase (symbol)
stP.xcoord = c(1,2,
               1,2,
               1,2)

# y-axis is station (colour)
stP.ycoord = c(3,3,
               2,2,
               1,1)

stP.myLabel = c("Inner", "Inner", "Mid", "Mid", "Outer", "Outer")
stP.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

stP.legDf = data.frame(stP.xcoord, stP.ycoord, stP.myLabel, stP.tidePhase)


stP.ggLeg = ggplot()+
  geom_point(data = stP.legDf, aes(x = as.factor(stP.xcoord), y = as.factor(stP.ycoord), pch = stP.tidePhase, fill = stP.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+
  scale_y_discrete(name = "Station", breaks = c(3, 2, 1), labels = c("In", "Mid", "Out"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))

stP.withLeg = grid.arrange(gulfNMDSbays[[3]], stP.ggLeg, nrow=6, ncol = 4,
                           layout_matrix = rbind(c(1,1,1, NA), 
                                                 c(1,1,1, NA),
                                                 c(1,1,1, 2),
                                                 c(1,1,1, 2),
                                                 c(1,1,1, NA),
                                                 c(1,1,1, NA)))

###############################################################################
###############################################################################
## NEWFOUNDLAND

nl.xcoord = c(1, 2, 3,
              1, 2, 3,
              1, 2, 3,
              1, 2, 3)

nl.ycoord = c(4, 4, 4,
              3, 3, 3,
              2, 2, 2,
              1, 1, 1)

nl.myLabel = c("Inner", "Inner", "Inner", "Mid-Inner", "Mid-Inner", "Mid-Inner", "Mid-Outer", "Mid-Outer", "Missing", "Outer", "Outer", "Missing")
nl.tidePhase = c("Mid-Falling", "Low", "Mid-Rising", "Mid-Falling", "Low", "Mid-Rising", "Mid-Falling", "Low", "Mid-Rising", "Mid-Falling", "Low", "Mid-Rising")

nl.legDf = data.frame(nl.xcoord, nl.ycoord, nl.myLabel, nl.tidePhase)

nl.ggLeg = ggplot()+
  geom_point(data = nl.legDf, aes(x = as.factor(nl.xcoord), y = as.factor(nl.ycoord), pch = nl.tidePhase, fill = nl.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationColNL)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("MF", "LT", "MR"), position = "top")+
  scale_y_discrete(name = "Station", breaks = c(4, 3, 2, 1), labels = c("In", "MI", "MO", "Out"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))

nl.withLeg = grid.arrange(nlNMDSbays[[1]], nl.ggLeg, nrow=6, ncol = 4,
                           layout_matrix = rbind(c(1,1,1, NA), 
                                                 c(1,1,1, NA),
                                                 c(1,1,1, 2),
                                                 c(1,1,1, 2),
                                                 c(1,1,1, NA),
                                                 c(1,1,1, NA)))

###############################################################################
###############################################################################
## Pacific

# Legend is the same for each field season (subplot)

# x-axis is tide phase (symbol)
pac.xcoord = c(1,2,
               1,2,
               1,2)

# y-axis is station (colour)
pac.ycoord = c(3, 3,
               2, 2,
               1, 1)

pac.myLabel = c("Inner", "Inner", "Mid", "Mid", "Outer", "Outer")
pac.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

pac.legDf = data.frame(pac.xcoord, pac.ycoord, pac.myLabel, pac.tidePhase)


pac.ggLeg = ggplot()+
  geom_point(data = pac.legDf, aes(x = as.factor(pac.xcoord), y = as.factor(pac.ycoord), pch = pac.tidePhase, fill = pac.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+ 
  scale_y_discrete(name = "Station", breaks = c(1, 2, 3), labels = c("Outer", "Mid", "Inner"))+ 
  theme_minimal()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "grey30"),
    #axis.title = element_blank(),
    legend.position = "none",
    panel.background=element_rect(colour="grey30"),
    panel.grid = element_blank(), 
    plot.background = element_blank(),
    # This affects the amount of space around each plot
    # If there is not enough space, plot_grid will make them too close together
    plot.margin=unit(c(0, 1, 0, 0),"cm"))


###############################################################################
###############################################################################
## Here are the final plots for each region
# Need to play around with size of plotting window so legend doesn't get squished

# Maritimes
grid.arrange(arg.withLeg, ch.withLeg, sob.withLeg, wh.withLeg)

# Gulf
grid.arrange(coc.withLeg, mal.withLeg, stP.withLeg, ncol = 2)

# Newfoundland. Make it the same size as other plots
grid.arrange(nl.withLeg, ncol = 2, nrow = 2)

# Pacific
# Play around with where exactly the legend should go because idk lol
grid.arrange(pacNMDSbays[[1]], pacNMDSbays[[2]], pacNMDSbays[[3]], pac.ggLeg, nrow=6, ncol = 6,
                           layout_matrix = rbind(c(1,1,1, 2, 2, 2), 
                                                 c(1,1,1, 2, 2, 2),
                                                 c(1,1,1, 2, 2, 2),
                                                 c(3,3,3, NA,NA, NA),
                                                 c(3,3,3, NA, 4, NA),
                                                 c(3,3,3, NA, NA, NA)))

