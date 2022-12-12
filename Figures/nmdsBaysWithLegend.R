###############################################################################












source("Figures/nmdsBays.R")



# Argyle
arg.ycoord = c(1, 1, 1,
            2, 2, 2, 
            3, 3, 3)

arg.xcoord = c(1, 2, 3, 
            1, 2, 3, 
            1, 2, 3)


arg.myLabel = c("Inner", "Inner", "Inner", "Mid", "Mid", "Mid", "Outer", "Outer", "Outer")
arg.tidePhase = c("Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High")

arg.legDf = data.frame(arg.xcoord, arg.ycoord, arg.myLabel, arg.tidePhase)

arg.ggLeg = ggplot()+
  geom_point(data = arg.legDf, aes(x = as.factor(arg.xcoord), y = as.factor(arg.ycoord), pch = arg.tidePhase, fill = arg.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MR", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("In", "Mid", "Out"))+ 
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


arg.withLeg = grid.arrange(marNMDSbays[[1]], arg.ggLeg, nrow=6, ncol = 4,
                  layout_matrix = rbind(c(1,1,1, NA), 
                                        c(1,1,1, NA),
                                        c(1,1,1, 2),
                                        c(1,1,1, 2),
                                        c(1,1,1, NA),
                                        c(1,1,1, NA)))


###############################################################################
# Country Harbour

ch.xcoord = c(1,2,
              1,2,
              1,2)

ch.ycoord = c(1, 1,
              2, 2, 
              3, 3)

ch.myLabel = c("Inner", "Inner", "Mid", "Mid", "Outer", "Outer")
ch.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

ch.leg = data.frame(ch.xcoord, ch.ycoord, ch.myLabel, ch.tidePhase)

ch.ggLeg = ggplot()+
  geom_point(data = ch.leg, aes(x = as.factor(ch.xcoord), y = as.factor(ch.ycoord), pch = ch.tidePhase, fill = ch.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("In", "Mid", "Out"))+ # Losing my mind why this is the correct order. But it is!
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
# Sober Island

sob.xcoord = c(1,2,3,
              1,2,3,
              1,2,3)

sob.ycoord = c(1, 1, 1,
              2, 2, 2, 
              3, 3, 3)

sob.myLabel = c("Missing", "Inner", "Missing", "Missing", "Mid", "Missing", "Outer", "Missing", "Outer")
sob.tidePhase = c("Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High", "Low", "Mid-Rising", "High")

sob.leg = data.frame(sob.xcoord, sob.ycoord, sob.myLabel, sob.tidePhase)

sob.ggLeg = ggplot()+
  geom_point(data = sob.leg, aes(x = as.factor(sob.xcoord), y = as.factor(sob.ycoord), pch = sob.tidePhase, fill = sob.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MR", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("In", "Mid", "Out"))+ 
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

grid.arrange(arg.withLeg, ch.withLeg, sob.withLeg, wh.withLeg)


###############################################################################

## Whitehead

# x-axis is tide phase (symbol)
wh.xcoord = c(1, 2, 3, 
            1, 2, 3, 
            1, 2, 3)

# y-axis is station (colour)
wh.ycoord = c(1, 1, 1,
              2, 2, 2, 
              3, 3, 3)

wh.myLabel = c("Inner", "Inner", "Inner", "Mid", "Mid", "Mid", "Outer", "Outer", "Outer")
wh.tidePhase = c("Low", "Mid-Falling", "High", "Low", "Mid-Falling", "High", "Low", "Mid-Falling", "High")

wh.legDf = data.frame(xcoord, ycoord, myLabel, tidePhase)

wh.ggLeg = ggplot()+
  geom_point(data = wh.legDf, aes(x = as.factor(wh.xcoord), y = as.factor(wh.ycoord), pch = wh.tidePhase, fill = wh.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2, 3), labels = c("LT", "MF", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("In", "Mid", "Out"))+ 
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
####### GULF

## Cocagne

# x-axis is tide phase (symbol)
coc.xcoord = c(1, 2, 
               1, 2, 
               1, 2)
               
# y-axis is station (colour)
coc.ycoord = c(1, 1,
               2, 2, 
               3, 3)

coc.myLabel = c("South", "South", "Mid", "Mid", "North", "North")
coc.tidePhase = c("Low", "Mid-Rising", "Low", "Mid-Rising", "Low", "Mid-Rising")

coc.legDf = data.frame(coc.xcoord, coc.ycoord, coc.myLabel, coc.tidePhase)

coc.ggLeg = ggplot()+
  geom_point(data = coc.legDf, aes(x = as.factor(coc.xcoord), y = as.factor(coc.ycoord), pch = coc.tidePhase, fill = coc.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "MR"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("South", "Mid", "North"))+ 
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

mal.myLabel = c("South", "Mid", "North")
mal.tidePhase = c("Low", "Low", "Low")

mal.legDf = data.frame(mal.xcoord, mal.ycoord, mal.myLabel, mal.tidePhase)

mal.ggLeg = ggplot()+
  geom_point(data = mal.legDf, aes(x = as.factor(mal.xcoord), y = as.factor(mal.ycoord), pch = mal.tidePhase, fill = mal.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1), labels = c("LT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("South", "Mid", "North"))+ 
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
stP.ycoord = c(1,1,
               2,2,
               3,3)

stP.myLabel = c("South", "South", "Mid", "Mid", "North", "North")
stP.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

stP.legDf = data.frame(stP.xcoord, stP.ycoord, stP.myLabel, stP.tidePhase)


stP.ggLeg = ggplot()+
  geom_point(data = stP.legDf, aes(x = as.factor(stP.xcoord), y = as.factor(stP.ycoord), pch = stP.tidePhase, fill = stP.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("South", "Mid", "North"))+ 
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

grid.arrange(coc.withLeg, mal.withLeg, stP.withLeg, ncol = 2)


###############################################################################
###############################################################################

# Pacific

# x-axis is tide phase (symbol)
pac.xcoord = c(1,2,
               1,2,
               1,2)

# y-axis is station (colour)
pac.ycoord = c(1,1,
               2,2,
               3,3)

pac.myLabel = c("Inner", "Inner", "Mid", "Mid", "Outer", "Outer")
pac.tidePhase = c("Low", "High", "Low", "High", "Low", "High")

pac.legDf = data.frame(pac.xcoord, pac.ycoord, pac.myLabel, pac.tidePhase)


pac.ggLeg = ggplot()+
  geom_point(data = pac.legDf, aes(x = as.factor(pac.xcoord), y = as.factor(pac.ycoord), pch = pac.tidePhase, fill = pac.myLabel), col = "black", size = 4)+
  scale_shape_manual(values = pchTide)+
  scale_fill_manual(values = stationCol)+ 
  scale_x_discrete(name = "Tide Phase", breaks = c(1, 2), labels = c("LT", "HT"), position = "top")+ # trick to prevent the bay label names from getting cut off
  scale_y_discrete(name = "Station", breaks = c("1", "2", "3"), labels = c("Inner", "Mid", "Outer"))+ 
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

### FIX THIS ON MONDAY
grid.arrange(pacNMDSbays[[1]], pacNMDSbays[[2]], pacNMDSbays[[3]], pac.ggLeg)
