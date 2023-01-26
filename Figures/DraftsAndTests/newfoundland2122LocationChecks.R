# Read in coastline information
source("DataProcessing/studyAreaDataPrep.R")

# Read in Newfoundland metadata 
nlMetaRaw2122 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_2022_NL_Jan252023.xlsx")) %>%
  filter(sampleType == "Z") %>%
  filter(netMesh == 250)


nlMetaRaw20 = suppress_warnings(read_excel("../AMPDataFiles/FlowCamMetadata/AMP_Metadata_Plankton_2021_NL_Jan132022_OG.xlsx", sheet = "zoo")) %>%
  filter(sampleType == "Z") %>%
  filter(netMesh == 250) %>%
  filter(monthStart == 9) # only September has data



# All were punctual stations
# Need to remove the ones with NAs (these have no data, don't worry about them)
nl20WGS = st_as_sf(nlMetaRaw20, coords = c("longitude", "latitude"), crs = 4326)
nl20UTM = st_transform(nl20WGS, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


nl2122WGS = st_as_sf(nlMetaRaw2122, coords = c("longitude", "latitude"), crs = 4326)
nl2122UTM = st_transform(nl2122WGS, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))




ggplot()+
  geom_polygon(seArmCoastline, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black", linewidth = 0.1)+
  geom_sf(data = nl2122UTM, pch = 21, col = "black", fill = "orange", size = 6, alpha = 0.7)+
  geom_sf(data = nl20UTM, pch = 21, col = "black", fill = "blue", size = 6, alpha = 0.7)+
  geom_sf_text(data = nl2122UTM, aes(label = stationProgram))+
  
  
  coord_sf(xlim = c(618843, 623646), ylim = c(5464557, 5469483), crs = 32621)+ # UTM zone 21N
  annotation_scale(location = "bl", text_cex = 0.8, pad_x = unit(3.5, "cm"))+
  ggtitle("(L) Southeast Arm")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

