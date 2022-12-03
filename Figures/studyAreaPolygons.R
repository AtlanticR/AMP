# Clipping land to a bounding box!
# I definitely deleted a bunch of packages. But here's 

test = readOGR("C:/Users/FINNISS/Desktop/FWApoly/FWApoly_reduced.shp")

plot(test)

fortPoly = fortify(test)

ggplot()+
  geom_polygon(fortPoly, mapping = aes(x = long, y = lat, group=group), fill = "gray92", col = "black")+
  #coord_sf(crs = 4326)+
  #geom_sf()+
  #coord_map(xlim = c(-125.9472, -125.82), ylim = c(49.15, 49.24))+
  # Use this instead of coord_map to get the scalebar thing to work. 
  # annotation_scale needs the crs to be set here too
  coord_sf(xlim = c(-125.9472, -125.82), ylim = c(49.15, 49.24), crs = 4236)+
  #geom_spatial_point(aes(x = 49.1171, y = -125.95), crs = 4326)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14))+
  #coord_sf(crs = 4326)+
  
  #ggsn::scalebar(fortPoly, dist_unit = "km", transform = T, model = "WSG84")
  
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    height = unit(2, "cm"),
    width = unit(2, "cm"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"))+
  annotation_scale(location = "br", text_cex = 1)
                   

