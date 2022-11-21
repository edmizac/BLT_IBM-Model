library("here")
library("ggplot2")
library("ggspatial")
library("sf")

# Spatial objects

gua.polyg <- sf::st_read(here("Data", "Shapefiles", "Fragment polygon.shp"))
gua.sf <- ggplot2::ggplot() +
  geom_sf(data = gua.polyg,
          color = "black",
          fill = "#A9DFBF") +
  coord_sf(
    datum = sf::st_crs(our_crs),
    xlim = gua_xlim_set,    
    ylim = gua_ylim_set) +  
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .35) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)

pathshp <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize"

sma.polyg <- sf::st_read(paste0(pathshp, "/", "SantaMaria_only_rec.shp"))
sma.sf <- ggplot2::ggplot() +
  geom_sf(data = sma.polyg,
          color = "black",
          fill = "#A9DFBF") +
  coord_sf(
    datum = sf::st_crs(our_crs),
    xlim = sma_xlim_set,    
    ylim = sma_ylim_set) +  
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .35) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)


taq.polyg <- sf::st_read(paste0(pathshp, "/", "Taquara_only2.shp"))
taq.sf <- ggplot2::ggplot() +
  geom_sf(data = taq.polyg,
          color = "black",
          fill = "#A9DFBF") +
  coord_sf(
    datum = sf::st_crs(our_crs),
    xlim = taq_xlim_set,    
    ylim = taq_ylim_set) +  
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .35) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)

suz.polyg <- sf::st_read(paste0(pathshp, "/", "Suzano_polygon_unishp.shp"))
suz.sf <- ggplot2::ggplot() +
  geom_sf(data = suz.polyg,
          color = "black",
          fill = "#A9DFBF") +
  coord_sf(
    datum = sf::st_crs(our_crs),
    xlim = suz_xlim_set,    
    ylim = suz_ylim_set) +  
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .35) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)


# Save RDS
save(gua.sf, sma.sf, taq.sf, suz.sf, file = here("Data", "06_sf-plots.RData"))
