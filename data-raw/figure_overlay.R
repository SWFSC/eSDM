###################################################################################################
# Code for generating figure 4 for eSDM paper

library(eSDM)
library(RColorBrewer)
library(sf)
library(tmap)

# mapview::mapview(preds.3)
# display.brewer.pal(9, "YlGnBu")

x <- st_geometry(preds.2)
y <- st_geometry(preds.3)

# x[1496] is the base geometry polygon
overlap <- st_intersection(x[1496], y)
y.which <- st_intersects(x[1496], y)[[1]]

lim.xmin <- -120.022
lim.xmax <- -119.777
lim.ymin <- 34.3
lim.ymax <- 34.5

lim.bbox <- c(lim.xmin, lim.xmax, lim.ymin, lim.ymax)
lim.poly <- c(lim.xmin, lim.xmax, lim.xmax, lim.xmin, lim.xmin,
              lim.ymin, lim.ymin, lim.ymax, lim.ymax, lim.ymin)
rm(lim.xmin, lim.xmax, lim.ymin, lim.ymax)

area.poly <- st_sfc(st_polygon(list(matrix(lim.poly, ncol = 2))), crs = st_crs(x))

y.clip <- st_crop(y, area.poly)


###################################################################################################
b.pal <- brewer.pal(9, "YlGnBu")
######## TODO fix whitespace

z.base <- tm_shape(x, bbox = matrix(lim.bbox, nrow = 2, byrow = TRUE)) +
  tm_polygons(col = "navajowhite2", border.col = "black", lwd = 2)

z.base.mid <- tm_shape(x[1496]) +
  tm_borders(col = "dodgerblue", lwd = 3)

z.over <- tm_shape(y.clip, bbox = matrix(lim.bbox, nrow = 2, byrow = TRUE)) +
  tm_polygons(col = "grey", alpha = 0.55) +
  tm_shape(y) +
  tm_borders(col = "red", lwd = 2)

z.int.poly <- tm_shape(overlap) +
  tm_polygons(col = b.pal[2], border.col = NA) +
  tm_shape(x[1496]) +
  tm_borders(col = "dodgerblue", lwd = 3) +
  tm_shape(y.clip) +
  tm_borders(col = "red", lwd = 2)

z.int.other <- tm_shape(y[y.which]) +
  tm_borders(col = "forestgreen", lwd = 3)

z.layout <- tm_layout(frame = "white", inner.margins = 0)
###################################################################################################
f1 <- z.base + z.base.mid + z.layout
f2 <- z.over + z.layout
f3 <- z.base + z.base.mid + z.over + z.layout
f4 <- z.base + z.base.mid + z.over + z.int.poly + z.layout


###################################################################################################
tmap_save(f1, filename = "../eSDM paper/Figures/Overlay1.png", width = 4, height = 4)
tmap_save(f2, filename = "../eSDM paper/Figures/Overlay2.png", width = 4, height = 4)
tmap_save(f3, filename = "../eSDM paper/Figures/Overlay3.png", width = 4, height = 4)
tmap_save(f4, filename = "../eSDM paper/Figures/Overlay4.png", width = 4, height = 4)

###################################################################################################
