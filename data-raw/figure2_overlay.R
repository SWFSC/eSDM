# Code for creating Figure 2 for eSDM manuscript (Woodman et al. in prep)

###################################################################################################
library(eSDM) #devtools::install_github("smwoodman/eSDM")
library(RColorBrewer)
library(sf)
library(tmap)

### Prep work
x <- st_geometry(eSDM::preds.2)
y <- st_geometry(eSDM::preds.3)

x.mid <- x[1496] #x[1496] is the 'current' base geometry polygon
overlap <- st_intersection(x.mid, y)
y.which <- st_intersects(x.mid, y)[[1]]

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
x.mid.clip <- st_intersection(x.mid, y.clip)


###################################################################################################
### Create tmap pieces of maps (polygons and other objects)
b.pal <- brewer.pal(9, "YlGnBu")

z.base <- tm_shape(x, bbox = matrix(lim.bbox, nrow = 2, byrow = TRUE)) +
  tm_polygons(col = "navajowhite2", border.col = "black", lwd = 2)

z.base.mid <- tm_shape(x.mid) +
  tm_borders(col = "dodgerblue", lwd = 3)

z.over <- tm_shape(y.clip, bbox = matrix(lim.bbox, nrow = 2, byrow = TRUE)) +
  tm_polygons(col = "grey", alpha = 0.55) +
  tm_shape(y) +
  tm_borders(col = "red", lwd = 2)

z.int.poly <- tm_shape(overlap) +
  tm_polygons(col = b.pal[2], border.col = NA, border.alpha = 0) +
  tm_shape(x.mid) +
  tm_borders(col = "dodgerblue", lwd = 3, alpha = 1) +
  tm_shape(x.mid.clip) +
  tm_borders(col = "grey", lwd = 3, alpha = 0.45) +
  tm_shape(y.clip) +
  tm_borders(col = "red", lwd = 2)

z.label <- function(x) tm_credits(x, size = 1.3, position = c("LEFT", "TOP"))

# Making inner.margins anything other than 0 creates awkward space at map edges
z.layout <- tm_layout(frame = "white", frame.lwd = 6, inner.margins = 0.0)


###################################################################################################
### Create tmap maps
f1 <- z.base + z.base.mid + z.layout + z.label("(a)")
f2 <- z.over + z.layout + z.label("(b)")
f3 <- z.base + z.base.mid + z.over + z.layout + z.label("(c)")
f4 <- z.base + z.base.mid + z.over + z.int.poly + z.layout + z.label("(d)")


###################################################################################################
### Save tmap maps
f1234 <- tmap_arrange(list(f1, f2, f3, f4), ncol = 2)
tmap_save(f1234, filename = "../eSDM paper/Figures/Fig 2.png", width = 8, height = 8)

# # Save individual panels
# tmap_save(f1, filename = "../eSDM paper/Figures/Overlay1.png", width = 4, height = 4)
# tmap_save(f2, filename = "../eSDM paper/Figures/Overlay2.png", width = 4, height = 4)
# tmap_save(f3, filename = "../eSDM paper/Figures/Overlay3.png", width = 4, height = 4)
# tmap_save(f4, filename = "../eSDM paper/Figures/Overlay4.png", width = 4, height = 4)

###################################################################################################
