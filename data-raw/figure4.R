# Code for creating Figure 4 for eSDM manuscript (Woodman et al. in prep)

###################################################################################################
library(dplyr)
library(eSDM) #devtools::install_github("smwoodman/eSDM")
library(maps)
library(RColorBrewer)
library(sf)
library(tmap)

load("../eSDM-data/Ignore/Example analysis workspaces/eSDM_bwPredAll_ens_maps.RDATA")


###################################################################################################
# Values
map.world <- st_geometry(
  st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
)
range.poly <- st_sfc(
  st_polygon(list(matrix(
    c(-132, -132, -116, -116, -132, 29.5, 49, 49, 29.5, 29.5), ncol = 2
  ))),
  crs = 4326
)
rpoly.mat <- matrix(st_bbox(range.poly), ncol = 2)


###################################################################################################
# Helper functions

### Return a list with the break point values, legend text, and color palette
blp.calc <- function(x, x.col, pal.len = 10) {
  x <- st_set_geometry(x, NULL)
  b.val <- seq(
    from = min(x[, x.col], na.rm = TRUE), to = max(x[, x.col], na.rm = TRUE),
    length.out = pal.len + 1
  )
  b.val[1] <- 0
  col.pal <- rev(RColorBrewer::brewer.pal(pal.len, "Spectral"))

  b.val.txt <- format(round(b.val, 3), justify = "right") #round(b.val, 5)
  leg.txt <- paste(head(b.val.txt, -1), tail(b.val.txt, -1), sep = " - ")

  list(b.val, leg.txt, col.pal)
}

### Create tmap maps
tmap.func.help <- function(tmap.obj, t.col, t.breaks, t.pal, t.leg.txt,
                           t.map, t.mat, t.title) {
  tm_shape(t.map, bbox = t.mat, projection = 4326) +
    tm_polygons(col = "tan", border.col = NA, alpha = 1, lty = 1, lwd = 1) +

    tm_shape(tmap.obj, bbox = t.mat, projection = 4326) +
    tm_fill(col = t.col, border.col = "transparent",
            style = "fixed", breaks = t.breaks, palette = t.pal,
            # colorNA = NA, textNA = "NA", showNA = NA,
            showNA =  FALSE, title = "", labels = t.leg.txt,
            legend.is.portrait = TRUE, legend.reverse = TRUE) +


    tm_layout(bg.color = "white", legend.bg.color = "white",
              main.title = t.title,
              main.title.position = "center",
              main.title.size = 1.4,
              inner.margins = c(0.02, 0.02, 0, 0), outer.margins = 0.03) +
    tm_legend(show = TRUE, outside = FALSE, position = c("right", "top"),
              text.size = 1, width = 1,
              frame = "black") +
    tm_grid(x = seq(-135, -120, by = 5), y = seq(30, 50, by = 5),
            col = "grey", lwd = 1, alpha = 1,
            labels.inside.frame = FALSE, labels.size = 1, labels.rot = c(0, 90),
            labels.format = list(fun = function(i) parse(text = paste(i, "*degree"))))
}


###################################################################################################
# Create and save map

### Confirm that first ensemble is unweighted
stopifnot(vals.save$ensemble.method[1] == "Unweighted")

### Get rescaled values and create ensemble sf object
over.resc.df <- vals.save$ensemble.overlaid.res[[1]]

e1 <- over.resc.df %>%
  transmute(pred_all = apply(over.resc.df, 1, mean, na.rm = TRUE),
            se = apply(over.resc.df, 1, sd, na.rm = TRUE),
            cv = se / pred_all)  %>%
  st_sf(geometry = vals.save$overlay.base.sfc, agr = "constant")

blp1 <- blp.calc(e1, "pred_all")
blp2 <- blp.calc(e1, "cv")

### Save map - use tmap for same map style as GUI
png("../eSDM paper/Figures/Fig 4.png", height = 7, width = 13, units = "in", res = 300)
tmap.obj1 <- tmap.func.help(
  e1, "pred_all", blp1[[1]], blp1[[3]], blp1[[2]],
  map.world, rpoly.mat, "Unweighted ensemble - predictions"
)
tmap.obj2 <- tmap.func.help(
  e1, "se", blp1[[1]], blp1[[3]], blp1[[2]],
  map.world, rpoly.mat, "Unweighted ensemble - SE"
)
tmap.obj3 <- tmap.func.help(
  e1, "cv", blp2[[1]], blp2[[3]], blp2[[2]],
  map.world, rpoly.mat, "Unweighted ensemble - CV"
)
tmap_arrange(
  list(tmap.obj1, tmap.obj2, tmap.obj3), ncol = 3, asp = NULL, outer.margins = 0.05
)
dev.off()

###################################################################################################
