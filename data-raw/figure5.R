# Code for creating Figure 5 for Woodman et al. (in press)

###############################################################################
library(RColorBrewer)
library(sf)
library(tmap)


###############################################################################
# Helper functions and values

### Base map
load("data/gshhg.l.L16.rda")
map.world <- gshhg.l.L16

### Plot extent
range.poly <- st_sfc(
  st_polygon(list(matrix(
    c(-132, -132, -116, -116, -132, 29.5, 49, 49, 29.5, 29.5), ncol = 2
  ))),
  crs = 4326
)
rpoly.mat <- matrix(st_bbox(range.poly), ncol = 2)

### Size of text and legend width
main.size <- 1.4
leg.size  <- 1
leg.width <- 1
grid.size <- 1
txt.size  <- 1.2

### Plotting functions
source("data-raw/figure_plot.R", local = TRUE, echo = FALSE)


###############################################################################
# Create and save figure

### Load tss-weighted ensemble and validation data
load("data-raw/ens_sf_wtss.rda")
load("data-raw/valid_sf_pres.rda")

blp1 <- tmap_sdm_help(ens.sf.wtss, "Pred_ens")
blp2 <- tmap_sdm_help_perc(ens.sf.wtss, "Pred_ens")

#------------------------------------------------------------------------------
# Plot of predictions (whales / km^-2)
tmap.obj1 <- tmap_sdm(
  ens.sf.wtss, "Pred_ens", blp1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
) +
  tm_credits(expression("Predictions\n(whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "bottom"))

# Plot of SE values (with same color scheme as predictions)
tmap.obj2 <- tmap_sdm(
  ens.sf.wtss, "SE_ens", blp1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
) +
  tm_credits(expression("SE of\npredictions\n(whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "bottom"))

# Plot of predictions (percentiles)
tmap.obj3 <- tmap_sdm(
  ens.sf.wtss, "Pred_ens", blp2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
) +
  tm_credits("Predictions\n(percentiles)",
             fontfamily = "sans", size = txt.size,
             position = c("left", "bottom"))

# Plot of predictions (percentiles) with presence points
tmap.obj4 <- tmap_sdm(
  ens.sf.wtss, "Pred_ens", blp2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
) +
  tm_shape(st_geometry(valid.sf.pres)) +
  tm_dots(col = "black", shape = 19, size = 0.1, legend.show = FALSE) +
  tm_credits("Predictions\n(percentiles)",
             fontfamily = "sans", size = txt.size,
             position = c("left", "bottom"))


#------------------------------------------------------------------------------
### Generate and save plot
png("../eSDM paper/Figures_working/Fig5.png", height = 13.5, width = 8.6,
    units = "in", res = 300)
tmap_arrange(
  list(tmap.obj1, tmap.obj2, tmap.obj3, tmap.obj4),
  ncol = 2, nrow = 2, asp = NULL, outer.margins = 0.02
)
dev.off()

###############################################################################
