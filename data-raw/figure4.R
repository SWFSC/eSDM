# Code for creating Figure 4 for Woodman et al. (in review)

###############################################################################
library(maps)
library(RColorBrewer)
library(sf)
library(tmap)


###############################################################################
# Helper functions and values

### Base map
map.world <- st_geometry(
  st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
)

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

### Plotting functions
source(
  system.file("vignette_helper.R", package = "eSDM"),
  local = TRUE, echo = FALSE
)


###############################################################################
# Create and save map

### Load unweighted ensemble and prep plots
# rda file created in example-analysis.Rmd vignette
load("data-raw/ens_sf_unw.rda")

blp1 <- tmap.sdm.help(ens.sf.unw, "Pred_ens")
blp2 <- tmap.sdm.help(ens.sf.unw, "CV")

# Plot of predictions (whales / km^-2)
tmap.obj1 <- tmap.sdm(
  ens.sf.unw, "Pred_ens", blp1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
)
# Plot of SE values (with same color sceme as predictions)
tmap.obj2 <- tmap.sdm(
  ens.sf.unw, "SE", blp1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
)
# Plot of CV values
tmap.obj3 <- tmap.sdm(
  ens.sf.unw, "CV", blp2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size
)


### Generate and save plot
png("../eSDM paper/Figures/Fig 4.png", height = 6.9, width = 13,
    units = "in", res = 300)
tmap_arrange(
  list(tmap.obj1, tmap.obj2, tmap.obj3), ncol = 3, asp = NULL,
  outer.margins = 0.05
)
dev.off()

###############################################################################
