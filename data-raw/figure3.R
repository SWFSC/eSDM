# Code for creating Figure 3 for Woodman et al. (in press)

###############################################################################
library(RColorBrewer)
library(sf)
library(tmap)


###############################################################################
# Helper functions and values

### Base map
load("data/gshhg.l.L16.rda")
map.world <- gshhg.l.L16

### Study area
study.area <- st_read("inst/extdata/Shapefiles/Study_Area_CCE.shp")

### Plot extent
range.poly <- st_sfc(
  st_polygon(list(matrix(
    c(-136, -136, -115, -115, -136, 29, 51, 51, 29, 29), ncol = 2
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

### Load and extract original SDMs
load("data-raw/eSDM_ME&E_final.RDATA")
model.b <- st_transform(vals.save$models.orig[[1]], 4326)
model.h <- st_transform(vals.save$models.orig[[2]], 4326)
model.r <- st_transform(vals.save$models.orig[[3]], 4326)


#------------------------------------------------------------------------------
### Model_B
blp.b1 <- tmap_sdm_help(model.b, "Pred")
blp.b2 <- tmap_sdm_help_perc(model.b, "Pred")

# Plot of predictions (whales / km^-2)
tmap.b.obj1 <- tmap_sdm(
  model.b, "Pred", blp.b1, map.world, rpoly.mat, "Model_B",
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of SE values (with same color scheme as predictions)
tmap.b.obj2 <- tmap_sdm(
  model.b, "SE", blp.b1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("SE of predictions (whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of predictions (percentiles)
tmap.b.obj3 <- tmap_sdm(
  model.b, "Pred", blp.b2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (percentiles)"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))


#------------------------------------------------------------------------------
### Model_H
blp.h1 <- tmap_sdm_help(model.h, "Pred")
blp.h2 <- tmap_sdm_help_perc(model.h, "Pred")

# Plot of predictions (whales / km^-2)
tmap.h.obj1 <- tmap_sdm(
  model.h, "Pred", blp.h1, map.world, rpoly.mat, "Model_H",
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (habitat preference)"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of SE values (with same color scheme as predictions)
tmap.h.obj2 <- tmap_sdm(
  model.h, "SE", blp.h1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("SE of predictions (habitat preference)"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of predictions (percentiles)
tmap.h.obj3 <- tmap_sdm(
  model.h, "Pred", blp.h2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (percentiles)"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

#------------------------------------------------------------------------------
### Model_R
blp.r1 <- tmap_sdm_help(model.r, "Pred")
blp.r2 <- tmap_sdm_help_perc(model.r, "Pred")

# Plot of predictions (whales / km^-2)
tmap.r.obj1 <- tmap_sdm(
  model.r, "Pred", blp.r1, map.world, rpoly.mat, "Model_R",
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of SE values (with same color scheme as predictions)
tmap.r.obj2 <- tmap_sdm(
  model.r, "SE", blp.r1, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("SE of predictions (whales km"^-2*")"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

# Plot of predictions (percentiles)
tmap.r.obj3 <- tmap_sdm(
  model.r, "Pred", blp.r2, map.world, rpoly.mat, NA,
  main.size, leg.size, leg.width, grid.size, t.studyarea = study.area
) +
  tm_credits(expression("Predictions (percentiles)"),
             fontfamily = "sans", size = txt.size,
             position = c("left", "BOTTOM"))

#------------------------------------------------------------------------------
### Generate and save plot
tmap.list <- list(
  tmap.b.obj1, tmap.h.obj1, tmap.r.obj1,
  tmap.b.obj2, tmap.h.obj2, tmap.r.obj2,
  tmap.b.obj3, tmap.h.obj3, tmap.r.obj3
)

png("../eSDM paper/Figures_working/Fig3.png", height = 18, width = 13,
    units = "in", res = 300)
tmap_arrange(tmap.list, ncol = 3, nrow = 3, asp = NULL, outer.margins = 0.02)
dev.off()
###############################################################################
