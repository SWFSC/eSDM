###############################################################################
# Code for generating Figs 3, 4, and 5 from Woodman et al.
# Uses tmap package to generate plots

# tmap_sdm_help* are copied directly from eSDM_vignette_helper.R
#   tmap_sdm was adapted to be able to plot study area polygon

#------------------------------------------------------------------------------
# Return a list with the break point values, legend text, and color palette
tmap_sdm_help <- function(x, x.col, pal.len = 10) {
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


#------------------------------------------------------------------------------
# Calcualte break points for relative percentages (percentiles) maps
breaks_calc <- function(x, breaks = c(seq(0.4, 0.05, by = -0.05), 0.02)) {
  x <- stats::na.omit(x)
  x <- sort(x, decreasing = TRUE)

  c(-Inf, x[ceiling(breaks * length(x))], Inf)
}


#------------------------------------------------------------------------------
# Return a list with the break point values, legend text, and color palette
# For pecentile plot
tmap_sdm_help_perc <- function(x, x.col, pal.len = 10) {
  col.pal <- c(
    "#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0",
    "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"
  )
  leg.txt <- c(
    "Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", "20 - 25%",
    "15 - 20%", "10 - 15%", "5 - 10%", "2 - 5%", "Highest 2%"
  )

  x <- st_set_geometry(x, NULL)
  b.val <- breaks_calc(x[, x.col])

  list(b.val, leg.txt, col.pal)
}


#------------------------------------------------------------------------------
# Create tmap maps
tmap_sdm <- function(tmap.obj, t.col, t.blp, t.map, t.mat, t.title,
                     t.main.size, t.leg.size, t.leg.width, t.grid.size,
                     t.alpha = 1, t.grid.col = "black",
                     t.ticks = TRUE, t.lines = FALSE, t.studyarea = NULL) {

  tmap.curr <- tm_shape(tmap.obj, bbox = t.mat, projection = 4326) +
    tm_fill(col = t.col, border.col = "transparent", alpha = t.alpha,
            style = "fixed", breaks = t.blp[[1]], palette = t.blp[[3]],
            showNA =  FALSE, title = "", labels = t.blp[[2]],
            legend.is.portrait = TRUE, legend.reverse = TRUE) +
    tm_layout(bg.color = "white", legend.bg.color = "white",
              main.title = t.title,
              main.title.position = "center",
              main.title.size = t.main.size,
              inner.margins = c(0.02, 0.02, 0, 0), outer.margins = 0.03) +
    tm_legend(show = TRUE, outside = FALSE, position = c("right", "top"),
              text.size = t.leg.size,
              width = t.leg.width,
              frame = "black") +
    tm_graticules(x = seq(-135, -120, by = 5), y = seq(30, 50, by = 5),
                  col = t.grid.col, lwd = 1, alpha = 1,
                  ticks = t.ticks, lines = t.lines,
                  labels.inside.frame = FALSE, labels.size = t.grid.size,
                  labels.rot = c(0, 90))

  if (!is.null(t.studyarea)) {
    tmap.curr +
      tm_shape(t.studyarea, bbox = t.mat, projection = 4326) +
      tm_polygons(col = NA, border.col = "red", alpha = 0, lty = 1, lwd = 1) +
      tm_shape(t.map, bbox = t.mat, projection = 4326) +
      tm_polygons(col = "tan", border.col = NA, border.alpha = 0, alpha = 1, lty = 1, lwd = 1)
  } else {
    tmap.curr +
      tm_shape(t.map, bbox = t.mat, projection = 4326) +
      tm_polygons(col = "tan", border.col = NA, border.alpha = 0, alpha = 1, lty = 1, lwd = 1)
  }
}

###############################################################################
