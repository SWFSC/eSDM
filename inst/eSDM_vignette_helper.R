# Helper functions for eSDM vignette 'example-analysis'
# By Sam Woodman, May 2019

###############################################################################
# Erase y from x
# From https://github.com/r-spatial/sf/issues/346
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


###############################################################################
# Percentile maps

#------------------------------------------------------------------------------
# Calcualte break points for relative percentages (percentiles) maps
breaks_calc <- function(x, breaks = c(seq(0.4, 0.05, by = -0.05), 0.02)) {
  x <- stats::na.omit(x)
  x <- sort(x, decreasing = TRUE)

  c(-Inf, x[ceiling(breaks * length(x))], Inf)
}


#------------------------------------------------------------------------------
# Simple plot of sf object by relative percentages (percentiles)
plot_sf_perc <- function(obj.sf, col.data = 1, map.base, round.num = 3,
                         plot.main, leg.cex = 1) {

  obj.data <- st_set_geometry(obj.sf, NULL)[, col.data]
  b.val <- breaks_calc(obj.data)

  col.pal <- c(
    "#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0",
    "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"
  )
  leg.perc.esdm <- c(
    "Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", "20 - 25%",
    "15 - 20%", "10 - 15%", "5 - 10%", "2 - 5%", "Highest 2%"
  )

  plot(
    obj.sf[col.data], axes = TRUE, border = NA,
    main = plot.main, cex.main = 1.4,
    breaks = b.val, pal = col.pal, key.pos = NULL, reset = FALSE
  )

  plot(map.base, add = TRUE, col = "tan", border = NA)

  legend("topright", legend = rev(leg.perc.esdm), col = rev(col.pal),
         pch = 15, cex = leg.cex, pt.cex = 2 * leg.cex)
}


###############################################################################
# Plot of predictions (numeric), SE values, and predictions (percentage)
# obj.sf must contain columns with names 'se' and 'cv'
plot_sf_3panel <- function(obj.sf, col.data.pred, main.txt = "",
                           map.base, plot.perc = TRUE) {

  #----------------------------------------------------------------------------
  ### Prep
  col.pal <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
  leg.col.pal <- (RColorBrewer::brewer.pal(10, "Spectral"))
  leg.cex <- 0.86
  leg.pt.cex <- 2 * leg.cex

  obj.sf <- obj.sf %>%
    rename(pred_all = !!col.data.pred) %>%
    st_transform(4326)

  # Preds - numeric
  b.val <- seq(
    from = min(obj.sf$pred_all, na.rm = TRUE), to = max(obj.sf$pred_all, na.rm = TRUE),
    length.out = 11
  )
  b.val.txt <- format(round(b.val, 3), justify = "right")
  leg.txt <- rev(paste(head(b.val.txt, -1), tail(b.val.txt, -1), sep = " - "))

  # SE
  b3.val <- b.val
  b3.val[1] <- min(b3.val, obj.sf$se, na.rm = TRUE)
  b3.val[length(b3.val)] <- max(b3.val, obj.sf$se, na.rm = TRUE)

  b3.val.txt <- format(round(b3.val, 3), justify = "right") #round(b3.val, 5)
  leg3.txt <- rev(paste(head(b3.val.txt, -1), tail(b3.val.txt, -1), sep = " - "))


  #----------------------------------------------------------------------------
  ### Plot
  mat.num <- if (plot.perc) 1:3 else 1:2
  layout(matrix(mat.num, nrow = 1))

  # Preds - numeric
  plot(
    obj.sf["pred_all"], axes = TRUE, border = NA,
    breaks = b.val, pal = col.pal, key.pos = NULL, reset = FALSE,
    main = paste0(main.txt, "Predictions"), cex.main = 1.4
  )
  plot(map.base, add = TRUE, col = "tan", border = NA)
  legend("topright", legend = leg.txt, col = leg.col.pal, pch = 15,
         cex = leg.cex, pt.cex = leg.pt.cex)
  graphics::box()

  # SE
  plot(
    obj.sf["se"], axes = TRUE, border = NA,
    breaks = b3.val, pal = col.pal, key.pos = NULL, reset = FALSE,
    main = paste0(main.txt, "SE"), cex.main = 1.4
  )
  plot(map.base, add = TRUE, col = "tan", border = NA)
  legend("topright", legend = leg3.txt, col = leg.col.pal, pch = 15,
         cex = leg.cex, pt.cex = leg.pt.cex)
  graphics::box()

  # Preds - percentage
  if (plot.perc) {
    plot_sf_perc(
      obj.sf, col.data = "pred_all", plot.main = paste0(main.txt, "Predictions"),
      leg.cex = leg.cex, map.base = map.base
    )
    graphics::box()
  }
}


###############################################################################
# Code for generating Fig 4. and Fig. 5 from Woodman et al. (in review)
# Uses tmap package to generate plots

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
                     t.alpha = 1) {
  tm_shape(t.map, bbox = t.mat, projection = 4326) +
    tm_polygons(col = "tan", border.col = NA, alpha = 1, lty = 1, lwd = 1) +

    tm_shape(tmap.obj, bbox = t.mat, projection = 4326) +
    tm_fill(col = t.col, border.col = "transparent", alpha = t.alpha,
            style = "fixed", breaks = t.blp[[1]], palette = t.blp[[3]],
            showNA =  FALSE, title = "", labels = t.blp[[2]],
            legend.is.portrait = TRUE, legend.reverse = TRUE) +
    tm_layout(bg.color = "white", legend.bg.color = "white",
              main.title = t.title,
              main.title.position = "center",
              main.title.size = t.main.size, #0.8,
              inner.margins = c(0.02, 0.02, 0, 0), outer.margins = 0.03) +
    tm_legend(show = TRUE, outside = FALSE, position = c("right", "top"),
              text.size = t.leg.size, #0.6,
              width = t.leg.width, #0.45,
              frame = "black") +
    tm_grid(x = seq(-135, -120, by = 5), y = seq(30, 50, by = 5),
            col = "grey", lwd = 1, alpha = 1,
            labels.inside.frame = FALSE, labels.size = t.grid.size, #0.6,
            labels.rot = c(0, 90),
            labels.format = list(fun = function(i) {
              parse(text = paste(i, "*degree"))
            }))
}

###############################################################################
