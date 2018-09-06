###############################################################################
# Get dimensions for eSDM preview within the app
multiplot_inapp <- function(x) {
  plot.ncol <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )
  plot.nrow <- case_when(
    x <= 3 ~ 1,
    x <= 6 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    x == 1 ~ 1.0,
    TRUE ~ 1.3
  )
  main.cex.curr <- case_when(
    x == 1 ~ 1,
    TRUE ~ 1.2
  )

  leg.lcm <- 3.0
  leg.txt.cex <- ifelse(x == 1, 0.8, 1.3)
  if (x == 1) {
    leg.mai <- c(0.42, 0, 0.24, 1)
  } else {
    leg.mai <- c(0.27, 0, 0.16, 1)
  }

  c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm, leg.txt.cex,
    leg.mai)
}


###############################################################################
# Get dimensions for eSDM preview being downloaded
multiplot_download <- function(x) {
  plot.ncol <- case_when(
    x <= 2 ~ 1,
    x <= 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )
  plot.nrow <- case_when(
    x == 1 ~ 1,
    x <= 6 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    x == 1 ~ 1.2,
    x <= 4 ~ 1.3,
    TRUE ~ 1.0
  )

  main.cex.curr <- case_when(
    x == 1 ~ 1.0,
    x == 2 ~ 1.3,
    TRUE ~ 1.3
  )

  leg.lcm <- 3.0
  leg.txt.cex <- ifelse(x == 1, 0.8, 1.3)
  if (x == 1) {
    leg.mai <- c(0.42, 0, 0.24, 1)
  } else {
    leg.mai <- c(0.27, 0, 0.16, 1)
  }

  c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm, leg.txt.cex,
    leg.mai)
}


###############################################################################
# Plot layout of sf objects given number of rows and columns + other plot info
multiplot_layout <- function(models.toplot, data.names, plot.titles, perc.num,
                             col.pal, leg.labels, plot.ncol, plot.nrow,
                             axis.cex.curr, main.cex.curr, leg.width,
                             leg.txt.cex, leg.mai) {

  # -------------------------------------------------------
  stopifnot(
    is.list(models.toplot),
    length(models.toplot) == length(data.names),
    length(models.toplot) == length(plot.titles),
    length(col.pal) == length(leg.labels),
    perc.num %in% c(1, 2)
  )
  on.exit(layout(1))

  # -------------------------------------------------------
  models.num <- length(models.toplot)
  col.num <- length(col.pal)
  col.num.leg <- col.num + 1
  col.pal.leg <- c("gray", col.pal)
  layout.num <- plot.nrow * plot.ncol
  models.layout.diff <- layout.num - models.num

  # -------------------------------------------------------
  # Create layout based on inputs
  if (perc.num == 1) {
    mat.num <- do.call(c, lapply(0:(plot.nrow - 1), function(i) {
      c((plot.ncol * i) + 1:plot.ncol, layout.num + 1)
    }))
    layout(
      matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol + 1, byrow = TRUE),
      width = c(rep(1, plot.ncol), lcm(leg.width))
    )

  } else {
    layout(
      matrix(1:(layout.num * 2), nrow = plot.nrow, ncol = plot.ncol * 2,
             byrow = TRUE),
      widths = rep(c(1, lcm(leg.width)), layout.num)
    )
  }

  # -------------------------------------------------------
  # Plot SDM previews
  for (i in 1:models.num) {
    preview_ll(
      models.toplot[[i]], data.names[[i]], plot.titles[[i]], perc.num, col.pal,
      axis.cex.curr, main.cex.curr
    )

    # Add a legend for each value plot
    if (perc.num == 2) {
      data.vec <- st_set_geometry(models.toplot[[i]], NULL)[, data.names[[i]]]
      b.model <- seq(
        from = min(data.vec, na.rm = TRUE), to = max(data.vec, na.rm = TRUE),
        length.out = 11
      )
      b.model.lab <- format(round(b.model, 3), justify = "right")

      opar <- par(mai = leg.mai)
      on.exit(par(opar), add = TRUE)

      graphics::image(
        1, 1:col.num.leg, t(as.matrix(1:col.num.leg)), col = col.pal.leg,
        axes = FALSE, xlab = "", ylab = ""
      )
      graphics::box(col = "black")
      graphics::axis(
        4, at = c(1, (1:col.num.leg) + 0.5),
        labels = c("NA", b.model.lab),
        tick = FALSE, las = 1, cex.axis = leg.txt.cex
      )
    }
  }

  # Add single legend for percentages
  if (perc.num == 1) {
    # Fill in empty plots if necessary
    if (models.layout.diff != 0) {
      for (j in 1:models.layout.diff) graphics::plot.new()
    }

    opar <- par(mai = leg.mai)
    on.exit(par(opar), add = TRUE)

    graphics::image(
      0.6, 1:col.num.leg, t(as.matrix(1:col.num.leg)), col = col.pal.leg,
      axes = FALSE, xlab = "", ylab = ""
    )
    graphics::box(col = "black")
    graphics::axis(
      4, at = 1:col.num.leg, labels = c("NA", leg.labels),
      tick = FALSE, las = 1, cex.axis = leg.txt.cex
    )
  }
}


###############################################################################
### Adapted from the sf package, https://github.com/r-spatial/sf/blob/master/R/graticule.R
degreeLabelsNS_sf = function(x) {
  pos = sign(x) + 2
  dir = c("~S", "", "~N")
  paste0(abs(x), "*degree", dir[pos])
}

degreeLabelsEW_sf = function(x) {
  x <- ifelse(x > 180, x - 360, x)
  pos = sign(x) + 2
  if (any(x == -180))
    pos[x == -180] = 2
  if (any(x == 180))
    pos[x == 180] = 2
  dir = c("~W", "", "~E")
  paste0(abs(x), "*degree", dir[pos])
}

### Based off of st_graticule, for generating coordinates
preview_ll_axes <- function(x) {
  stopifnot(inherits(x, c("sf", "sfc")))

  bb <- st_bbox(x)
  lon <- pretty(bb[c(1, 3)], n = 4)
  lat <- pretty(bb[c(2, 4)], n = 4)

  if (st_is_longlat(x)) {
    lon.label <- degreeLabelsEW_sf(lon)
    lat.label <- degreeLabelsNS_sf(lat)
  } else {
    lon.label <- lon
    lat.label <- lat
  }

  stopifnot(length(lon.label) == length(lon), length(lat.label) == length(lat))

  data.frame(lon, lon.label, lat, lat.label, stringsAsFactors = FALSE)
}



###############################################################################
# Generate plot of sf object
preview_ll <- function(sdm.ll, data.name, title.ll, perc, col.pal,
                       axis.cex, main.cex) {
  data.vec <- st_set_geometry(sdm.ll, NULL)[, data.name]

  if (perc == 1) {
    b.model <- breaks_calc(data.vec)

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, xaxt = "n", yaxt = "n"
    )

  } else {
    b.model <- seq(
      from = min(data.vec, na.rm = TRUE), to = max(data.vec, na.rm = TRUE),
      length.out = 11
    )

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, xaxt = "n", yaxt = "n"
    )
  }

  # Plot axes
  z <- preview_ll_axes(sdm.ll)
  if (st_is_longlat(sdm.ll)) {
    axis(1, at = z$lon, labels = parse(text = z$lon.label), cex.axis = axis.cex)
    axis(2, at = z$lat, labels = parse(text = z$lat.label), cex.axis = axis.cex)

  } else {
    axis(1, at = z$lon, labels = z$lon.label, cex.axis = axis.cex)
    axis(2, at = z$lat, labels = z$lat.label, cex.axis = axis.cex)
  }

  # Plot NA polys
  if (anyNA(data.vec)) {
    sdm.na <- st_geometry(sdm.ll)[is.na(data.vec)]
    plot(sdm.na, add = TRUE, border = NA, col = "gray")
  }
}


###############################################################################
# Generate leaflet plot of provided sf object
preview_interactive <- function(sdm.ll, data.name, title.ll = NULL, perc,
                                col.pal, leg.labels = NULL, leg.title = NULL) {
  stopifnot(
    inherits(sdm.ll, "sf"),
    !is.null(sdm.ll[data.name]),
    perc %in% c(1, 2),
    identical(st_crs(sdm.ll), st_crs(4326))
  )
  if (!is.null(leg.labels) & length(col.pal) != length(leg.labels)) {
    stop("If 'leg.labels' is not NULL, then 'col.pal' and 'leg.labels' ",
         "must be the same length")
  }

  data.vec <- st_set_geometry(sdm.ll[data.name], NULL)[, 1]
  data.vec.w <- if (ncol(sdm.ll) > 2) st_set_geometry(sdm.ll, NULL)[, 2] else NA
  stopifnot(is.numeric(data.vec))
  sdm.cent <- suppressWarnings(st_centroid(st_combine(sdm.ll))[[1]])


  #----------------------------------------------------------------------------
  # Common parts of leaflet map
  leaf.map <- leaflet(sdm.ll) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
    addControl(tags$h5(title.ll), layerId = "SDM name", position = "bottomleft") %>%
    setView(lng = sdm.cent[1], lat = sdm.cent[2], zoom = 5)

  if (requireNamespace("mapview", quietly = TRUE)) {
    leaf.map <- leaf.map %>% mapview::addMouseCoordinates(style = "basic")
  }


  #----------------------------------------------------------------------------
  if (perc == 1) {
    # Color prediction based on relative percentages
    b.model <- breaks_calc(data.vec)
    binpal <- colorBin(col.pal, data.vec, bins = b.model, na.color = "gray")

    leaf.map <- leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8, group = "Predictions") %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(rev(leg.labels), "NA"), opacity = 1, group = "Predictions")

  } else {
    # Color predictions based on actual values
    binpal <- colorBin(
      col.pal, data.vec, bins = 10, pretty = FALSE, na.color = "gray"
    )
    data.breaks.vals <- format(round(seq(
      max(data.vec, na.rm = TRUE), min(data.vec, na.rm = TRUE), length.out = 11
    ), 3), justify = "right")
    data.breaks.labs <- paste(
      data.breaks.vals[2:11], "-", data.breaks.vals[1:10]
    )

    leaf.map <- leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8, group = "Predictions") %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(data.breaks.labs, "NA"), opacity = 1, group = "Predictions")
  }


  #----------------------------------------------------------------------------
  if (all(is.na(data.vec.w))) {
    # No weight data; include message
    leaf.map %>%
      addControl(
        tags$h5("No weight data"), layerId = "Weight info", position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE))


  } else {
    # Incldue weight data
    pal.w <- viridis::viridis(10)
    binpal <- colorBin(
      pal.w, data.vec.w, bins = 10, pretty = FALSE, na.color = "gray"
    )
    data.breaks.vals.w <- format(round(seq(
      max(data.vec.w, na.rm = TRUE), min(data.vec.w, na.rm = TRUE), length.out = 11
    ), 3), justify = "right")
    data.breaks.labs.w <- paste(
      data.breaks.vals.w[2:11], "-", data.breaks.vals.w[1:10]
    )

    leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec.w), fillOpacity = 0.6, group = "Weights") %>%
      addLegend(
        "topright", title = "Weights", colors = rev(pal.w),
        labels = data.breaks.labs.w, opacity = 1, group = "Weights") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = c("Predictions", "Weights"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Weights")
  }
}

###############################################################################
