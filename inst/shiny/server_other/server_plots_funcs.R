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
    x <= 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x <= 4 ~ 0.6,
    TRUE ~ 0.6
  )
  main.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x == 2 ~ 0.6,
    TRUE ~ 0.4
  )

  leg.lcm <- 2.9
  leg.txt.cex <- 0.7
  leg.mai <- c(0.3, 0, 0.2, 1)

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
# Generate plot of sf object
preview_ll <- function(sdm.ll, data.name, title.ll, perc, col.pal,
                       axis.cex, main.cex) {
  data.vec <- st_set_geometry(sdm.ll, NULL)[, data.name]

  if (perc == 1) {
    b.model <- breaks_calc(data.vec)

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, cex.axis = axis.cex
    )

  } else {
    b.model <- seq(
      from = min(data.vec, na.rm = TRUE), to = max(data.vec, na.rm = TRUE),
      length.out = 11
    )

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, cex.axis = axis.cex
    )
  }

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
  stopifnot(is.numeric(data.vec))
  sdm.cent <- suppressWarnings(st_centroid(st_combine(sdm.ll))[[1]])

  #--------------------------------------------------------
  # Common parts of leaflet map
  leaf.map <- leaflet(sdm.ll) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
    addControl(
      tags$h5(title.ll), layerId = "SDM name", position = "bottomleft") %>%
    addLayersControl(
      baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
      position = "bottomright",
      options = layersControlOptions(collapsed = FALSE)) %>%
    setView(
      lng = sdm.cent[1], lat = sdm.cent[2], zoom = 5)

  if (requireNamespace("mapview", quietly = TRUE)) {
    leaf.map <- leaf.map %>% mapview::addMouseCoordinates(style = "basic")
  }

  #--------------------------------------------------------
  # perc-specific parts of leaflet map
  if (perc == 1) {
    b.model <- breaks_calc(data.vec)
    binpal <- colorBin(col.pal, data.vec, bins = b.model, na.color = "gray")

    leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8) %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(rev(leg.labels), "NA"), opacity = 1)

  } else {
    binpal <- colorBin(col.pal, data.vec, bins = 10, pretty = FALSE,
                       na.color = "gray")
    data.breaks.vals <- format(round(seq(
      max(data.vec, na.rm = TRUE), min(data.vec, na.rm = TRUE), length.out = 11
    ), 3), justify = "right")
    data.breaks.labs <- paste(
      data.breaks.vals[2:11], "-", data.breaks.vals[1:10]
    )

    leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8) %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(data.breaks.labs, "NA") , opacity = 1)
    # addLegend(
    #   "topright", title = leg.title, pal = binpal, values = ~data.vec,
    #   opacity = 1)
  }
}

###############################################################################
