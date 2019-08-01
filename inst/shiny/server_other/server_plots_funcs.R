# Functions used by server_plots.R and server_plots_download.R

###############################################################################
# Get dimensions for preview

#------------------------------------------------------------------------------
### Within the app
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

  c(plot.ncol, plot.nrow, multiplot_both(x))
}


#------------------------------------------------------------------------------
### Downloaded file
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

  c(plot.ncol, plot.nrow, multiplot_both(x))
}


#------------------------------------------------------------------------------
### Params with same dimensions for inapp and download
multiplot_both <- function(x) {
  axis.cex.curr <- case_when(
    x == 1 ~ 1.1,
    TRUE ~ 1.6
  )
  main.cex.curr <- case_when(
    x == 1 ~ 1,
    TRUE ~ 1.4
  )

  leg.lcm <- 3.5
  leg.txt.cex <- ifelse(x == 1, 1.1, 1.5)
  if (x == 1) {
    leg.mai <- c(0.42, 0, 0.24, 1.2)
  } else {
    leg.mai <- c(0.27, 0, 0.16, 1.2)
  }

  c(axis.cex.curr, main.cex.curr, leg.lcm, leg.txt.cex,
    leg.mai)
}


###############################################################################
# Helper functions called by plotting functions

#------------------------------------------------------------------------------
### Calculate break points for density intervals
# Break points are at (top): 2%, 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
breaks_calc <- function(x, breaks = c(seq(0.4, 0.05, by = -0.05), 0.02)) {
  x <- stats::na.omit(x)
  x <- sort(x, decreasing = TRUE)

  c(-Inf, x[ceiling(breaks * length(x))], Inf)
}


### Get break point and palette info given data
preview_vals_break_col <- function(data.vec) {
  data.vec.uniq <- length(unique(na.omit(data.vec)))
  d <- ifelse(data.vec.uniq > 10, 10, data.vec.uniq)
  validate(
    need(data.vec,
         paste("Error: The GUI was unable to plot the selected predictions",
               "because all of the prediction values are NA"))
  )

  b.model <- seq(
    from = min(data.vec, na.rm = TRUE), to = max(data.vec, na.rm = TRUE),
    length.out = d + 1
  )
  if (d < 3) {
    col.pal <- c("#74add1", "#f46d43")[seq_len(d)]
  } else {
    col.pal <- rev(RColorBrewer::brewer.pal(d, "Spectral"))
  }

  list(b.model = b.model, col.pal = col.pal)
}


#------------------------------------------------------------------------------
### Adapted from https://github.com/r-spatial/sf/blob/master/R/graticule.R
degreeLabelsNS_sf = function(x) {
  pos = sign(x) + 2
  dir = c("~S", "", "~N")
  paste0(abs(x), "*degree", dir[pos])
}

degreeLabelsEW_sf = function(x) {
  x <- ifelse(x > 180, x - 360, x)
  pos = sign(x) + 2
  if (any(x == -180)) pos[x == -180] = 2
  if (any(x == 180)) pos[x == 180] = 2
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

  list(
    data.frame(lon, lon.label, stringsAsFactors = FALSE),
    data.frame(lat, lat.label, stringsAsFactors = FALSE)
  )
}


#------------------------------------------------------------------------------
### Vertical legend for preview plots
preview_legend <- function(l.col.pal, l.leg.mai, l.leg.labels, l.leg.txt.cex,
                           l.perc) {
  l.col.num <- length(l.col.pal)
  l.col.num.leg <- l.col.num + 1
  l.col.pal.leg <- c("gray", l.col.pal)

  l.at <- if (l.perc == 2) c(1, seq_len(l.col.num.leg) + 0.5) else seq_len(l.col.num.leg)

  opar <- par(mai = l.leg.mai)
  on.exit(par(opar), add = TRUE)

  graphics::image(
    1, seq_len(l.col.num.leg), t(as.matrix(seq_len(l.col.num.leg))), col = l.col.pal.leg,
    axes = FALSE, xlab = "", ylab = ""
  )
  graphics::box(col = "black")
  graphics::axis(
    4, at = l.at, labels = c("NA", l.leg.labels),
    tick = FALSE, las = 1, cex.axis = l.leg.txt.cex
  )
}


###############################################################################
###############################################################################
### Plot layout of sf objects given number of rows and columns + other plot info
multiplot_layout <- function(models.toplot, data.names, plot.titles, perc.num,
                             col.pal, leg.labels, plot.ncol, plot.nrow,
                             axis.cex.curr, main.cex.curr, leg.width,
                             leg.txt.cex, leg.mai, var.key = NULL) {
  ### Inputs:
  # var.key: vector with numbers at the indicies of uncertainty plot(s)
  #   that provide the prediction plot associated with that uncertainty

  # -------------------------------------------------------
  models.num <- length(models.toplot)

  validate(
    need(models.num > 0,
         "Error: Please select at least one set of predictions to preview")
  )

  stopifnot(
    is.list(models.toplot),
    models.num == length(data.names),
    models.num == length(plot.titles),
    is.na(col.pal) | (length(col.pal) == length(leg.labels)),
    perc.num %in% c(1, 2)
  )
  on.exit(layout(1))

  # -------------------------------------------------------
  layout.num <- plot.nrow * plot.ncol
  models.layout.diff <- layout.num - models.num

  # -------------------------------------------------------
  # Create layout based on inputs
  if (perc.num == 1) {
    mat.num <- do.call(c, lapply(0:(plot.nrow - 1), function(i) {
      c((plot.ncol * i) + seq_len(plot.ncol), layout.num + 1)
    }))
    layout(
      matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol + 1, byrow = TRUE),
      width = c(rep(1, plot.ncol), lcm(leg.width))
    )

  } else {
    layout(
      matrix(seq_len(layout.num * 2), nrow = plot.nrow, ncol = plot.ncol * 2,
             byrow = TRUE),
      widths = rep(c(1, lcm(leg.width)), layout.num)
    )
  }

  # -------------------------------------------------------
  # Plot SDM previews
  for (i in seq_len(models.num)) {
    temp <- NULL

    # If plotting uncertainty (SE) numerically,
    #   use break points determined from associated predicitons
    if (isTruthy(var.key)) {
      if (!is.na(var.key[i])) {
        stopifnot(perc.num == 2)

        i.key <- var.key[i]
        temp <- preview_vals_break_col(
          st_set_geometry(models.toplot[[i.key]], NULL)[, data.names[[i.key]]]
        )

        # Adjust min and max values as necessary
        d.vec <- st_set_geometry(models.toplot[[i]], NULL)[, data.names[[i]]]
        temp[[1]][1] <- min(c(temp[[1]], d.vec), na.rm = TRUE)
        temp[[1]][length(temp[[1]])] <- max(c(temp[[1]], d.vec), na.rm = TRUE)
        rm(d.vec, i.key)
      }
    }

    preview_ll(
      models.toplot[[i]], data.names[[i]], plot.titles[[i]], perc.num, col.pal,
      axis.cex.curr, main.cex.curr, var.temp = temp
    )

    # Add a legend for each value plot
    if (perc.num == 2) {
      # If not plotting uncertainty numerically, generate 'temp'
      if (is.null(temp)) {
        temp <- preview_vals_break_col(
          st_set_geometry(models.toplot[[i]], NULL)[, data.names[[i]]]
        )
      }
      b.model <- temp[[1]]
      col.pal <- temp[[2]]

      d <- max(3, nchar(format(signif(b.model[2], 1), scientific = FALSE)) - 2)
      b.model.lab <- format(round(b.model, d), justify = "right")

      preview_legend(col.pal, leg.mai, b.model.lab, leg.txt.cex, 2)
      rm(temp, b.model, col.pal, d, b.model.lab)
    }
  }

  # Add single legend for percentages
  if (perc.num == 1) {
    # Fill in empty plots if necessary
    if (models.layout.diff != 0) {
      for (j in seq_len(models.layout.diff)) graphics::plot.new()
    }
    preview_legend(col.pal, leg.mai, leg.labels, leg.txt.cex, 1)
  }
}


###############################################################################
###############################################################################
### Generate static plot of sf object
preview_ll <- function(sdm.ll, data.name, title.ll, perc, col.pal,
                       axis.cex, main.cex, var.temp = NULL) {
  ### Inputs:
  # var.temp: output of preview_vals_break_col() call; used to keep break
  #   poitns consistent for numeric pred and SE plots

  # Convert to 0-360 longitude range if necessary
  sdm.ll <- check_preview360_split(sdm.ll)
  data.vec <- st_set_geometry(sdm.ll, NULL)[, data.name]

  # Plot predictions
  if (perc == 1) {
    b.model <- breaks_calc(data.vec)
    validate(
      need(length(unique(b.model)) >= 11,
           paste("Error: At least one of the selected predictions",
                 "(or assocaited uncertainty) does not",
                 "have enough unique prediction values to plot",
                 "a preview with a 'percentage' unit type"))
    )

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, xaxt = "n", yaxt = "n"
    )

  } else {
    # If plotting SE values numerically, use break values from assocaited preds
    if (isTruthy(var.temp)) {
      temp <- var.temp
    } else {
      temp <- preview_vals_break_col(data.vec)
    }

    b.model <- temp[[1]]
    col.pal <- temp[[2]]
    rm(temp)

    plot(
      sdm.ll[data.name], axes = TRUE, border = NA,
      breaks = b.model, pal = col.pal, key.pos = NULL, reset = FALSE,
      main = title.ll, cex.main = main.cex, xaxt = "n", yaxt = "n"
    )
  }

  # Plot axes
  z <- try(preview_ll_axes(sdm.ll), silent = TRUE)
  if (isTruthy(z)  && !(st_bbox(sdm.ll)[1] == -180 && st_bbox(sdm.ll)[3] == 180)) {
    z1 <- z[[1]]
    z2 <- z[[2]]

    if (st_is_longlat(sdm.ll)) {
      axis(1, at = z1$lon, labels = parse(text = z1$lon.label), cex.axis = axis.cex)
      axis(2, at = z2$lat, labels = parse(text = z2$lat.label), cex.axis = axis.cex)

    } else {
      axis(1, at = z1$lon, labels = z1$lon.label, cex.axis = axis.cex)
      axis(2, at = z2$lat, labels = z2$lat.label, cex.axis = axis.cex)
    }

  } else {
    axis(1, cex.axis = axis.cex)
    axis(2, cex.axis = axis.cex)
  }

  # Plot NA polys
  if (anyNA(data.vec)) {
    sdm.na <- st_geometry(sdm.ll)[is.na(data.vec)]
    plot(sdm.na, add = TRUE, border = NA, col = "gray")
  }
}


###############################################################################
###############################################################################
### Generate leaflet plot of provided sf object
preview_interactive <- function(sdm.ll, data.names, title.ll = NULL, perc,
                                col.pal, leg.labels = NULL, leg.title = NULL) {
  stopifnot(
    inherits(sdm.ll, "sf"),
    isTruthy(data.names),
    length(data.names) == 3,
    perc %in% c(1, 2),
    identical(st_crs(sdm.ll), st_crs(4326))
  )

  if (isTruthy(leg.labels) & !anyNA(col.pal) &
      length(col.pal) != length(leg.labels)) {
    stop("If 'leg.labels' is not NULL, then 'col.pal' and 'leg.labels' ",
         "must be the same length")
  }

  # Convert to 0-360 longitude range if necessary
  sdm.ll <- check_preview360_split(sdm.ll)
  sdm.df <- st_set_geometry(sdm.ll, NULL)

  data.vec <- sdm.df[, data.names[1]]
  data.vec.v <- if (!is.na(data.names[2])) sdm.df[, data.names[2]] else NULL
  data.vec.w <- if (!is.na(data.names[3])) sdm.df[, data.names[3]] else NULL
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

  if (requireNamespace("leafem", quietly = TRUE)) {
    leaf.map <- leaf.map %>% leafem::addMouseCoordinates()
  }


  #----------------------------------------------------------------------------
  if (perc == 1) {
    # Color values  based on relative percentages

    ### Predictions
    b.model <- breaks_calc(data.vec)
    validate(
      need(length(unique(b.model)) >= 11,
           paste("Error: The selected predictions do not",
                 "have enough unique prediction values to plot",
                 "an interactive preview with a 'percentage' unit type"))
    )
    binpal <- colorBin(col.pal, data.vec, bins = b.model, na.color = "gray")

    leaf.map <- leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8, group = "Predictions") %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(rev(leg.labels), "NA"), opacity = 1, group = "Predictions")


    #------------------------------------------------------
    ### Uncertainty
    if (!all(is.na(data.vec.v))) {
      b.model.v <- breaks_calc(data.vec.v)
      validate(
        need(length(unique(b.model.v)) >= 11,
             paste("Error: The selected predictions do not",
                   "have enough unique uncertainty values to plot",
                   "an interactive preview with a 'percentage' unit type"))
      )
      binpal.v <- colorBin(col.pal, data.vec.v, bins = b.model.v, na.color = "gray")

      leaf.map <- leaf.map %>%
        addPolygons(
          stroke = FALSE, color = ~binpal.v(data.vec.v), fillOpacity = 0.8, group = "Uncertainty") %>%
        addLegend(
          "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
          labels = c(rev(leg.labels), "NA"), opacity = 1, group = "Uncertainty")
    }

    #------------------------------------------------------
    ### Weights
    if (!all(is.na(data.vec.w))) {
      b.model.w <- breaks_calc(data.vec.w)
      validate(
        need(length(unique(b.model.w)) >= 11,
             paste("Error: The selected predictions do not",
                   "have enough unique weight values to plot",
                   "an interactive preview with a 'percentage' unit type"))
      )
      binpal.w <- colorBin(col.pal, data.vec.w, bins = b.model.w, na.color = "gray")

      leaf.map <- leaf.map %>%
        addPolygons(
          stroke = FALSE, color = ~binpal.w(data.vec.w), fillOpacity = 0.8, group = "Weights") %>%
        addLegend(
          "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
          labels = c(rev(leg.labels), "NA"), opacity = 1, group = "Weights")
    }

    #--------------------------------------------------------------------------
  } else {
    # Color values based on actual values

    ### Predictions
    col.pal <- preview_vals_break_col(data.vec)[[2]]
    col.num <- length(col.pal)

    temp <- preview_interactive_vals_colscheme(col.num, col.pal, data.vec)
    binpal <- temp[[1]]
    data.breaks.labs <- temp[[2]]
    rm(temp)

    leaf.map <- leaf.map %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 0.8, group = "Predictions") %>%
      addLegend(
        "topright", title = leg.title, colors = c(rev(col.pal), "gray"),
        labels = c(data.breaks.labs, "NA"), opacity = 1, group = "Predictions")


    ### Uncertainty - same color scale as predictions
    if (isTruthy(data.vec.v)) {
      #####
      # browser()

      validate(
        need(col.num >= 10, "Error: Error plotting uncertainty for selected predictions")
      )

      b.new <- seq(min(data.vec, na.rm = TRUE), max(data.vec, na.rm = TRUE), length.out = 11)
      b.new[1] <- min(c(data.vec.v, data.vec), na.rm = TRUE)
      b.new[11] <- max(c(data.vec.v, data.vec), na.rm = TRUE)


      binpal.v <- colorBin(
        col.pal, data.vec.v, bins = b.new, pretty = FALSE, na.color = "gray"
      )
      data.breaks.vals <- b.new
      d <- max(3, nchar(format(signif(tail(data.breaks.vals, 2)[2], 1), scientific = FALSE)) - 2)
      data.breaks.vals <- format(round(data.breaks.vals, d), justify = "right")
      data.breaks.labs <- paste(
        tail(data.breaks.vals, -1), "-", head(data.breaks.vals, -1)
      )

      leaf.map <- leaf.map %>%
        addPolygons(
          stroke = FALSE, color = ~binpal.v(data.vec.v), fillOpacity = 0.6, group = "Uncertainty") %>%
        addLegend(
          "topright", title = "Uncertainty", colors = c(rev(col.pal), "gray"),
          labels = c(data.breaks.labs, "NA"), opacity = 1, group = "Uncertainty")
    }

    ### Weights
    if (isTruthy(data.vec.w)) {
      data.vec.w.uniq <- length(unique(na.omit(data.vec.w)))
      w.num <- ifelse(data.vec.w.uniq > 10, 10, data.vec.w.uniq)
      w.pal <- viridis::viridis(w.num)

      w.temp <- preview_interactive_vals_colscheme(w.num, w.pal, data.vec.w)
      w.binpal <- w.temp[[1]]
      data.breaks.labs.w <- w.temp[[2]]
      rm(w.temp)

      leaf.map <- leaf.map %>%
        addPolygons(
          stroke = FALSE, color = ~w.binpal(data.vec.w), fillOpacity = 0.6, group = "Weights") %>%
        addLegend(
          "topright", title = "Weights", colors = c(rev(w.pal), "gray"),
          labels = c(data.breaks.labs.w, "NA"), opacity = 1, group = "Weights")
    }
  }

  #----------------------------------------------------------------------------
  ### Add messages and layer control
  if (isTruthy(data.vec.v) & isTruthy(data.vec.w)) {
    leaf.map %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = c("Predictions", "Uncertainty", "Weights"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Uncertainty", "Weights"))

  } else if (isTruthy(data.vec.v)) {
    leaf.map %>%
      addControl(
        tags$h5("No weight data"), layerId = "Other info", position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = c("Predictions", "Uncertainty"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Uncertainty")

  } else if (isTruthy(data.vec.w)) {
    leaf.map %>%
      addControl(
        tags$h5("No uncertainty data"), layerId = "Other info", position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = c("Predictions", "Weights"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Weights")

  } else {
    # No uncertainty data; include message
    leaf.map %>%
      addControl(
        tags$h5("No uncertainty or weight data"), layerId = "Other info", position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  }
}


###############################################################################
### Get binpal and legend labels for values plot (not percentages plot)
preview_interactive_vals_colscheme <- function(col.num, col.pal, data.vec) {
  if (col.num < 10) {
    binpal <- colorNumeric(col.pal, data.vec, na.color = "gray")
    data.breaks.labs <- format(
      signif(sort(unique(na.omit(data.vec)), decreasing = TRUE), 3),
      justify = "right"
    )

  } else {
    binpal <- colorBin(
      col.pal, data.vec, bins = 10, pretty = FALSE, na.color = "gray"
    )
    data.breaks.vals <- seq(
      max(data.vec, na.rm = TRUE), min(data.vec, na.rm = TRUE),
      length.out = col.num + 1
    )
    d <- max(3, nchar(format(signif(tail(data.breaks.vals, 2)[2], 1), scientific = FALSE)) - 2)
    data.breaks.vals <- format(round(data.breaks.vals, d), justify = "right")
    data.breaks.labs <- paste(
      tail(data.breaks.vals, -1), "-", head(data.breaks.vals, -1)
    )
  }

  list(binpal, data.breaks.labs)
}

###############################################################################
