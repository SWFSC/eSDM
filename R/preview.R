#' title
#'
#' General function for plotting sf object
#'
#' @param models.toplot list of sf objects
#'
#' @export

multiplot_layout <- function(models.toplot, data.names, plot.titles, perc.num,
                             col.pal, leg.labels, plot.ncol, plot.nrow,
                             axis.cex.curr, main.cex.curr, leg.width = 3) {

  # -------------------------------------------------------
  # TODO long-term: make more flexible for use as stand-alone function
  stopifnot(
    is.list(models.toplot),
    length(models.toplot) == length(data.names),
    length(models.toplot) == length(plot.titles),
    length(col.pal) == length(leg.labels),
    perc.num %in% c(1, 2)
  )

  # -------------------------------------------------------
  models.num <- length(models.toplot)
  col.num <- length(col.pal)

  if (perc.num == 1) {
    mat.num <- c(1:models.num, rep(1 + models.num, plot.nrow))
    layout(matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol + 1),
           width = c(rep(1, plot.ncol), lcm(leg.width)))

  } else {
    layout(matrix(1:(models.num * 2), nrow = plot.nrow, ncol = plot.ncol * 2,
                  byrow = TRUE),
           widths = rep(c(1, lcm(leg.width)), models.num))
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
      b.model <- seq(from = min(data.vec, na.rm = TRUE),
                     to = max(data.vec, na.rm = TRUE),
                     length.out = 11)

      opar <- par(mai = c(0.3, 0, 0.2, 1))
      on.exit(par(opar), add = TRUE)

      graphics::image(1, 1:col.num, t(as.matrix(1:col.num)), col = col.pal,
                      axes = FALSE, xlab = "", ylab = "")
      graphics::box(col = "black")
      temp <- ifelse(models.num == 1, 0.8, 1.3) # not sure why this is necessary
      graphics::axis(4, at = (0:col.num) + 0.5, labels = round(b.model, 5),
                     tick = FALSE, las = 1, cex.axis = temp)
    }
  }

  # Add single legend for percentages
  if (perc.num == 1) {
    opar <- par(mai = c(0.3, 0, 0.2, 1))
    on.exit(par(opar), add = TRUE)

    graphics::image(0.6, 1:col.num, t(as.matrix(1:col.num)), col = col.pal,
                    axes = FALSE, xlab = "", ylab = "")
    graphics::box(col = "black")
    temp <- ifelse(models.num == 1, 0.8, 1.3) # not sure why this is necessary
    graphics::axis(4, at = 1:col.num, labels = leg.labels, tick = FALSE,
                   las = 1, cex.axis = temp)
  }

  # -------------------------------------------------------
  # Reset layout
  layout(1)
}


#' title
#'
#' General function for plotting sf object
#'
#' @param sf.ll sf object in crs.ll
#'
#' @export

preview_ll <- function(sf.ll, data.name, title.ll, perc, col.pal,
                       axis.cex, main.cex) {
  ### Prep:
  data.vec <- st_set_geometry(sf.ll, NULL)[, data.name]

  ### Generate plot with densities color-coded by percentages or values
  if (perc == 1) {
    b.model <- breaks_calc(data.vec)

    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.pal,
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex,
         key.pos = NULL, reset = FALSE)
    #graticule = st_crs(sf.ll),

  } else {
    b.model <- seq(from = min(data.vec, na.rm = TRUE),
                   to = max(data.vec, na.rm = TRUE),
                   length.out = 11)

    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.pal,
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex,
         key.pos = NULL, reset = FALSE)
  }
}


#' title
#'
#' General function for plotting sf object
#'
#' @param sf.ll sf object in crs.ll
#'
#' @export

preview_interactive <- function(sf.ll, data.name, perc, col.pal,
                                leg.labels = NULL, title.ll = NULL, leg.title = NULL) {
  stopifnot(
    inherits(sf.ll, "sf"),
    !is.null(sf.ll[data.name]),
    perc %in% c(1, 2)
  )
  if (!is.null(leg.labels) & length(col.pal) != length(leg.labels)) {
    stop("If 'leg.labels' is not NULL, then 'col.pal' and 'leg.labels' ",
         "must be the same length")
  }

  data.vec <- st_set_geometry(sf.ll[data.name], NULL)[, 1]
  stopifnot(is.numeric(data.vec))

  if (perc == 1) {
    b.model <- eSDM::breaks_calc(data.vec)
    binpal <- colorBin(col.pal, data.vec, bins = b.model, na.color = "grey")

    leaflet(sf.ll) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      # addTiles(group = "OpenStreetMap") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 1) %>%
      addLegend(
        "bottomright", title = "Relative prediction value",
        colors = c(col.pal, "grey"), labels = c(leg.labels, "NA"),
        opacity = 1) %>%
      # addGraticule(interval = 5) %>%
      # setView(lat = st_centroid(st_combine(nc))[[1]][2], ..., zoom = 4) %>%
      mapview::addMouseCoordinates()

  } else {
    binpal <- colorBin(col.pal, data.vec, bins = 10, pretty = FALSE,
                       na.color = NA)

    leaflet(sf.ll) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      # addTiles(group = "OpenStreetMap") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
      addPolygons(
        stroke = FALSE, color = ~binpal(data.vec), fillOpacity = 1) %>%
      addLegend(
        "bottomright", title = "Absolute prediction value", pal = binpal,
        values = ~data.vec, opacity = 1) %>%
      mapview::addMouseCoordinates()
  }
}
