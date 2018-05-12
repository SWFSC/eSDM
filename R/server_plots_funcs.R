#' Title
#'
#' Produce thing for plotting
#'
#' @param models.toplot List of sf objects to plot
#'
#' @export

multiplot <- function(models.toplot, data.name, plot.titles, perc.num,
                      col.num, col.pal, leg.inc, leg.labels) {
  models.num <- length(models.toplot)

  # Set variables with plot parameters
  plot.ncol <- case_when(
    models.num == 1 ~ 1,
    models.num == 2 ~ 2,
    models.num == 3 ~ 3,
    models.num == 4 ~ 2,
    models.num <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(models.num))
  )
  plot.nrow <- case_when(
    models.num <= 3 ~ 1,
    models.num <= 6 ~ 2,
    models.num <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(models.num))
  )

  axis.cex.curr <- case_when(
    # models.num == 1 ~ 1.2,
    models.num <= 3 ~ 1.2,
    TRUE ~ 0.6
  )
  main.cex.curr <- case_when(
    models.num == 1 ~ 1.6,
    TRUE ~ 1.0
  )

  # TODO make this smart?
  x.tick.num <- 5
  y.tick.num <- 5


  # Layout prep
  if (leg.inc & perc.num == 1) {
    mat.num <- c(1:models.num,
                 rep(1 + models.num, plot.nrow))
    lay.w <- c(rep((0.92 / plot.ncol), plot.ncol), 0.08)
    layout(matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol + 1),
           width = lay.w)
  } else {
    mat.num <- 1:models.num
    layout(matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol))
  }

  # Plot SDM previews
  for (i in 1:models.num) {
    preview_ll(models.toplot[[i]],
               data.name,
               plot.titles[[i]],
               perc.num,
               axis.cex.curr, main.cex.curr,
               col.pal = col.pal)
  }

  # Plot legend
  if (leg.inc & perc.num == 1) {
    # Set plot margins to minimal for top, right, and bottom to fill space
    opar <- par(mai = c(0.1, 0.82, 0.1, 0))
    on.exit(par(opar), add = TRUE)

    # Plot things
    graphics::image(1, 1:col.num, t(as.matrix(1:col.num)), col = col.pal,
                    axes = FALSE, xlab = "", ylab = "")
    graphics::box(col = "black")
    axis(2, at = 1:col.num, labels = leg.labels, tick = FALSE, las = 1, cex.axis = 1.2)
  }

  # Reset layout
  layout(1)
}


#' Title
#'
#' Produce thing for download
#'
#' @param list.data.display List of proper things
#'
#' @export

plot_multi_download <- function(list.data.download) {
  models.num <- length(list.data.download$models.toplot)

  # Set variables with plot parameters
  plot.ncol <- case_when(
    models.num == 1 ~ 1,
    models.num <= 4 ~ 2,
    models.num <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(models.num))
  )
  plot.nrow <- case_when(
    models.num <= 2 ~ 1,
    models.num <= 6 ~ 2,
    models.num <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(models.num))
  )

  axis.cex.curr <- case_when(
    models.num == 1 ~ 0.8,
    models.num <= 4 ~ 0.3,
    TRUE ~ 0.2
  )
  main.cex.curr <- case_when(
    models.num == 1 ~ 1.0,
    models.num == 2 ~ 0.5,
    TRUE ~ 0.4
  )

  x.tick.num <- 5
  y.tick.num <- 5

  # Generate gtable object of plot(s)
  layout(matrix(1:list.data.download$models.num, nrow = plot.nrow,
                ncol = plot.ncol, byrow = FALSE))
  for (i in 1:list.data.download$models.num) {
    preview_ll(list.data.download$models.toplot[[i]],
               list.data.download$data.name,
               list.data.download$plot.titles[[i]],
               list.data.download$perc.num,
               axis.cex.curr, main.cex.curr)
  }
  layout(1)
}


#' title
#'
#' General function for plotting sf object
#'
#' @param sf.ll sf object in crs.ll
#'
#' @export

preview_ll <- function(sf.ll, data.name, title.ll, perc,
                       axis.cex, main.cex, col.pal) {
  ### Prep:
  data.vec <- st_set_geometry(sf.ll, NULL)[, data.name]

  ### Generate plot with densities color-coded by percentages or values
  if(perc == 1) {
    b.model <- breaks.calc(data.vec)
    # b.model[1] <- b.model[1] - 0.1 # so that left.open = FALSE
    # temp <- findInterval(data.vec, b.model,
    #                      rightmost.closed = TRUE, left.open = FALSE)
    #
    # plot(st_geometry(sf.ll), axes = TRUE, border = NA,
    #      col = col.ramp[temp],
    #      main = title.ll, cex.main = main.cex, cex.axis = axis.cex)
    # legend("bottomleft", legend = labels.lab, col = rev(col.ramp),
    #        pch = 15, pt.cex = 2, bty = "n", cex = 0.8)

    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.pal,
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex,
         key.pos = NULL, reset = FALSE)
    #graticule = st_crs(sf.ll),
  } else {
    b.model <- seq(from = min(data.vec, na.rm = TRUE),
                   to = max(data.vec, na.rm = TRUE),
                   length.out = 11)
    # temp <- findInterval(data.vec, b.model,
    #                      rightmost.closed = TRUE, left.open = FALSE)
    #
    # plot(st_geometry(sf.ll), axes = TRUE, border = NA,
    #      col = col.ramp[temp],
    #      main = title.ll, cex.main = main.cex, cex.axis = axis.cex)
    #
    # legend("bottomleft", legend = labels.lab, col = rev(col.ramp),
    #        pch = 15, pt.cex = 2, bty = "n")

    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.pal,
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex,
         key.pos = NULL, reset = FALSE)
  }
}
