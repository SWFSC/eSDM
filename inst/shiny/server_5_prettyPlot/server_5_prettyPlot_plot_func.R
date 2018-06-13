###############################################################################
### Non-reactive plotting function for prettyPlot
# called in server_render.R
plot_pretty <- function(model.toplot, data.name, plot.lim, axes.inc,
                        title.ll, lab.x, lab.y,
                        title.cex, lab.cex, axis.cex, axis.tcl,
                        list.background, list.colorscheme, list.addobj) {
  l.cs <- list.colorscheme

  #--------------------------------------------------------
  if (l.cs$leg.inc) {
    layout(matrix(1:2, ncol = 2), width = c(1, lcm(3.6)))
  }
  plot(
    list.background[[1]], axes = axes.inc, border = NA, col = list.background[[2]],
    xlim = plot.lim[1:2], ylim = plot.lim[3:4],
    main = title.ll, cex.main = title.cex,
    cex.lab = lab.cex, cex.axis = axis.cex, tcl = axis.tcl,
    xlab = lab.x, ylab = lab.y,
    key.pos = NULL, reset = FALSE
  )
  if (!axes.inc) graphics::box()


  #--------------------------------------------------------
  # Plot added objects pt 1
  # Currently hard-coded, to change
  for (i in list.addobj) {
    if (i$pre.sdm) {
      if (i$obj.text == 1) {
        plot(
          i$obj.sfc, add = TRUE, border = i$col, lwd = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else if (i$obj.text == 2) {
        plot(
          i$obj.sfc, add = TRUE, border = NA, col = i$col, lwd = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else if (i$obj.text == 3) {
        plot(
          i$obj.sfc, add = TRUE, col = i$col, cex = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else {
        validate(need(FALSE, "Not ready"))
      }
    }
  }


  #--------------------------------------------------------
  # plot(st_geometry(model.toplot))

  # This is separate for the sake of background and plotting x and y axis labels
  plot(
    model.toplot[data.name], add = TRUE, border = NA,
    breaks = l.cs$data.breaks, pal = l.cs$color.palette,
    key.pos = NULL, reset = FALSE
  )


  #--------------------------------------------------------
  # Plot added objects pt 2
  # Currently hard-coded, to change
  for (i in list.addobj) {
    if (!i$pre.sdm) {
      if (i$obj.text == 1) {
        plot(
          i$obj.sfc, add = TRUE, border = i$col, lwd = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else if (i$obj.text == 2) {
        plot(
          i$obj.sfc, add = TRUE, border = NA, col = i$col, lwd = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else if (i$obj.text == 3) {
        plot(
          i$obj.sfc, add = TRUE, col = i$col, cex = i$cex,
          reset = FALSE, key.pos = NULL
        )
      } else {
        validate(need(FALSE, "Not ready"))
      }
    }
  }

  #--------------------------------------------------------
  # Plot legend if necessary
  if (l.cs$leg.inc) {
    col.num <- length(l.cs$color.palette)
    col.pal <- l.cs$color.palette
    leg.labels <- l.cs$labels
    opar <- par(mai = c(1, 0, 0.86, 1.2))
    on.exit(par(opar), add = TRUE)

    graphics::image(0.6, 1:col.num, t(as.matrix(1:col.num)), col = col.pal,
                    axes = FALSE, xlab = "", ylab = "")
    graphics::box(col = "black")
    # temp <- ifelse(models.num == 1, 0.8, 1.3) # not sure why this is necessary
    graphics::axis(4, at = 1:col.num, labels = leg.labels, tick = FALSE,
                   las = 1, cex.axis = 1)
  }
}

###############################################################################
