###############################################################################
### Non-reactive plotting functions for prettyPlot
# called in server_render.R
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
pretty_plot_plot <- eventReactive(input$pretty_plot_plot_event, {
  req(vals$pretty.params.list)

  plot.nrow <- input$pretty_plot_nrow
  plot.ncol <- input$pretty_plot_ncol

  validate(
    need(inherits(plot.nrow, "integer") & inherits(plot.ncol, "integer"),
         paste("Error: 'Number of rows' and 'Number of columns'",
               "must be whole numbers")) %then%
      need((plot.nrow * plot.ncol) >= length(vals$pretty.params.list),
           paste("Error: 'Number of rows' * 'Number of columns' must be",
                 "greater than or equal to the number of items in the",
                 "to-plot list"))
  )

  vals$pretty.plot.list <- list(
    dims = c(plot.nrow, plot.ncol), idx.list = vals$pretty.toplot.idx,
    params.list = vals$pretty.params.list
  )

  ""
})


###############################################################################
plot_pretty_top <- function(dims, idx.list, params.list) {
  # Layout prep
  layout.num <- dims[1] * dims[2]
  models.num <- length(params.list)
  layout.dif <- layout.num - models.num

  counter <- 1
  layout.vec <- c()
  for(j in 1:layout.num) {
    if (j > models.num) {
      layout.vec <- c(layout.vec, counter, counter)
      counter <- counter + 1

    } else {
      if (params.list[[j]]$list.colorscheme$leg.inc) {
        layout.vec <- c(layout.vec, counter, counter + 1)
        counter <- counter + 2

      } else {
        layout.vec <- c(layout.vec, counter, counter)
        counter <- counter + 1
      }
    }
  }
  rm(counter)

  # Create layout
  layout(
    matrix(
      layout.vec, nrow = dims[1], ncol = dims[2] * 2, byrow = TRUE
    ),
    widths = rep(c(1, lcm(3.4)), layout.num)
  )
  # layout(
  #   matrix(
  #     1:(layout.num * 2), nrow = dims[1], ncol = dims[2] * 2, byrow = TRUE
  #   ),
  #   widths = rep(c(1, lcm(3.4)), layout.num)
  # )

  # Plot the plots
  for(k in params.list) {
    plot_pretty(
      k$model.toplot, k$data.name, k$plot.lim, k$axes.inc,
      k$title.ll, k$lab.x, k$lab.y,
      k$title.cex, k$lab.cex, k$axis.cex, k$axis.tcl,
      k$list.background, k$list.colorscheme, k$list.addobj
    )
  }

  # Fill any empty layout sections
  if (layout.dif != 0) for (l in 1:layout.dif) graphics::plot.new()
}


###############################################################################
plot_pretty <- function(model.toplot, data.name, plot.lim, axes.inc,
                        title.ll, lab.x, lab.y,
                        title.cex, lab.cex, axis.cex, axis.tcl,
                        list.background, list.colorscheme, list.addobj) {
  l.cs <- list.colorscheme

  #--------------------------------------------------------
  # if (l.cs$leg.inc) {
  #   layout(matrix(1:2, ncol = 2), width = c(1, lcm(3.6)))
  # }
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
  # TODO Currently hard-coded, need to change
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
    graphics::axis(4, at = 1:col.num, labels = leg.labels, tick = FALSE,
                   las = 1, cex.axis = 1)
  }
}

###############################################################################
