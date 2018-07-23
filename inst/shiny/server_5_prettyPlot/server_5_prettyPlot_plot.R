###############################################################################
### Non-reactive plotting functions for prettyPlot
# called in server_render.R
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
pretty_plot_plot <- eventReactive(input$pretty_plot_plot_event, {
  req(vals$pretty.params.list)

  plot.which <- input$pretty_plot_toplot_table_out_rows_selected
  plot.nrow <- input$pretty_plot_nrow
  plot.ncol <- input$pretty_plot_ncol

  validate(
    need(plot.which,
         "Error: Select at least one item from the to-plot list to plot"),
    need(inherits(plot.nrow, "integer") & inherits(plot.ncol, "integer"),
         paste("Error: 'Number of rows' and 'Number of columns'",
               "must be whole numbers")) %then%
      need((plot.nrow * plot.ncol) >= length(plot.which),
           paste("Error: 'Number of rows' * 'Number of columns' must be",
                 "greater than or equal to the number of items",
                 "selected from the to-plot list to plot"))
  )

  vals$pretty.plot.list <- list(
    dims = c(nrow = plot.nrow, ncol = plot.ncol),
    idx.list = vals$pretty.toplot.idx[plot.which],
    params.list = vals$pretty.params.list[plot.which]
  )

  ""
})


###############################################################################
plot_pretty_top <- function(dims, idx.list, params.list) {
  # Plot the plots
  if ((dims[1] * dims[2]) == 1) {
    k <- params.list[[1]]
    plot_pretty(
      k$model.toplot, k$data.name, k$plot.lim,
      k$list.titlelab, k$list.tick
      # k$list.background, k$list.colorscheme, k$list.addobj
    )

  } else {
    tmap.list <- lapply(params.list, function(k) {
      plot_pretty(
        k$model.toplot, k$data.name, k$plot.lim,
        k$list.titlelab, k$list.tick
        # k$list.background, k$list.colorscheme, k$list.addobj
      )
    })

    do.call(tmap_arrange, c(tmap.list, dims))
  }
}


###############################################################################
# list.background, list.colorscheme, list.addobj
plot_pretty <- function(model.toplot, data.name, plot.lim,
                        #title.ll, lab.x, lab.y, title.cex, lab.cex,
                        list.titlelab, list.tick) {
  l1 <- list.titlelab
  l2 <- list.tick

  # TODO remove when colorscheme is implemented
  b.preds <- breaks_calc(st_set_geometry(model.toplot, NULL)[, data.name])

  tmap.obj <- tm_shape(model.toplot, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
    tm_fill(col = data.name, style = "fixed", breaks = b.preds, n = 10, palette = pal.esdm,
            title = "", labels = leg.perc.esdm, legend.is.portrait = TRUE) +
    tm_layout(main.title = l1$title, main.title.position = "left",
              main.title.size = l1$titlecex) +
    tm_xlab(l1$xlab, l1$labcex) +
    tm_ylab(l1$ylab, l1$labcex) +
    tm_legend(outside = TRUE, outside.position = "right", frame = "black")

  if (l2$inc) {
    tmap.obj <- tmap.obj +
      tm_grid(x = l2$x.vals, y = l2$y.vals, col = l2$grid.col,
              lwd = l2$grid.lw, alpha = l2$grid.alpha,
              labels.inside.frame = l2$grid.labs.in,
              labels.size = l2$grid.labs.size)
  }

  tmap.obj
}

###############################################################################
