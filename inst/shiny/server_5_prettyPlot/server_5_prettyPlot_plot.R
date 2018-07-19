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

  validate(need(length(plot.which) == 1, "tmap_arrange stuff"))

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
    dims = c(plot.nrow, plot.ncol),
    idx.list = vals$pretty.toplot.idx[plot.which],
    params.list = vals$pretty.params.list[plot.which]
  )

  ""
})


###############################################################################
plot_pretty_top <- function(dims, idx.list, params.list) {
  # Plot the plots
  k <- params.list[[1]]
  plot_pretty(
    k$model.toplot, k$data.name, k$plot.lim, k$axes.inc,
    k$title.ll, k$lab.x, k$lab.y,
    k$title.cex, k$lab.cex
    )

  # for(k in params.list) {
  #   plot_pretty(
  #     k$model.toplot, k$data.name, k$plot.lim, k$axes.inc,
  #     k$title.ll, k$lab.x, k$lab.y,
  #     k$title.cex, k$lab.cex, k$axis.cex, k$axis.tcl,
  #     k$list.background, k$list.colorscheme, k$list.addobj
  #   )
  # }
}


###############################################################################
# axis.cex, axis.tcl,
# list.background, list.colorscheme, list.addobj
plot_pretty <- function(model.toplot, data.name, plot.lim, axes.inc,
                        title.ll, lab.x, lab.y,
                        title.cex, lab.cex) {
  # axes.inc doesn't do anything yet


  b.preds <- breaks_calc(st_set_geometry(model.toplot, NULL)[, data.name])

  tm_shape(model.toplot, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
    tm_fill(col = data.name, style = "fixed", breaks = b.preds, n = 10, palette = pal.esdm,
            title = "", labels = leg.perc.esdm, legend.is.portrait = TRUE) +
    tm_layout(main.title = title.ll, title.size = title.cex) +
    tm_xlab(lab.x, lab.cex) +
    tm_ylab(lab.y, lab.cex) +
    tm_legend(outside = TRUE, outside.position = "right", frame = "black") +
    tm_grid(labels.inside.frame = TRUE, alpha = 0.8)
}

###############################################################################
