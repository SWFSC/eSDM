###############################################################################
### Non-reactive plotting functions for prettyPlot called in server_render.R
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
  if ((dims[1] * dims[2]) == 1) {
    k <- params.list[[1]]
    plot_pretty(
      k$model.toplot, k$plot.lim, k$background.color,
      k$list.titlelab, k$list.tick, k$list.colorscheme, k$list.legend,
      k$list.addobj
    )

  } else {
    tmap.list <- lapply(params.list, function(k) {
      plot_pretty(
        k$model.toplot, k$plot.lim, k$background.color,
        k$list.titlelab, k$list.tick, k$list.colorscheme, k$list.legend,
        k$list.addobj
      )
    })

    do.call(tmap_arrange, c(tmap.list, dims))
  }
}


###############################################################################
plot_pretty <- function(model.toplot, plot.lim, background.color,
                        list.titlelab, list.tick, list.colorscheme,
                        list.legend, list.addobj) {
  l1 <- list.titlelab
  l2 <- list.tick
  l3 <- list.colorscheme
  l4 <- list.legend
  # l4 <- list.addobj

  #----------------------------------------------
  tmap.obj <- tm_shape(model.toplot, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
    tm_fill(col = l3$data.name, style = "fixed", breaks = l3$data.breaks,
            palette = l3$col.pal,
            title = "", labels = l3$leg.labs, legend.is.portrait = TRUE) +
    tm_layout(bg.color = background.color, legend.bg.color = "white",
              main.title = l1$title, main.title.position = "left",
              main.title.size = l1$titlecex) +
    tm_xlab(l1$xlab, l1$labcex) +
    tm_ylab(l1$ylab, l1$labcex) +
    tm_legend(outside = TRUE, outside.position = "right", frame = "black")

  #----------------------------------------------
  if (l4$inc) {
    tmap.obj <- tmap.obj +
      tm_legend(show = TRUE, outside = l4$out, position = l4$pos,
                outside.position = l4$out.pos, text.size = l4$text.size)
  } else {
    tmap.obj <- tmap.obj + tm_legend(show = FALSE)
  }

  #----------------------------------------------
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
