###############################################################################
### Non-reactive plotting functions for prettyPlot called in server_render.R
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
# Set reactive values that will then be plotted in server_render
pretty_plot_plot <- eventReactive(input$pretty_plot_plot_event, {
  req(vals$pretty.params.toplot)

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

  vals$pretty.plot <- list(
    dims = c(nrow = plot.nrow, ncol = plot.ncol),
    idx.list = vals$pretty.toplot.idx[plot.which],
    params.list = vals$pretty.params.toplot[plot.which]
  )

  ""
})


###############################################################################
# Top-level function called within renderPlot() in server_render
# When using tmap arrange spatial plot space is filled,
#   but a normal tmap call respects provided axis limits
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
# Returns individual tmap objects
plot_pretty <- function(model.toplot, plot.lim, background.color,
                        list.titlelab, list.tick, list.colorscheme,
                        list.legend, list.addobj) {
  #----------------------------------------------
  # For ease of calling / sake of space
  l1 <- list.colorscheme
  l2 <- list.legend
  l3 <- list.titlelab
  l4 <- list.tick
  # l5 <- list.addobj

  #----------------------------------------------
  # Shape, fill (colorscheme), title, axis labels
  tmap.obj <- tm_shape(model.toplot, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
    tm_fill(col = l1$data.name, style = "fixed", breaks = l1$data.breaks,
            palette = l1$col.pal,
            title = "", labels = l1$leg.labs, legend.is.portrait = TRUE) +
    tm_layout(bg.color = background.color, legend.bg.color = "white",
              main.title = l3$title, main.title.position = "left",
              main.title.size = l3$titlecex) +
    tm_xlab(l3$xlab, l3$labcex) +
    tm_ylab(l3$ylab, l3$labcex)

  #----------------------------------------------
  # Legend
  if (l4$inc) {
    tmap.obj <- tmap.obj +
      tm_legend(show = TRUE, outside = l2$out, position = l2$pos,
                outside.position = l2$out.pos, text.size = l2$text.size,
                frame = l2$border)
  } else {
    tmap.obj <- tmap.obj + tm_legend(show = FALSE)
  }

  #----------------------------------------------
  # Grid lines and labels
  if (l4$inc) {
    tmap.obj <- tmap.obj +
      tm_grid(x = l4$x.vals, y = l4$y.vals, col = l4$grid.col,
              lwd = l4$grid.lw, alpha = l4$grid.alpha,
              labels.inside.frame = l4$grid.labs.in,
              labels.size = l4$grid.labs.size)
  }


  #----------------------------------------------
  tmap.obj
}

###############################################################################
