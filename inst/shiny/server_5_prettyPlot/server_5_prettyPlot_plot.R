###############################################################################
### Non-reactive plotting functions for prettyPlot called in server_render.R
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
#
pretty_plot_dim_warnings <- reactive({
  plot.width  <- input$pretty_width_inch * 96
  x <- req(session$clientData$output_pretty_plot_out_width)

  if (x < plot.width) {
    paste(
      "Warning: The user-specififed 'Plot width (in)' is larger than current the plot window,",
      "and thus some of the plots might not be properly displayed within the plot window"
    )
  } else {
    ""
  }
})

# Set reactive values that will then be plotted in server_render
pretty_plot <- eventReactive(input$pretty_plot_event, {
  req(vals$pretty.params.toplot)

  plot.which  <- input$pretty_toplot_table_out_rows_selected
  plot.nrow   <- input$pretty_nrow
  plot.ncol   <- input$pretty_ncol
  plot.width  <- input$pretty_width_inch * 96
  plot.height <- input$pretty_height_inch * 96


  validate(
    need(plot.which,
         "Error: You must select at least one saved map to plot")
  )
  validate(
    need(inherits(plot.nrow, "integer") & inherits(plot.ncol, "integer"),
         paste("Error: 'Number of rows' and 'Number of columns'",
               "must be whole numbers")) %then%
      need((plot.nrow * plot.ncol) >= length(plot.which),
           paste("Error: 'Number of rows' * 'Number of columns' must be",
                 "greater than or equal to the number of items",
                 "selected from the to-plot list to plot"))
  )
  validate(
    need(inherits(plot.width, c("integer", "numeric")) &
           inherits(plot.height, c("integer", "numeric")),
         paste("Error: 'Plot width (in)' and 'Plot height (in)'",
               "must be numbers")) %then%
      need(plot.width > 0 & plot.height > 0,
           paste("Error: 'Plot width (in)' and 'Plot height (in)' must both",
                 "be greater than 0"))
  )

  vals$pretty.plot <- list(
    dims = c(nrow = plot.nrow, ncol = plot.ncol,
             width = plot.width, height = plot.height),
    idx.list = vals$pretty.toplot.idx[plot.which],
    params.list = vals$pretty.params.toplot[plot.which]
  )

  ""
})


###############################################################################
###############################################################################
# Top-level function called within renderPlot() in server_render
# When using tmap arrange spatial plot space is filled,
#   but a normal tmap call respects provided axis limits
plot_pretty_top <- function(dims, idx.list, params.list) {
  tmap.list <- lapply(params.list, function(k) {
    if (isTruthy(k$list.addobj)) {
      addobj.pre.bool  <- sapply(k$list.addobj, function(i) i$obj.order == 1)
      list.addobj.pre  <- k$list.addobj[addobj.pre.bool]
      list.addobj.post <- k$list.addobj[!addobj.pre.bool]

    } else {
      list.addobj.pre  <- list()
      list.addobj.post <- list()
    }

    plot_pretty(
      k$model.toplot, k$plot.lim, k$background.color,
      k$list.colorscheme, k$list.legend, k$list.titlelab, k$list.margin,
      k$list.tick, list.addobj.pre, list.addobj.post
    )
  })

  # tmap_arrange call ~0.3s slower than just printing tmap object
  if (length(tmap.list) == 1) {
    tmap.list[[1]]
  } else {
    tmap_arrange(tmap.list, dims[1:2], asp = NULL)
  }
}


###############################################################################
# Returns individual tmap objects
plot_pretty <- function(model.toplot, plot.lim, background.color,
                        list.colorscheme, list.legend, list.titlelab,
                        list.margin, list.tick,
                        list.addobj.pre, list.addobj.post) {
  #----------------------------------------------
  # For ease of calling / sake of space
  l1 <- list.colorscheme
  l2 <- list.legend
  l3 <- list.titlelab
  l3b <- unlist(list.margin)
  l4 <- list.tick
  l5a <- list.addobj.pre
  l5b <- list.addobj.post

  #----------------------------------------------
  # Additional objects - pre
  for (i in l5a) { #l5a will be list() if empty and thus won't enter for() loop
    if (i$obj.text == "Validation data points") { # special due to 2 colors
      tmap.obj <- tm_shape(i$obj, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
        tm_dots(col = "sight", palette = c(i$col.ptfill, i$col.absborder),
                shape = i$pchlty, size = i$cexlwd, legend.show = FALSE)

    } else if (i$obj.type == 1) { #pts
      tmap.obj <- tm_shape(i$obj, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
        tm_dots(col = i$col.ptfill, shape = i$pchlty, size = i$cexlwd,
                legend.show = FALSE)

    } else { #polys
      tmap.obj <- tm_shape(i$obj, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
        tm_polygons(col = i$col.ptfill, border.col = i$col.absborder,
                    alpha = ifelse(is.na(i$col.ptfill), 0, 1),
                    lty = i$pchlty, lwd = i$cexlwd)
    }
  }
  rm(i)

  #----------------------------------------------
  # Shape, fill (colorscheme), title, axis labels, margins
  if (exists("tmap.obj")) {
    tmap.obj <- tmap.obj +
      tm_shape(model.toplot) +
      tm_fill(col = l1$data.name, border.col = "transparent",
              style = "fixed", breaks = l1$data.breaks, palette = l1$col.pal,
              colorNA = l1$col.na, textNA = "NA", showNA = NA,
              title = "", labels = l1$leg.labs, legend.is.portrait = TRUE)

  } else {
    tmap.obj <- tm_shape(model.toplot, bbox = matrix(plot.lim, nrow = 2, byrow = TRUE)) +
      tm_fill(col = l1$data.name, border.col = "transparent",
              style = "fixed", breaks = l1$data.breaks, palette = l1$col.pal,
              colorNA = l1$col.na, textNA = "NA", showNA = NA,
              title = "", labels = l1$leg.labs, legend.is.portrait = TRUE)
  }

  tmap.obj <- tmap.obj +
    tm_layout(bg.color = background.color, legend.bg.color = "white",
              main.title = l3$title, main.title.position = "center",
              main.title.size = l3$titlecex,
              inner.margins = l3b[1:4], outer.margins = l3b[5]) +
    tm_xlab(l3$xlab, l3$labcex) +
    tm_ylab(l3$ylab, l3$labcex)

  #----------------------------------------------
  # Legend
  if (l2$inc) {
    if (l2$out) {
      tmap.obj <- tmap.obj +
        tm_legend(show = TRUE, outside = TRUE, outside.position = l2$out.pos,
                  text.size = l2$text.size, outside.size = l2$width,
                  frame = l2$border)

    } else {
      tmap.obj <- tmap.obj +
        tm_legend(show = TRUE, outside = FALSE, position = l2$pos,
                  text.size = l2$text.size, width = l2$width,
                  frame = l2$border)
    }

  } else {
    tmap.obj <- tmap.obj + tm_legend(show = FALSE)
  }

  #----------------------------------------------
  # Grid lines and labels
  if (l4$inc) {
    if (st_is_longlat(model.toplot)) {
      tmap.obj <- tmap.obj +
        tm_grid(x = l4$x.vals, y = l4$y.vals, col = l4$grid.col,
                lwd = l4$grid.lw, alpha = l4$grid.alpha,
                labels.inside.frame = l4$grid.labs.in,
                labels.size = l4$grid.labs.size, labels.rot = c(0, 90),
                labels.format = list(fun = function(i) parse(text = paste(i, "*degree"))))

    } else {
      tmap.obj <- tmap.obj +
        tm_grid(x = l4$x.vals, y = l4$y.vals, col = l4$grid.col,
                lwd = l4$grid.lw, alpha = l4$grid.alpha,
                labels.inside.frame = l4$grid.labs.in,
                labels.size = l4$grid.labs.size, labels.rot = c(0, 90))
    }
  }

  #----------------------------------------------
  # Additional objects - post
  for (j in l5b) { #l5b will be list() if empty and thus won't enter for() loop
    if (j$obj.text == "Validation data points") { # special due to 2 colors
      tmap.obj <- tmap.obj +
        tm_shape(j$obj) +
        tm_dots(col = "sight", palette = c(j$col.ptfill, j$col.absborder),
                shape = j$pchlty, size = j$cexlwd, legend.show = FALSE)

    } else if (j$obj.type == 1) { #pts
      tmap.obj <- tmap.obj +
        tm_shape(j$obj) +
        tm_dots(col = j$col.ptfill, shape = j$pchlty, size = j$cexlwd,
                legend.show = FALSE)

    } else { #polys
      tmap.obj <- tmap.obj +
        tm_shape(j$obj) +
        tm_polygons(col = j$col.ptfill, border.col = j$col.absborder,
                    alpha = ifelse(is.na(j$col.ptfill), 0, 1),
                    lty = j$pchlty, lwd = j$cexlwd)
    }
  }
  rm(j)

  #----------------------------------------------
  tmap.obj
}

###############################################################################
