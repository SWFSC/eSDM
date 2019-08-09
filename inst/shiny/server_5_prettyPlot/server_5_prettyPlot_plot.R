###############################################################################
### Non-reactive plotting functions for prettyPlot called in server_render.R
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
# Warnings for if plot dimensions are larger than window size
pretty_plot_dim_warnings <- reactive({
  plot.width  <- req(input$pretty_width_inch) * 96
  x <- req(session$clientData$output_pretty_plot_out_width)

  if (x < plot.width) {
    paste(
      "Warning: The user-specified 'Plot width (in)' is",
      "larger than the current plot window,",
      "and thus some of the plots might not be",
      "properly displayed within the plot window"
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
    need(inherits(plot.nrow, "integer") && inherits(plot.ncol, "integer"),
         paste("Error: 'Number of rows' and 'Number of columns'",
               "must be whole numbers")),
    need(isTruthy(plot.width) && isTruthy(plot.height) &&
           is.numeric(plot.width) && is.numeric(plot.height),
         paste("Error: 'Plot width (in)' and 'Plot height (in)'",
               "must be numbers"))
  )

  validate(
    need((plot.nrow * plot.ncol) >= length(plot.which),
         paste("Error: 'Number of rows' * 'Number of columns' must be",
               "greater than or equal to the number of items",
               "selected from the to-plot list to plot")),
    need(plot.width > 0 && plot.height > 0,
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
# Top-level function called within renderPlot() in server_render
# tmap_arrange() overrides the margins
plot_pretty_top <- function(dims, idx.list, params.list) {
  tmap.list <- lapply(params.list, function(k) {
    if (isTruthy(k$list.addobj)) {
      addobj.pre.bool  <- sapply(k$list.addobj, function(i) i$obj.order == 1)
      list.addobj.pre  <- k$list.addobj[addobj.pre.bool]
      list.addobj.post <- k$list.addobj[!addobj.pre.bool]

      temp <- length(list.addobj.pre) + length(list.addobj.post)
      validate(
        need(temp == length(k$list.addobj),
             "Error: Error in processing additional objects")
      )
      rm(temp)

    } else {
      list.addobj.pre  <- list()
      list.addobj.post <- list()
    }

    plot_pretty(
      k$model.toplot, k$map.range, k$background.color,
      k$list.colorscheme, k$list.legend, k$list.titlelab, k$list.margin,
      k$list.tick, list.addobj.pre, list.addobj.post
    )
  })

  # tmap_arrange call ~0.3s slower than just printing tmap object
  # outer.margins arg ignored by tmap_arrange
  if (length(tmap.list) == 1) {
    tmap.list[[1]]
  } else {
    tmap_arrange(tmap.list, dims[1:2], asp = NULL)
  }
}


###############################################################################
# Returns individual tmap objects
plot_pretty <- function(model.toplot, map.range, background.color,
                        list.colorscheme, list.legend, list.titlelab,
                        list.margin, list.tick,
                        list.addobj.pre, list.addobj.post) {
  #----------------------------------------------------------------------------
  # For ease of calling / sake of space
  l1 <- list.colorscheme
  l2 <- list.legend
  l3 <- list.titlelab
  l3b <- unlist(list.margin)
  l4 <- list.tick
  l5a <- list.addobj.pre
  l5b <- list.addobj.post
  m.orig <- model.toplot

  #----------------------------------------------------------------------------
  # Make range polygon for intersections
  # st_intersection()'s below happen here so users can update map range params
  range.poly <- pretty_range_poly_func(map.range, st_crs(model.toplot))
  range.outline <- pretty_crsNA_func(st_cast(range.poly, "MULTILINESTRING"))
  rpoly.mat <- matrix(st_bbox(range.poly), ncol = 2)
  # tm_shape() does not currently handle bbox obj correctly for range [0, 360]

  #----------------------------------------------------------------------------
  # Additional objects - pre
  # l5a will be list() if empty and thus won't enter for() loop
  for (j in l5a) {
    if (exists("tmap.obj")) {
      tmap.obj <- tmap.obj + plot_pretty_addobj(j, range.poly)
    } else {
      tmap.obj <- plot_pretty_addobj(j, range.poly, rpoly.mat)
    }
  }
  rm(j)

  #----------------------------------------------------------------------------
  # Shape, fill (colorscheme), title, axis labels, margins
  model.toplot <- pretty_crsNA_func(
    pretty_int_func(m.orig, range.poly, "selected predictions")
  )

  if (exists("tmap.obj")) {
    tmap.obj <- tmap.obj +
      tm_shape(model.toplot, projection = st_crs(m.orig)) +
      tm_fill(col = l1$data.name, border.col = "transparent",
              style = "fixed", breaks = l1$data.breaks, palette = l1$col.pal,
              colorNA = l1$col.na, textNA = "NA", showNA = NA,
              title = "", labels = l1$leg.labs,
              legend.is.portrait = TRUE, legend.reverse = TRUE)

  } else {
    tmap.obj <- tm_shape(model.toplot, bbox = rpoly.mat,
                         projection = st_crs(m.orig)) +
      tm_fill(col = l1$data.name, border.col = "transparent",
              style = "fixed", breaks = l1$data.breaks, palette = l1$col.pal,
              colorNA = l1$col.na, textNA = "NA", showNA = NA,
              title = "", labels = l1$leg.labs,
              legend.is.portrait = TRUE, legend.reverse = TRUE)
  }

  tmap.obj <- tmap.obj +
    tm_layout(bg.color = background.color, legend.bg.color = "white",
              main.title = l3$title, main.title.position = "center",
              main.title.size = l3$titlecex,
              inner.margins = l3b[1:4], outer.margins = l3b[5]) +
    tm_xlab(l3$xlab, l3$labcex) +
    tm_ylab(l3$ylab, l3$labcex)

  #----------------------------------------------------------------------------
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

  #----------------------------------------------------------------------------
  # Grid lines and labels
  if (l4$inc) {
    if (st_is_longlat(m.orig)) {
      tmap.obj <- tmap.obj +
        tm_graticules(x = l4$x.vals, y = l4$y.vals, col = l4$grid.col,
                      lwd = l4$grid.lw, alpha = l4$grid.alpha,
                      lines = l4$grid.lines, ticks = l4$grid.ticks,
                      labels.inside.frame = l4$grid.labs.in,
                      labels.size = l4$grid.labs.size, labels.rot = c(0, 90))

    } else {
      tmap.obj <- tmap.obj +
        tm_grid(x = l4$x.vals, y = l4$y.vals, col = l4$grid.col,
                lwd = l4$grid.lw, alpha = l4$grid.alpha,
                lines = l4$grid.lines, ticks = l4$grid.ticks,
                labels.inside.frame = l4$grid.labs.in,
                labels.size = l4$grid.labs.size, labels.rot = c(0, 90))
    }
  }

  #----------------------------------------------------------------------------
  # Additional objects - post
  # l5b will be list() if empty and thus won't enter for() loop
  for (j in l5b) tmap.obj <- tmap.obj + plot_pretty_addobj(j, range.poly)
  rm(j)

  #----------------------------------------------------------------------------
  # tmap.obj
  tmap.obj +
    tm_shape(range.outline, projection = st_crs(m.orig)) +
    tm_lines(col = background.color)
}


###############################################################################
# Helper function for additional object plotting part of plot_pretty()
plot_pretty_addobj <- function(i, range.poly, rpoly.mat = NULL) {
  i$obj <- pretty_crsNA_func(
    pretty_int_func(i$obj, range.poly, tolower(i$obj.text))
  )

  if (i$obj.text == "Validation data points") {
    # Special due to 2 colors. Also 'NA' color means that points are white
    if (!is.na(i$col.absborder) && !is.na(i$col.ptfill)) {
      tm_shape(i$obj, bbox = rpoly.mat, projection = st_crs(range.poly)) +
        tm_dots(col = "sight", palette = c(i$col.absborder, i$col.ptfill),
                shape = i$pchlty, size = i$cexlwd, legend.show = FALSE)
    } else if (is.na(i$col.ptfill)) {
      tm_shape(dplyr::filter(i$obj, sight == 0), bbox = rpoly.mat,
               projection = st_crs(range.poly)) +
        tm_dots(col = i$col.absborder,
                shape = i$pchlty, size = i$cexlwd, legend.show = FALSE)

    } else { #is.na(i$col.ptfill)
      tm_shape(dplyr::filter(i$obj, sight == 1), bbox = rpoly.mat,
               projection = st_crs(range.poly)) +
        tm_dots(col = i$col.ptfill,
                shape = i$pchlty, size = i$cexlwd, legend.show = FALSE)
    }

  } else if (i$obj.type == 1) { #pts
    tm_shape(i$obj, bbox = rpoly.mat, projection = st_crs(range.poly)) +
      tm_dots(col = i$col.ptfill, shape = i$pchlty, size = i$cexlwd,
              legend.show = FALSE)

  } else { #polys
    tm_shape(i$obj, bbox = rpoly.mat, projection = st_crs(range.poly)) +
      tm_polygons(col = i$col.ptfill, border.col = i$col.absborder,
                  alpha = ifelse(is.na(i$col.ptfill), 0, 1),
                  lty = i$pchlty, lwd = i$cexlwd)
  }
}

###############################################################################
