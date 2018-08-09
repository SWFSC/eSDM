### Functions that return data used in server_5_prettyPlot_plot.R
### Get model(s) to plot, generate lists with plotting information


###############################################################################
# Get set(s) of predictions to plot

### Process selected rows from tables of predictions
# Return list of [which tables have selected rows, a 3 element list of the...
# ...rows selected in those tables, a 3 element list of selected spdfs]
# '3 element lists' correspond to the 3 tables
pretty_plot_model_selected <- reactive({
  req(pretty_plot_models_idx_count() == 1)

  model.idx.list <- pretty_plot_models_idx_list()
  if (!is.null(model.idx.list[[1]])) {
    vals$models.orig[[model.idx.list[[1]]]]

  } else if (!is.null(model.idx.list[[2]])) {
    vals$overlaid.models[[model.idx.list[[2]]]]

  } else if (!is.null(model.idx.list[[3]])) {
    vals$ensemble.models[[model.idx.list[[3]]]]

  } else {
    validate(need(FALSE, "Pretty plot error"))
  }
})


### Get desired projection (crs object) for map
pretty_plot_crs_selected <- reactive({
  if (input$pretty_plot_proj_ll) {
    crs.ll

  } else {
    if (input$pretty_plot_proj_method == 1) {
      req(pretty_plot_models_idx_count() == 1)
      model.idx.list <- pretty_plot_models_idx_list()

      if (!is.null(model.idx.list[[1]])) {
        st_crs(vals$models.orig[[model.idx.list[[1]]]])

      } else if (!is.null(model.idx.list[[2]])) {
        st_crs(vals$overlaid.models[[model.idx.list[[2]]]])

      } else if (!is.null(model.idx.list[[3]])) {
        st_crs(vals$ensemble.models[[model.idx.list[[3]]]])

      } else {
        validate(need(FALSE, "Pretty plot crs error"))
      }

    } else if (input$pretty_plot_proj_method == 2) {
      st_crs(vals$models.orig[[req(as.numeric(input$pretty_plot_proj_idx))]])

    } else {
      x <- st_crs(input$pretty_plot_proj_epsg)
      validate(
        need(x[[2]],
             paste("Error: The provided EPSG code was not recognized;",
                   "please provide a valid code"))
      )

      x
    }
  }
})


### Return list of: (table idx, pred idx, currently selected model predictions)
# Currently only one model can be plotted so function just gets only model
# In the future, this function would be used to get model specified by user
pretty_plot_model_toplot <- reactive({
  model.selected <- pretty_plot_model_selected()
  crs.selected <- pretty_plot_crs_selected()

  if (identical(st_crs(model.selected), crs.selected)) {
    model.selected
  } else {
    st_transform(model.selected, crs.selected)
  }
})


###############################################################################
### Compile plot limits, and create a boundary box with which to clip model
### Return list of (plot limits, boundary box poly)
pretty_plot_range_poly <- reactive({
  plot.lim <- c(
    input$pretty_plot_range_xmin, input$pretty_plot_range_xmax,
    input$pretty_plot_range_ymin, input$pretty_plot_range_ymax
  )

  poly.x <- plot.lim[c(1, 1, 2, 2, 1)]
  poly.y <- plot.lim[c(3, 4, 4, 3, 3)]

  plot.lim.poly <- st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = pretty_plot_crs_selected()
  )

  list(plot.lim, plot.lim.poly)
})


###############################################################################
# Color scheme of predictions

### Process inputs and return list with num of colors and color palette to use
pretty_plot_colorscheme_palette_num <- reactive({
  req(input$pretty_plot_color_palette)

  perc <- input$pretty_plot_color_perc == 1
  color.palette.idx <- input$pretty_plot_color_palette

  if (perc) {
    color.num <- 10
  } else {
    color.num <- val.pretty.color.num()

    # color.num is supposed to be NULL for color.palette.idx %in% c(1, 6)
    # This forces func to wait until renderUI for color.num has caught up
    if (color.palette.idx %in% 2:5) req(color.num)
  }

  ### Set number of colors and color palette
  if (color.palette.idx == 1) {
    color.palette <- pal.esdm
    color.num <- 10

  } else if (color.palette.idx == 2) {
    validate(
      need(color.num <= 11,
           "Error: The 'RColorBrewer: Spectral' palette has a max of 11 colors")
    )
    color.palette <- rev(RColorBrewer::brewer.pal(color.num, "Spectral"))

  } else if (color.palette.idx == 3) {
    validate(
      need(color.num <= 9,
           "Error: The 'RColorBrewer: YlGnBu' palette has a max of 9 colors")
    )
    color.palette <- rev(RColorBrewer::brewer.pal(color.num, "YlGnBu"))

  } else if (color.palette.idx == 4) {
    color.palette <- viridis::viridis(color.num)

  } else if (color.palette.idx == 5) {
    color.palette <- viridis::inferno(color.num)

  } else if (color.palette.idx == 6) {
    color.num <- 12
    color.palette <- dichromat::colorschemes$"DarkRedtoBlue.12"

  } else {
    validate(need(FALSE, "Error: selecting color palette and number failed"))
  }

  list(color.palette, color.num)
})


# Generate list that specifies color scheme things
pretty_plot_colorscheme_list <- reactive({
  ### Get reactive elements
  perc <- input$pretty_plot_color_perc == 1
  color.palette <- pretty_plot_colorscheme_palette_num()[[1]]
  color.num     <- pretty_plot_colorscheme_palette_num()[[2]]

  x <- pretty_plot_model_toplot()
  data.which <- pretty_plot_table_row_idx()[1]
  data.name <- switch(data.which, "Pred", "Pred.overlaid", "Pred.ens")
  x.df <- st_set_geometry(x, NULL)[, data.name]

  ### Determine data break points and legend labels
  if (perc) {
    # Percentages
    data.breaks <- breaks_calc(x.df)
    labels.lab.pretty <- leg.perc.esdm

  } else {
    # Values
    data.breaks <- seq(
      min(x.df, na.rm = TRUE), max(x.df, na.rm = TRUE),
      length.out = (color.num + 1)
    )
    data.breaks.labs <- round(data.breaks, input$pretty_plot_legend_round)
    labels.lab.pretty <- paste(
      head(data.breaks.labs, -1), tail(data.breaks.labs, -1),
      sep = " - "
    )
  }

  ### Return list
  list(
    data.name = data.name, data.breaks = data.breaks, col.pal = color.palette,
    leg.labs = labels.lab.pretty
  )
})


###############################################################################
### Generate list of legend arguments
pretty_plot_legend_list <- reactive({
  if (input$pretty_plot_legend) {
    if (input$pretty_plot_legend_inout == 1) {
      leg.out <- FALSE
      leg.pos <- switch(
        as.numeric(input$pretty_plot_legend_pos),
        c("left", "top"), c("center", "top"), c("right", "top"),
        c("right", "center"),
        c("right", "bottom"), c("center", "bottom"), c("left", "bottom"),
        c("left", "center")
      )
      leg.outside.pos <- NULL
      leg.width <- 1

    } else{ #input$pretty_plot_legend_inout == 2
      leg.out <- TRUE
      leg.pos <- NULL
      leg.outside.pos <- input$pretty_plot_legend_pos
      leg.width <- input$pretty_plot_legend_width

      validate(
        need(dplyr::between(leg.width, 0.1, 0.5),
             "The 'Legend width' value must be between 0.1 and 0.5")
      )
    }

    leg.text.size <- input$pretty_plot_legend_size
    leg.border    <- ifelse(input$pretty_plot_legend_frame, "black", FALSE)

    list(
      inc = TRUE, out = leg.out, pos = leg.pos, out.pos = leg.outside.pos,
      text.size = leg.text.size, width = leg.width, border = leg.border
    )

  } else {
    list(inc = FALSE)
  }
})


###############################################################################
# Section 2
###############################################################################
# Title and axis labels
pretty_plot_titlelab_list <- reactive({
  list(
    title = input$pretty_plot_title, xlab = input$pretty_plot_xlab,
    ylab = input$pretty_plot_ylab, titlecex = input$pretty_plot_title_cex,
    labcex = input$pretty_plot_lab_cex
  )
})


###############################################################################
### Generate list of coordinate grid line and label info
pretty_plot_tick_list <- reactive({
  lon.grid.vals <- seq(
    from = input$pretty_plot_tick_lon_start,
    to = input$pretty_plot_range_xmax,
    by = input$pretty_plot_tick_lon_interval
  )

  lat.grid.vals <- seq(
    from = input$pretty_plot_tick_lat_start,
    to = input$pretty_plot_range_ymax,
    by = input$pretty_plot_tick_lat_interval
  )

  grid.labs.size <- ifelse(
    input$pretty_plot_tick_label_inc, input$pretty_plot_tick_label_size, 0
  )

  list(
    inc = input$pretty_plot_tick,
    x.vals = lon.grid.vals, y.vals = lat.grid.vals,
    grid.lw = input$pretty_plot_tick_lw,
    grid.alpha = input$pretty_plot_tick_alpha,
    grid.col = input$pretty_plot_tick_color,
    grid.labs.size = grid.labs.size,
    grid.labs.in = input$pretty_plot_tick_label_inout == 1
  )
})


###############################################################################
### Generate lists of additional objects to plot
pretty_plot_addobj_pre_list <- reactive({
  if (!isTruthy(vals$pretty.addobj)) return()

  addobj.pre <- lapply(vals$pretty.addobj, function(i) {
    if (i$pre.sdm) {
      obj.temp <- i$obj
      if (!identical(st_crs(obj.temp), pretty_plot_crs_selected())) {
        obj.temp <- st_transform(obj.temp, pretty_plot_crs_selected())
      }

      obj.temp <- suppressMessages(
        st_intersection(obj.temp, pretty_plot_range_poly()[[2]])
      )

      i$obj <- obj.temp
      i
    } else {
      NA
    }
  })

  if (length(addobj.pre) == 1) {
    if (!isTruthy(addobj.pre[[1]])) return()
  }

  addobj.pre[!is.na(addobj.pre)]
})


pretty_plot_addobj_post_list <- reactive({
  if (!isTruthy(vals$pretty.addobj)) return()

  addobj.post <- lapply(vals$pretty.addobj, function(i) {
    if (!i$pre.sdm) {
      obj.temp <- i$obj
      if (!identical(st_crs(obj.temp), pretty_plot_crs_selected())) {
        obj.temp <- st_transform(obj.temp, pretty_plot_crs_selected())
      }

      obj.temp <- suppressMessages(
        st_intersection(obj.temp, pretty_plot_range_poly()[[2]])
      )

      i$obj <- obj.temp
      i
    } else {
      NA
    }
  })

  if (length(addobj.post) == 1) {
    if (!isTruthy(addobj.post[[1]])) return()
  }

  addobj.post[!is.na(addobj.post)]
})

###############################################################################
