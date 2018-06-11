### Functions that return data used in server_5_prettyPlot_plot.R
### Get model(s) to plot, generate lists with plotting information


###############################################################################
# Get set(s) of predictions to plot

### Get desired projection (crs object) for map
pretty_plot_models_crs <- reactive({
  if (input$pretty_plot_proj_ll) {
    crs.ll
  } else {
    st_crs(vals$models.orig[[req(as.numeric(input$pretty_plot_proj_idx))]])
  }
})

### Process selected rows from tables of predictions
# Return list of [which tables have selected rows, a 3 element list of the...
# ...rows selected in those tables, a 3 element list of selected spdfs]
# '3 element lists' correspond to the 3 tables
pretty_plot_models_toplot_list <- reactive({
  tables.null <- pretty_plot_tables_null()
  models.idx.count <- pretty_plot_models_idx_count()

  req(models.idx.count == 1)

  validate(
    need(models.idx.count == 1,
         "Error: Pretty plots not ready for multiplots")
  )

  table.idx <- which(!tables.null)
  model.idx.list <- pretty_plot_models_idx_list()

  models.list.orig <- vals$models.ll[model.idx.list[[1]]]
  models.list.over <- vals$overlaid.models[model.idx.list[[2]]]
  models.list.ens  <- vals$ensemble.models[model.idx.list[[3]]]

  prettyplot.crs <- pretty_plot_models_crs()
  models.all <- list(models.list.orig, models.list.over, models.list.ens)
  models.all.proj <- lapply(models.all, function(j.list) {
    lapply(j.list, function(j) {
      if (identical(st_crs(j), prettyplot.crs)) {
        j
      } else {
        st_transform(j, prettyplot.crs)
      }
    })
  })

  list(table.idx, model.idx.list, models.all.proj)
})

### Return list of: (table idx, pred idx, currently selected model predictions)
# Currently only one model can be plotted so function just gets only model
# In the future, this function would be used to get model specified by user
pretty_plot_models_toplot <- reactive({
  toplot.list <- pretty_plot_models_toplot_list()

  table.which <- toplot.list[[1]]
  model.which <- toplot.list[[2]][[table.which]]
  model.sf    <- toplot.list[[3]][[table.which]][[1]]

  list(table.which, model.which, model.sf)
})


###############################################################################
### Compile plot limits, and create a boundary box with which to clip model
### Return list of (plot limits, boundary box poly)
pretty_plot_range_poly <- reactive({
  plot.lim <- c(input$pretty_plot_range_xmin, input$pretty_plot_range_xmax,
                input$pretty_plot_range_ymin, input$pretty_plot_range_ymax)

  poly.x <- plot.lim[c(1, 1, 2, 2, 1)]
  poly.y <- plot.lim[c(3, 4, 4, 3, 3)]

  plot.lim.poly <- st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = pretty_plot_models_crs()
  )

  list(plot.lim, plot.lim.poly)
})


###############################################################################
# Background color
pretty_plot_background <- reactive({
  input$pretty_plot_background_color
})


###############################################################################
# Color scheme of predictions

### Process inputs and return list with num of colors and color palette to use
pretty_plot_colorscheme_palette_num <- reactive({
  req(input$pretty_plot_color_palette)

  perc <- input$pretty_plot_color_perc == 1
  color.palette.idx <- input$pretty_plot_color_palette
  color.num <- 10
  if (!perc) {
    color.num <- val.pretty.color.num()

    # color.num is supposed to be NULL for color.palette.idx %in% c(1, 6)
    # This forces func to wait until renderUI for color.num has caught up
    if (color.palette.idx %in% 2:5) req(color.num)
  }

  ### Set number of colors and color palette
  if (color.palette.idx == 1) {
    color.num <- 10
    color.palette <- pal.esdm

  } else if (color.palette.idx == 2) {
    validate(
      need(color.num <= 11,
           "Error: 'RColorBrewer: Spectral' palette has a max of 11 colors")
    )
    color.palette <- rev(RColorBrewer::brewer.pal(color.num, "Spectral"))

  } else if (color.palette.idx == 3) {
    validate(
      need(color.num <= 9,
           "Error: 'RColorBrewer: YlGnBu' palette has a max of 9 colors")
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

  list(color.num, color.palette)
})


# Generate 'colorscheme' list, aka list of vars that control color scheme
pretty_plot_colorscheme_list <- reactive({
  ### Get reactive elements
  perc <- input$pretty_plot_color_perc == 1
  color.num     <- pretty_plot_colorscheme_palette_num()[[1]]
  color.palette <- pretty_plot_colorscheme_palette_num()[[2]]

  leg.include <- input$pretty_plot_legend
  leg.pos <- as.numeric(input$pretty_plot_legend_pos)

  list.selected <- pretty_plot_models_toplot()
  x <- list.selected[[3]]
  data.name <- switch(list.selected[[1]], "Pred", "Pred.overlaid", "Pred.ens")
  x.df <- st_set_geometry(x, NULL)[, data.name]


  ### Calculate data and legend break points and legend labels
  if (perc) {
    # Percentages
    leg.breaks.pretty <- seq(1, 0, length.out = 11)
    data.breaks <- eSDM::breaks_calc(x.df)
    labels.lab.pretty <- leg.perc.esdm
    # labels.at.pretty <- seq(0.95, 0.05, length.out = 10)

  } else {
    # Values
    leg.breaks.pretty <- seq(1, 0, length.out = (color.num + 1))
    data.breaks <- seq(min(x.df, na.rm = TRUE), max(x.df, na.rm = TRUE),
                       length.out = (color.num))
    labels.lab.pretty <- as.character(
      round(data.breaks, input$pretty_plot_legend_round)
    )
  }

  list(data.breaks = data.breaks, color.palette = color.palette,
       leg.inc = leg.include, labels = labels.lab.pretty)
})


###############################################################################
### Generate 'scales' list for spplot, aka list of tick/tick label details
### Argument in spplot:
# scales = list(draw = tick, alternating = 1, tck = c(1,0),
#               x = list(at = c(), labels = c()),
#               y = list(at = c(), labels = c()))
# labels degree symbol: labels = parse(text = paste0(c(), "*degree*W"))

# pretty_plot_scales_list <- reactive({
#   ### If ticks are not plotted, we're done
#   ### Else, process inputs and store results in 'scales' list
#   if (!input$pretty_plot_tick) {
#     list.scales <- list(draw = FALSE)
#
#   } else {
#     scales.tck <- c(input$pretty_plot_tick_length, 0) # tick length
#     list.scales <- list(draw = TRUE, tck = scales.tck)
#
#     ## If applicable: get manually entered tick locations
#     ## Else: use defaults by not providing any x.at or y.at arguments
#     if (input$pretty_plot_tick_manual == 2) {
#       # Read in tick locations
#       x.at <- unlist(strsplit(input$pretty_plot_tick_manual_lon, ", "))
#       y.at <- unlist(strsplit(input$pretty_plot_tick_manual_lat, ", "))
#       validate(
#         need(isTruthy(x.at),
#              paste("Error: Please either enter 'Longitude tick locations'" ,
#                    "values or select \"Use default tick locations\"")),
#         need(isTruthy(y.at),
#              paste("Error: Please either enter 'Latitude tick locations'" ,
#                    "values or select \"Use default tick locations\""))
#       )
#
#       # Convert tick locations to numbers
#       x.at <- suppressWarnings(as.numeric(x.at))
#       y.at <- suppressWarnings(as.numeric(y.at))
#       validate(
#         need(!anyNA(x.at),
#              paste("Error: Please ensure that the 'Longitude tick",
#                    "locations' entry is valid")),
#         need(!anyNA(y.at),
#              paste("Error: Please ensure that the 'Latitude tick",
#                    "locations' entry is valid"))
#       )
#
#       # Sort tick locations and check that tick locations ∈ [map limits]
#       x.at <- sort(x.at)
#       y.at <- sort(y.at)
#       plot.lim <- pretty_plot_range_poly()[[1]]
#
#       x.intervals <- findInterval(x.at, plot.lim[1:2], rightmost.closed = TRUE)
#       y.intervals <- findInterval(y.at, plot.lim[3:4], rightmost.closed = TRUE)
#
#       validate(
#         need(all(x.intervals == 1),
#              paste("Error: Not all 'Longitude tick location' entries",
#                    "are within the provided map range")),
#         need(all(y.intervals == 1),
#              paste("Error: Not all 'Latitude tick location' entries",
#                    "are within the provided map range"))
#       )
#
#       list.scales <- c(list.scales, list(x = list(at = x.at),
#                                          y = list(at = y.at)))
#     }
#
#     ## Store tick label details or lack thereof (no labels)
#     if (input$pretty_plot_tick_label) {
#       tick.lab.size <- c(input$pretty_plot_tick_label_size, 0)
#       list.scales <- c(list.scales, list(alternating = 1, cex = tick.lab.size))
#
#     } else {
#       list.scales <- c(list.scales, list(alternating = 0))
#
#     }
#   }
#
#   list.scales
# })


###############################################################################
### Generate list of additional polygons to plot
pretty_plot_addpolys_list <- reactive({

})

###############################################################################
