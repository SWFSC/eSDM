### Functions that return data used in server_5_prettyPlot_plot.R
### Get model(s) to plot, generate lists with plotting information

### NOTE ###
# numericInput()'s convert everything to numeric, so entries or symbols are
#   coerced to numeric NA


###############################################################################
# Get set(s) of predictions to plot

### Process selected rows from tables of predictions
# Return list of [which tables have selected rows, a 3 element list of the...
# ...rows selected in those tables, a 3 element list of selected spdfs]
# '3 element lists' correspond to the 3 tables
pretty_model_selected <- reactive({
  req(pretty_models_idx_count() == 1)

  model.idx.list <- pretty_models_idx_list()
  if (isTruthy(model.idx.list[[1]])) {
    vals$models.orig[[model.idx.list[[1]]]]

  } else if (isTruthy(model.idx.list[[2]])) {
    st_sf(
      vals$overlaid.models[[model.idx.list[[2]]]],
      geometry = vals$overlay.base.sfc, agr = "constant"
    )

  } else if (isTruthy(model.idx.list[[3]])) {
    st_sf(
      vals$ensemble.models[[model.idx.list[[3]]]],
      geometry = vals$overlay.base.sfc, agr = "constant"
    )
  } else {
    validate("High quality map error; please report this as an issue")
  }
})


### Get desired projection (crs object) for map
pretty_crs_selected <- reactive({
  if (input$pretty_proj_ll) {
    crs.ll

  } else {
    if (input$pretty_proj_method == 1) {
      req(pretty_models_idx_count() == 1)
      model.idx.list <- pretty_models_idx_list()
      validate(
        need(length(model.idx.list) == 3,
             "High quality map crs error; please report this as an issue")
      )

      if (isTruthy(model.idx.list[[1]])) {
        st_crs(vals$models.orig[[model.idx.list[[1]]]])

      } else {
        vals$overlay.crs #st_crs(vals$overlay.base.sfc)
      }

    } else if (input$pretty_proj_method == 2) {
      st_crs(vals$models.orig[[req(as.numeric(input$pretty_proj_idx))]])

    } else {
      x <- st_crs(input$pretty_proj_epsg)
      validate(
        need(x[[2]],
             paste("Error: The provided EPSG code was not recognized;",
                   "please provide a valid code"))
      )

      x
    }
  }
})


### Return model specified by user
pretty_model_toplot <- reactive({
  st_transform(pretty_model_selected(), pretty_crs_selected())
})


### Returns logical indicating whether [0, 360] range needs to be used
pretty_range_360 <- reactive({
  req(pretty_models_idx_count() == 1)

  check_360(pretty_model_toplot())
})


### Return model specified by user, transformed to 0-360 range
# Used in multiple places (toplot and color scheme), hence reactive
# Only called after check has already been done, hence no check_preview360()
pretty_model_toplot360 <- reactive({
  preview360_split(pretty_model_toplot())
})


###############################################################################
### Compile map range (plot limits)
pretty_map_range <- reactive({
  req(c(
    input$pretty_range_xmin, input$pretty_range_xmax,
    input$pretty_range_ymin, input$pretty_range_ymax
  ))
})


###############################################################################
# Color scheme of predictions

#------------------------------------------------------------------------------
### Process inputs and return list with num of colors and color palette to use
pretty_colorscheme_palette_num <- reactive({
  req(input$pretty_color_palette)

  perc <- input$pretty_color_perc == 1
  color.palette.idx <- input$pretty_color_palette

  if (perc) {
    color.num <- 10

  } else {
    color.num <- val.pretty.color.num()
    validate(
      need(color.num, "Error: The 'Number of colors' entry must be a number")
    )
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
    validate("Error: Error in Color Scheme processing")
  }

  list(color.palette, color.num)
})


#------------------------------------------------------------------------------
### Generate list that specifies color scheme things
pretty_colorscheme_list <- reactive({
  #----------------------------------------------------------
  ### NA color
  if (input$pretty_na_color_check) {
    col.na <- NULL
  } else {
    col.na <- input$pretty_na_color
  }

  #----------------------------------------------------------
  ### Get reactive elements
  perc <- input$pretty_color_perc == 1
  color.palette <- pretty_colorscheme_palette_num()[[1]]
  color.num     <- pretty_colorscheme_palette_num()[[2]]


  #----------------------------------------------------------
  ### Determine data break points and legend labels
  # Prep
  if (pretty_range_360()) {
    x <- pretty_model_toplot360()
  } else {
    x <- pretty_model_toplot()
  }

  data.name <- switch(pretty_table_row_idx()[1], "Pred", "Pred", "Pred_ens")

  # Call function
  temp <- pretty_colorscheme_func(
    x, data.name, pretty_map_range(), perc, color.num,
    leg.perc.esdm, input$pretty_legend_round
  )

  #----------------------------------------------------------
  ### Return list
  list(
    data.name = data.name, data.breaks = temp[[1]], col.pal = color.palette,
    col.na = col.na, leg.labs = temp[[2]],
    perc = perc, leg.round = input$pretty_legend_round #incldued for update
  )
})


###############################################################################
### Generate list of legend arguments
pretty_legend_list <- reactive({
  validate(
    need(!is.na(input$pretty_legend_size),
         "Error: The legend text size entry must be a number")
  )

  if (input$pretty_legend) {
    if (input$pretty_legend_inout == 1) {
      leg.out <- FALSE
      leg.pos <- list.pos.vals[[as.numeric(input$pretty_legend_pos)]]
      leg.outside.pos <- NULL
      leg.width <- 1

    } else { #input$pretty_legend_inout == 2
      leg.out <- TRUE
      leg.pos <- NULL
      leg.outside.pos <- input$pretty_legend_pos
      leg.width <- input$pretty_legend_width

      validate(
        need(dplyr::between(leg.width, 0.1, 0.5),
             "The 'Legend width' entry must be between 0.1 and 0.5")
      )
    }

    leg.text.size <- input$pretty_legend_size
    leg.border    <- ifelse(input$pretty_legend_frame, "black", FALSE)

    list(
      inc = TRUE, out = leg.out, pos = leg.pos, out.pos = leg.outside.pos,
      text.size = leg.text.size, width = leg.width, border = leg.border
    )

  } else {
    # defaults for others params included for sake of update
    list(
      inc = FALSE, out = FALSE, pos = list.pos.vals[[3]], out.pos = NULL,
      text.size = 1, width = 1, border = "black"
    )
  }
})


###############################################################################
# Section 2
###############################################################################
### Title and axis labels
pretty_titlelab_list <- reactive({
  validate(
    need(!is.na(input$pretty_title_cex) && !is.na(input$pretty_lab_cex),
         "Error: The title and axis size entries must be numbers")
  )

  list(
    title = input$pretty_title, xlab = input$pretty_xlab,
    ylab = input$pretty_ylab, titlecex = input$pretty_title_cex,
    labcex = input$pretty_lab_cex
  )
})


###############################################################################
### Margin info
pretty_margin_list <- reactive({
  temp <- c(
    input$pretty_margin_in1, input$pretty_margin_in2, input$pretty_margin_in3,
    input$pretty_margin_in4, input$pretty_margin_out
  )

  validate(
    need(!anyNA(temp) && length(temp) == 5,
         "Error: All margin values must be numbers")
  )

  list(
    input$pretty_margin_in1, input$pretty_margin_in2, input$pretty_margin_in3,
    input$pretty_margin_in4, input$pretty_margin_out
  )
})


###############################################################################
### Generate list of coordinate grid mark and label info
pretty_tick_list <- reactive({
  validate(
    need(input$pretty_tick_lon_start,
         "Error: The 'Longitude grid mark start' entry must be a number"),
    need(input$pretty_tick_lon_interval,
         "Error: The 'Longitde grid mark interval' entry must be a number"),
    need(input$pretty_tick_lat_start,
         "Error: The 'Latitude grid mark start' entry must be a number"),
    need(input$pretty_tick_lat_interval,
         "Error: The 'Latitude grid mark interval' entry must be a number"),
    need(input$pretty_tick_lw,
         "Error: The 'Grid mark width' entry must be a number"),
    need(input$pretty_tick_alpha,
         "Error: The 'Grid mark transparency' entry must be a number"),
    need(input$pretty_tick_label_size,
         "Error: The 'Coordinate label size' entry must be a number")
  )

  validate(
    need(input$pretty_tick_lon_start < input$pretty_range_xmax,
         paste("Error: The 'Longitude grid mark start' must be less than the",
               "'Longitude maximum'")),
    need(input$pretty_tick_lat_start < input$pretty_range_ymax,
         paste("Error: The 'Latitude grid mark start' must be less than the",
               "'Latitude maximum'")),
    need(input$pretty_tick_label_size > 0,
         "Error: The 'Coordinate label size' entry must be greater than zero")
  )

  grid.ticks <- ifelse(
    input$pretty_tick_label_inout == 1, FALSE, 2 %in% input$pretty_tick_which
  )

  lon.grid.vals <- seq(
    from = input$pretty_tick_lon_start, to = input$pretty_range_xmax,
    by = input$pretty_tick_lon_interval
  )

  lat.grid.vals <- seq(
    from = input$pretty_tick_lat_start, to = input$pretty_range_ymax,
    by = input$pretty_tick_lat_interval
  )

  list(
    inc = input$pretty_tick,
    grid.lines = 1 %in% input$pretty_tick_which, grid.ticks = grid.ticks,
    x.vals = lon.grid.vals, y.vals = lat.grid.vals,
    grid.lw = input$pretty_tick_lw,
    grid.alpha = input$pretty_tick_alpha,
    grid.col = input$pretty_tick_color,
    grid.labs.size = input$pretty_tick_label_size,
    grid.labs.in = input$pretty_tick_label_inout == 1
  )
})


###############################################################################
### Generate lists of additional objects to plot
pretty_addobj_list <- reactive({
  lapply(req(vals$pretty.addobj), function(i) {
    i$obj <- st_transform(i$obj, pretty_crs_selected())

    if (pretty_range_360()) {
      if (length(i$obj) > 6000) {
        i$obj <- check_preview360_split(i$obj, force.360 = TRUE)

      } else {
        i$obj <- st_union(
          check_preview360_mod(i$obj, force.360 = TRUE),
          by_feature = TRUE
        )
      }
    }

    range.poly <- pretty_range_poly_func(
      pretty_map_range(), pretty_crs_selected()
    )
    pretty_int_func(i$obj, range.poly, tolower(i$obj.text))

    i
  })
})

###############################################################################
