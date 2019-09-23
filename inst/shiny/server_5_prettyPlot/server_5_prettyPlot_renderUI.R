### renderUI code for 'High Quality Maps' tab


###############################################################################
# 'Specify map ID and save map' box

### Default title for map to be saved
output$pretty_toplot_add_id_uiOut_text <- renderUI({
  if (pretty_models_idx_count() == 1) {
    table.idx <- pretty_table_row_idx()[1]
    model.idx <- pretty_table_row_idx()[2]

    val.default <- switch(
      table.idx, paste("Original", model.idx),
      paste("Overlaid", model.idx), paste("Ensemble", model.idx)
    )
    val.default <- paste(val.default, "ID")

  } else  {
    val.default <- "Map ID"
  }

  textInput("pretty_toplot_add_id", tags$h5("Saved map ID"),
            value = val.default)
})

#------------------------------------------------------------------------------
### Checkbox to save map of associated SE values
output$pretty_toplot_se_uiOut_check <- renderUI({
  if (pretty_models_idx_count() == 1) {
    if (pretty_table_row_idx()[1] %in% 1:2) {
      if (!is.na(vals$models.data.names[[pretty_table_row_idx()[2]]][2]))  {
        input.lab <- paste(
          "Also save a map of the uncertainty (SE) values",
          "of the selected predictions"
        )

        checkboxInput("pretty_toplot_se", input.lab, value = FALSE)

      } else {
        "The selected predictions do not have uncertainty (SE) values to plot"
      }

    } else if (pretty_table_row_idx()[1] == 3) {
      input.lab <- paste(
        "Also save a map of the uncertainty (SE) values of the ensemble"
      )

      checkboxInput("pretty_toplot_se", input.lab, value = FALSE)

    } else {
      NULL
    }

  } else  {
    NULL
  }
})

### Helptext about SE map
output$pretty_toplot_se_uiOut_text <- renderUI({
  if (pretty_models_idx_count() == 1 & isTruthy(input$pretty_toplot_se)) {
    helpText(
      "This uncertainty map will have the parameters specified below,",
      "except that if the color scheme unit type is \"Values\",",
      "then the map will have the same",
      "color scale break points as the map of the predictions.", tags$br(),
      "The uncertainty map name will be as specified above, ",
      "with \"_SE\" appended to the end."
    )

  } else  {
    NULL
  }
})


###############################################################################
# Map coordinate system and range

### Select object with desired projection for map
output$pretty_proj_idx_uiOut_select <- renderUI({
  req(vals$models.names)

  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("pretty_proj_idx",
              tags$h5("Filename of original predictions",
                      "with desired coordinate system for the map"),
              choices = choices.list, selected = 1)
})

#------------------------------------------------------------------------------
### Get extent of selected predictions
pretty_range <- reactive({
  req(pretty_models_idx_count() == 1)

  x <- st_geometry(pretty_model_toplot())

  if (pretty_range_360()) {
    y1 <- st_sfc(st_polygon(list(
      matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
    )), crs = 4326)
    y1 <- st_transform(y1, pretty_crs_selected())

    y2 <- st_sfc(st_polygon(list(
      matrix(c(180, 0, 0, 180, 180, -90, -90, 90, 90, -90), ncol = 2)
    )), crs = 4326)
    y2 <- st_transform(y2, pretty_crs_selected())
    lon.add <- unname(st_bbox(y2))[3] * 2

    x1.b <- st_bbox(suppressMessages(st_intersection(x, y1)))
    x2.b <- st_bbox(suppressMessages(st_intersection(x, y2)))

    c(x2.b[1], ymin = min(x1.b[2], x2.b[2]), x1.b[3] + lon.add,
      ymax = max(x1.b[4],x2.b[4]))

  } else {
    st_bbox(x)
  }
})

# From https://stackoverflow.com/questions/35807523/r-decimal-ceiling
esdm_floor <-   function(x, level = 2) round(x - 5 * 10 ^ (-level - 1), level)
esdm_ceiling <- function(x, level = 2) round(x + 5 * 10 ^ (-level - 1), level)

#------------------------------------------------------------------------------
### Message about 0-360 range, message.360 is in 'server_5_prettyPlot.R'
output$pretty_range_360_uiOut_text <- renderUI({
  req(pretty_range_360())

  tags$h5(message.360, style = "color: red;")
})


### Render longitude, latitude min and max
output$pretty_range_xmin_uiOut_num <- renderUI({
  val.default <- esdm_floor(pretty_range()["xmin"])
  numericInput("pretty_range_xmin", tags$h5("Longitude minimum"),
               value = val.default)
})

output$pretty_range_xmax_uiOut_num <- renderUI({
  val.default <- ceiling(pretty_range()["xmax"])
  numericInput("pretty_range_xmax", tags$h5("Longitude maximum"),
               value = val.default)
})

output$pretty_range_ymin_uiOut_num <- renderUI({
  val.default <- esdm_floor(pretty_range()["ymin"])
  numericInput("pretty_range_ymin", tags$h5("Latitude minimum"),
               value = val.default)
})

output$pretty_range_ymax_uiOut_num <- renderUI({
  val.default <- esdm_ceiling(pretty_range()["ymax"])
  numericInput("pretty_range_ymax", tags$h5("Latitude maximum"),
               value = val.default)
})

#------------------------------------------------------------------------------
### Button to update map range values
output$pretty_range_lastmap_uiOut_button <- renderUI({
  validate(
    need(vals$pretty.params.toplot,
         "You must have saved at least one map to update range values"),
    errorClass = "validation2"
  )

  actionButton("pretty_range_lastmap", "Update range values")
})

### Update map range values to range of last saved map
pretty_range_lastmap_execute <- observeEvent(input$pretty_range_lastmap, {
  d <- tail(req(vals$pretty.params.toplot), 1)[[1]]$map.range

  updateNumericInput(session, "pretty_range_xmin", value = d[1])
  updateNumericInput(session, "pretty_range_xmax", value = d[2])
  updateNumericInput(session, "pretty_range_ymin", value = d[3])
  updateNumericInput(session, "pretty_range_ymax", value = d[4])
})


###############################################################################
# Color scheme inputs

### Color of NA predictions
output$pretty_na_color_uiOut_colour <- renderUI({
  x <- st_set_geometry(pretty_model_toplot(), NULL)[, 1]

  validate(
    need(anyNA(x), "The selected predictions do not have any NA predictions"),
    errorClass = "validation2"
  )

  colourpicker::colourInput(
    "pretty_na_color", tags$h5("Click to select color of NA predictions"),
    showColour = "background", value = "gray"
  )
})

### Color palette
# If 'plot predictions as percentages' is selected, then remove some options
output$pretty_color_palette_uiOut_select <- renderUI({
  choices.list <- list(
    "Default: red to white to blue" = 1,
    "RColorBrewer: Spectral (rainbow)" = 2,
    "RColorBrewer: YlGnBu" = 3, "viridis: viridis" = 4,
    "viridis: inferno" = 5, "dichromat: DarkRedtoBlue" = 6
  )

  # Remove palettes that don't work with 10 colors
  if (input$pretty_color_perc == 1) choices.list <- choices.list[-c(3, 6)]

  # If possible, keep same selected palette when going from perc to values
  col.sel <- input$pretty_color_palette
  if (isTruthy(col.sel)) {
    if (!(col.sel %in% choices.list)) col.sel <- 1
  }

  selectInput("pretty_color_palette", tags$h5("Color palette"),
              choices = choices.list, selected = col.sel, width = "80%")
})

### Number of colors
# Selectively give user input control, depending on perc/palette
# input$pretty_color_palette is NULL if helpText() is outputted,
# but colorschem.num value is hardcoded for those situations in
# pretty_colorscheme_list()
output$pretty_color_num_uiOut_num <- renderUI({
  if (input$pretty_color_perc == 1) {
    helpText("The number of colors must be ten when color-coding",
             "predictions by relative percentages")

  } else {
    if (input$pretty_color_palette == 1) {
      helpText("The number of colors must be 10 when",
               "using this color palette")

    } else if (input$pretty_color_palette == 2) {
      numericInput("pretty_color_num",
                   tags$h5("Number of colors (Min: 3; Max: 11)"),
                   value = 11, step = 1, min = 3, max = 11, width = "80%")

    } else if (input$pretty_color_palette == 3) {
      numericInput("pretty_color_num",
                   tags$h5("Number of colors (Min: 3; Max: 9)"),
                   value = 9, step = 1, min = 3, max = 9, width = "80%")

    } else if (input$pretty_color_palette == 6) {
      helpText("The number of colors must be 12 when",
               "using this color palette")

    } else {
      numericInput("pretty_color_num", tags$h5("Number of colors"),
                   value = 10, step = 1, min = 1, width = "80%")
    }
  }
})

# These do things - 'val.pretty.color.num' is a reactiveVal() output
observe(val.pretty.color.num(input$pretty_color_num))
observeEvent(input$pretty_color_palette, {
  if (input$pretty_color_palette == 2) val.pretty.color.num(11)
  if (input$pretty_color_palette == 3) val.pretty.color.num(9)
})
observeEvent(input$pretty_color_perc, {
  if (input$pretty_color_perc == 2) {
    if (input$pretty_color_palette == 1) val.pretty.color.num(10)
    if (input$pretty_color_palette == 6) val.pretty.color.num(12)
  }
})


###############################################################################
# Legend

# Legend position
output$pretty_legend_pos_uiOut_select <- renderUI({
  if (input$pretty_legend_inout == 1) {
    choices.list <- choices.list.pos
    choices.sel <- 3

  } else { #input$pretty_legend_inout == 2
    choices.list <- choices.list.posout
    choices.sel <- 1
  }

  selectInput("pretty_legend_pos", tags$h5("Legend position"),
              choices = choices.list, selected = choices.sel)
})

# Legend width
output$pretty_legend_width_uiOut_numeric <- renderUI({
  if (input$pretty_legend_inout == 1) {
    helpText("The legend width will be determined by the 'Legend text size' input")

  } else {
    numericInput("pretty_legend_width", tags$h5("Legend width"),
                 value = 0.2, min = 0.1, max = 0.5, step = 0.05)
  }
})


###############################################################################
### Title of plot
output$pretty_title_uiOut_text <- renderUI({
  req(pretty_models_idx_count() == 1)

  table.idx <- pretty_table_row_idx()[1]
  model.idx <- pretty_table_row_idx()[2]

  val.default <- switch(
    table.idx, paste("Original", model.idx),
    paste("Overlaid", model.idx), paste("Ensemble", model.idx)
  )

  textInput("pretty_title", tags$h5("Map title"), value = val.default)
})


###############################################################################
# Coordinate grid marks and labels

### Longitude start
output$pretty_tick_lon_start_uiOut_numeric <- renderUI({
  if (!isTruthy(input$pretty_range_xmin) | !isTruthy(input$pretty_range_xmax)) {
    val.def <- 1

  } else {
    if (input$pretty_range_xmin >= input$pretty_range_xmax) {
      val.def <- 1

    } else {
      num.range <- c(input$pretty_range_xmin, input$pretty_range_xmax)
      val.def <- base::pretty(num.range, n = 5)
      val.def <- val.def[dplyr::between(val.def, num.range[1], num.range[2])][1]
    }
  }

  numericInput("pretty_tick_lon_start", tags$h5("Longitude grid mark start"),
               value = val.def, min = 0, step = 5)
})


### Longitude interval
output$pretty_tick_lon_interval_uiOut_numeric <- renderUI({
  if (!isTruthy(input$pretty_range_xmin) | !isTruthy(input$pretty_range_xmax)) {
    val.def <- 1

  } else {
    if (input$pretty_range_xmin >= input$pretty_range_xmax) {
      val.def <- 1

    } else {
      range.diff <- input$pretty_range_xmax - input$pretty_range_xmin
      if (range.diff > 360) {
        temp <- grDevices::axisTicks(
          usr = c(input$pretty_range_xmin, input$pretty_range_xmax),
          log = FALSE, nint = 5
        )
        val.def <- diff(temp)[1]

      } else if (range.diff >= 50) {
        val.def <- 10
      } else if (range.diff >= 10) {
        val.def <- 5
      } else if (range.diff >= 5) {
        val.def <- 1
      } else {
        val.def <- round(range.diff / 5, 1)
      }
    }
  }

  numericInput("pretty_tick_lon_interval", tags$h5("Longitude grid mark interval"),
               value = val.def, min = 0, step = val.def)
})


### Latitude start
output$pretty_tick_lat_start_uiOut_numeric <- renderUI({
  req(input$pretty_range_ymin, input$pretty_range_ymax)

  if (input$pretty_range_ymin >= input$pretty_range_ymax) {
    val.def <- 1

  } else {
    num.range <- c(input$pretty_range_ymin, input$pretty_range_ymax)
    val.def <- base::pretty(num.range, n = 5)
    val.def <- val.def[dplyr::between(val.def, num.range[1], num.range[2])][1]
  }

  numericInput("pretty_tick_lat_start", tags$h5("Latitude grid mark start"),
               value = val.def, min = 0, step = 0.1)
})


### Latitude interval
# pretty_tick_lat_interval_uiOut_numeric
output$pretty_tick_lat_interval_uiOut_numeric <- renderUI({
  req(input$pretty_range_ymin, input$pretty_range_ymax)

  if (input$pretty_range_ymin >= input$pretty_range_ymax) {
    val.def <- 1

  } else {
    range.diff <- input$pretty_range_ymax - input$pretty_range_ymin
    if (range.diff > 180) {
      temp <- grDevices::axisTicks(
        usr = c(input$pretty_range_ymin, input$pretty_range_ymax),
        log = FALSE, nint = 5
      )
      val.def <- diff(temp)[1]

    } else if (range.diff >= 50) {
      val.def <- 10
    } else if (range.diff >= 10) {
      val.def <- 5
    } else if (range.diff >= 5) {
      val.def <- 1
    } else {
      val.def <- round(range.diff / 5, 1)
    }
  }

  numericInput("pretty_tick_lat_interval", tags$h5("Latitude grid mark interval"),
               value = val.def, min = 0, step = val.def)
})


###############################################################################
# Additional polygons
# Located in 'server_5_prettyPlot_addpolys.R'


###############################################################################
# Download
# Located in 'server_5_prettyPlot_download.R'

###############################################################################
