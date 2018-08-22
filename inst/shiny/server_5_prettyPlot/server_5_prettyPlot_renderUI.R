### renderUI code for 'High Quality Maps' tab


###############################################################################
# Map control widgets
output$pretty_toplot_remove_execute_uiOut_button <- renderUI({
  req(pretty_toplot_table())

  actionButton("pretty_toplot_remove_execute",
               "Remove selected saved maps")
})

output$pretty_toplot_add_id_uiOut_text <- renderUI({
  val.default <- "Map ID"
  if (pretty_models_idx_count() == 1) {

    table.idx <- pretty_table_row_idx()[1]
    model.idx <- pretty_table_row_idx()[2]

    val.default <- switch(
      table.idx, paste("Original", model.idx),
      paste("Overlaid", model.idx), paste("Ensemble", model.idx)
    )
    val.default <- paste(val.default, "ID")
  }

  textInput("pretty_toplot_add_id", tags$h5("Saved map ID"),
            value = val.default)
})


###############################################################################
# Range and projection of map

### Select object with desired projection for map
output$pretty_proj_idx_uiOut_select <- renderUI({
  req(vals$models.names)

  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("pretty_proj_idx",
              tags$h5("Filename of original model predictions",
                      "with desired coordinate system for the map"),
              choices = choices.list, selected = 1)
})

### Get extent of selected predictions
pretty_range <- reactive({
  req(pretty_models_idx_count() == 1)
  round(st_bbox(pretty_model_toplot()), 2)
})

### Render longitude, latitude min and max
output$pretty_range_xmin_uiOut_num <- renderUI({
  val.default <- pretty_range()$xmin
  numericInput("pretty_range_xmin", tags$h5("Longitude minimum"),
               value = val.default, min = -180, max = 180)
})

output$pretty_range_xmax_uiOut_num <- renderUI({
  val.default <- pretty_range()$xmax
  numericInput("pretty_range_xmax", tags$h5("Longitude maximum"),
               value = val.default, min = -180, max = 180)
})

output$pretty_range_ymin_uiOut_num <- renderUI({
  val.default <- pretty_range()$ymin
  numericInput("pretty_range_ymin", tags$h5("Latitude minimum"),
               value = val.default, min = -90, max = 90)
})

output$pretty_range_ymax_uiOut_num <- renderUI({
  val.default <- pretty_range()$ymax
  numericInput("pretty_range_ymax", tags$h5("Latitude maximum"),
               value = val.default, min = -90, max = 90)
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
    "Default: blue to white to red" = 1,
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

  # choices.list <- c("Choose color palette" = "", choices.list)

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
observe({
  val.pretty.color.num(input$pretty_color_num)
})

observeEvent(input$pretty_color_palette, {
  req(input$pretty_color_palette == 2)
  val.pretty.color.num(11)
})

observeEvent(input$pretty_color_palette, {
  req(input$pretty_color_palette == 3)
  val.pretty.color.num(9)
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

  # val.default <- ifelse(
  #   table.idx == 3,
  #   paste("Ensembling method:", vals$ensemble.method[model.idx], "||",
  #         "Rescaling method:", vals$ensemble.rescaling[model.idx]),
  #   paste("Model file:", vals$models.names[model.idx], "||",
  #         "Data header:", vals$models.data.names[[model.idx]][1])
  # )

  textInput("pretty_title", tags$h5("Map title"), value = val.default)
})


###############################################################################
# Coordinate grid lines and labels

### Longitude start
output$pretty_tick_lon_start_uiOut_numeric <- renderUI({
  req(input$pretty_range_xmin, input$pretty_range_xmax)

  if (input$pretty_range_xmin >= input$pretty_range_xmax) {
    val.def <- 1

  } else {
    num.range <- c(input$pretty_range_xmin, input$pretty_range_xmax)
    val.def <- base::pretty(num.range, n = 5)
    val.def <- val.def[dplyr::between(val.def, num.range[1], num.range[2])][1]
  }

  numericInput("pretty_tick_lon_start", tags$h5("Longitude grid line start"),
               value = val.def, min = 0, step = 5)
})


### Longitude interval
output$pretty_tick_lon_interval_uiOut_numeric <- renderUI({
  req(input$pretty_range_xmin, input$pretty_range_xmax)

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

  numericInput("pretty_tick_lon_interval", tags$h5("Longitude grid line interval"),
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

  numericInput("pretty_tick_lat_start", tags$h5("Latitude grid line start"),
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

  numericInput("pretty_tick_lat_interval", tags$h5("Latitude grid line interval"),
               value = val.def, min = 0, step = val.def)
})


###############################################################################
# Additional polygons
# Located in 'server_5_prettyPlot_addpolys.R'


###############################################################################
# Download
# Located in 'server_5_prettyPlot_download.R'

###############################################################################
