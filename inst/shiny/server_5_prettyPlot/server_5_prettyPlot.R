### Code for creating high quality (plots) of any sets of predictions
# Flags used in prettyPlot ui file


###############################################################################
observeEvent(input$pretty_table_orig_out_rows_selected, {
  x <- input$pretty_table_orig_out_rows_selected
  y <- input$pretty_table_over_out_rows_selected
  z <- input$pretty_table_ens_out_rows_selected

  if(!is.null(x) & !is.null(y)) {
    dataTableProxy("pretty_table_over_out") %>% selectRows(list())
  }
  if(!is.null(x) & !is.null(z)) {
    dataTableProxy("pretty_table_ens_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)

observeEvent(input$pretty_table_over_out_rows_selected, {
  x <- input$pretty_table_orig_out_rows_selected
  y <- input$pretty_table_over_out_rows_selected
  z <- input$pretty_table_ens_out_rows_selected

  if(!is.null(y) & !is.null(x)) {
    dataTableProxy("pretty_table_orig_out") %>% selectRows(list())
  }
  if(!is.null(y) & !is.null(z)) {
    dataTableProxy("pretty_table_ens_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)

observeEvent(input$pretty_table_ens_out_rows_selected, {
  x <- input$pretty_table_orig_out_rows_selected
  y <- input$pretty_table_over_out_rows_selected
  z <- input$pretty_table_ens_out_rows_selected

  if(!is.null(z) & !is.null(x)) {
    dataTableProxy("pretty_table_orig_out") %>% selectRows(list())
  }
  if(!is.null(z) & !is.null(y)) {
    dataTableProxy("pretty_table_over_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)


###############################################################################
# Idx of returned object (list or vector) corresponds to...
# ...table idx (orig, over, ens)
# Each function calls input$... so that there isn't a reactive chain within
# these funcs

### Return list of selected rows (SDMS to plot)
pretty_models_idx_list <- reactive({
  list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )
})

### Returns vector of two numbers: table in which table is selected and idx of selected row
pretty_table_row_idx <- reactive({
  row.list <- list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )
  req(length(unlist(row.list)) == 1)

  table.idx <- which(!sapply(row.list, is.null))
  row.idx <- row.list[[table.idx]]

  c(table.idx, row.idx)
})

### Returns number of selected rows (SDMs to plot)
pretty_models_idx_count <- reactive({
  row.list <- list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )

  length(unlist(row.list))
})


###############################################################################
# Flags

### Flag for if any model predictions are in app
output$pretty_display_flag <- reactive({
  list.models.all <- list(
    vals$models.ll, vals$overlaid.models, vals$ensemble.models
  )
  any(vapply(list.models.all, length, 1) > 0)
})
outputOptions(output, "pretty_display_flag", suspendWhenHidden = FALSE)

### Flag for whether or not to display parameter widgets
output$pretty_params_display_flag <- reactive({
  input$pretty_mapcontrol == 1 & length(unlist(pretty_models_idx_list())) == 1
})
outputOptions(output, "pretty_params_display_flag", suspendWhenHidden = FALSE)

### Flag for if any object have been added to to-plot list
output$pretty_display_toplot_flag <- reactive({
  isTruthy(vals$pretty.params.toplot)
})
outputOptions(output, "pretty_display_toplot_flag", suspendWhenHidden = FALSE)


###############################################################################
# Clear data table selections whenever map control option is changed
observeEvent(input$pretty_mapcontrol, {
  dataTableProxy("pretty_table_orig_out") %>% selectRows(list())
  dataTableProxy("pretty_table_over_out") %>% selectRows(list())
  dataTableProxy("pretty_table_ens_out") %>% selectRows(list())
  dataTableProxy("pretty_update_table_out") %>% selectRows(list())
})


###############################################################################
### Preview of selected color palette and number of colors
pretty_color_preview <- reactive({
  color.palette <- pretty_colorscheme_palette_num()[[1]]
  color.num     <- pretty_colorscheme_palette_num()[[2]]
  color.labels  <- color.num:1

  # Set plot margins to minimal for top, right, and bottom to fill space
  par(mai = c(0.1, 0.82, 0.1, 0))
  image(1, 1:color.num, t(as.matrix(1:color.num)), col = color.palette,
        axes = FALSE, xlab = "", ylab = "")

  graphics::box(col = "black")
  axis(2, at = 1:color.num, labels = color.labels, tick = FALSE, las = 1)
  par(mai = c(1.02, 0.82, 0.82, 0.42))
})


###############################################################################
### Reset colourInput()'s when 'transparent' checkboxes are checked
observeEvent(input$pretty_na_color_check, {
  if (input$pretty_na_color_check) {
    shinyjs::reset("pretty_na_color")
  }
})

observeEvent(input$pretty_addobj_color_ptfillcheck, {
  if (input$pretty_addobj_color_ptfillcheck) {
    shinyjs::reset("pretty_addobj_color_ptfill")
  }
})

observeEvent(input$pretty_addobj_color_absbordercheck, {
  if (input$pretty_addobj_color_absbordercheck) {
    shinyjs::reset("pretty_addobj_color_absborder")
  }
})


###############################################################################
# Objects used multiple times

choices.list.pch <- list(
  "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2,
  "3: Plus" = 3, "4: X" = 4, "5: Open Diamond" = 5,
  "6: Open Down Triangle" = 6, "7: Square with X" = 7, "8: Asterisk" = 8,
  "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
  "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12,
  "13: Circle with X" = 13, "14: Square with Up Triangle" = 14,
  "15: Filled Square" = 15, "16: Filled Circle" = 16,
  "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
  "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
)

choices.list.lty <- list(
  "1: Solid" = 1, "2: Dash" = 2, "3: Dot" = 3, "4: Dot-dash" = 4,
  "5: Long dash" = 5, "6: Dot-long dash" = 6
)

choices.list.posout <- list(
  "Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top"
)

choices.list.pos <- list(
  "Top left" = 1, "Top center" = 2, "Top right" = 3, "Center right" = 4,
  "Bottom right" = 5, "Bottom center" = 6, "Bottom left" = 7,
  "Center left" = 8
)

list.pos.vals <- list(
  c("left", "top"), c("center", "top"), c("right", "top"),
  c("right", "center"),
  c("right", "bottom"), c("center", "bottom"), c("left", "bottom"),
  c("left", "center")
)

message.360 <- paste(
  "The selected predictions span the antimeridian (180 decimal degrees),",
  "and thus longitude map range and grid line values must be within",
  "the range [0, 360] decimal degrees",
  "or the equivalant range for the specified coordinate system"
)

###############################################################################
