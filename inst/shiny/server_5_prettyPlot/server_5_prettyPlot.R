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
pretty_plot_models_idx_list <- reactive({
  list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )
})

### Returns vector of two numbers: table in which table is selected and idx of selected row
pretty_plot_table_row_idx <- reactive({
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
pretty_plot_models_idx_count <- reactive({
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

  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "pretty_display_flag", suspendWhenHidden = FALSE)

### Flag for number of model predictions are selected to pretty plot
output$pretty_params_display_flag <- reactive({
  if (input$pretty_plot_mapcontrol == 1) {
    length(unlist(pretty_plot_models_idx_list())) == 1

  } else if (input$pretty_plot_mapcontrol == 2) {
    isTruthy(input$pretty_plot_update_table_out_rows_selected)

  } else { #input$pretty_plot_mapcontrol == 3
    FALSE
  }
})
outputOptions(output, "pretty_params_display_flag", suspendWhenHidden = FALSE)



###############################################################################
### Preview of selected color palette and number of colors
pretty_plot_color_preview <- reactive({
  color.palette <- pretty_plot_colorscheme_palette_num()[[1]]
  color.num     <- pretty_plot_colorscheme_palette_num()[[2]]
  color.labels  <- color.num:1
  # if (!input$pretty_plot_color_na_transparent) {
  #   color.labels <- c("NA", color.labels)
  #   color.num <- color.num + 1
  #   color.palette <- c(input$pretty_plot_color_na ,color.palette)
  # }

  # Set plot margins to minimal for top, right, and bottom to fill space
  par(mai = c(0.1, 0.82, 0.1, 0))
  image(1, 1:color.num, t(as.matrix(1:color.num)), col = color.palette,
        axes = FALSE, xlab = "", ylab = "")

  graphics::box(col = "black")
  axis(2, at = 1:color.num, labels = color.labels, tick = FALSE, las = 1)
  par(mai = c(1.02, 0.82, 0.82, 0.42))
})

###############################################################################
### Reset background color to white
observeEvent(input$pretty_plot_color_na_reset_execute, {
  shinyjs::reset("pretty_plot_color_na")
})

observeEvent(input$pretty_plot_background_reset_execute, {
  shinyjs::reset("pretty_plot_background_color")
})
