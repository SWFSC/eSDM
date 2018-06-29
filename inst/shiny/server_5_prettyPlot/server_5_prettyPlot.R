### Code for creating high quality (plots) of any sets of predictions
# Flags used in prettyPlot ui file


###############################################################################
# Idx of returned object (list or vector) corresponds to...
# ...table idx (orig, over, ens)
# Each function calls input$... so that there isn't a reactive chain within
# these funcs

### Return list of selected rows (SDMS to plot)
pretty_plot_models_idx_list <- reactive({
  list(input$pretty_table_orig_out_rows_selected,
       input$pretty_table_over_out_rows_selected,
       input$pretty_table_ens_out_rows_selected)
})

### Returns vector of logicals representing whether a table has a row selected
pretty_plot_tables_null <- reactive({
  idx.list <- list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )

  sapply(idx.list, is.null)
})

### Returns number of selected rows (SDMs to plot)
pretty_plot_models_idx_count <- reactive({
  idx.list <- list(
    input$pretty_table_orig_out_rows_selected,
    input$pretty_table_over_out_rows_selected,
    input$pretty_table_ens_out_rows_selected
  )

  length(unlist(idx.list))
})


###############################################################################
# Flags

### Flag for if any model predictions are in app
output$pretty_display_flag <- reactive({
  list.models.all <- list(vals$models.ll, vals$overlaid.models,
                          vals$ensemble.models)

  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "pretty_display_flag", suspendWhenHidden = FALSE)

### Flag for number of model predictions are selected to pretty plot
output$pretty_pred_selected_flag <- reactive({
  length(unlist(pretty_plot_models_idx_list())) == 1
})
outputOptions(output, "pretty_pred_selected_flag", suspendWhenHidden = FALSE)

### Flag for if map has been generated (download flag)
output$pretty_display_download <- reactive({
  length(vals$pretty.params.list) > 0
})
outputOptions(output, "pretty_display_download", suspendWhenHidden = FALSE)


###############################################################################
### Preview of selected color palette and number of colors
pretty_plot_color_preview <- reactive({
  color.num     <- pretty_plot_colorscheme_palette_num()[[1]]
  color.palette <- pretty_plot_colorscheme_palette_num()[[2]]
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
