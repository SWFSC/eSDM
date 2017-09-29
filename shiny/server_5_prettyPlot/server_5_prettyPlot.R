### Code for creating high quality (plots) of any sets of predictions
# Flags used in prettyPlot ui file


###############################################################################
# Idx of returned object (list or vector) corresponds to...
# ...table idx (orig, over, ens)

### Return list of selected rows (SDMS to plot)
pretty_plot_models_idx_list <- reactive({
  list(input$pretty_table_orig_out_rows_selected, 
       input$pretty_table_over_out_rows_selected, 
       input$pretty_table_ens_out_rows_selected)
})

### Returns vector of logicals representing whether a table has a row selected
pretty_plot_tables_null <- reactive({
  sapply(pretty_plot_models_idx_list(), is.null)
})

### Returns number of selected rows (SDMs to plot)
pretty_plot_models_idx_count <- reactive({
  length(unlist(pretty_plot_models_idx_list()))
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
  models.selected.num <- length(unlist(pretty_plot_models_idx_list()))
  
  case_when(models.selected.num == 0 ~ 0,
            models.selected.num == 1 ~ 1,
            TRUE ~ 2)
})
outputOptions(output, "pretty_pred_selected_flag", suspendWhenHidden = FALSE)


###############################################################################
### Preview of selected color palette and number of colors
pretty_plot_color_preview <- reactive({
  color.num     <- pretty_plot_colorscheme_palette_num()[[1]]
  color.palette <- pretty_plot_colorscheme_palette_num()[[2]]
  
  pie(rep(1, color.num), col = rev(color.palette))
})
