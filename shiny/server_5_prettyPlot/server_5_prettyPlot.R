### Code for creating high quality (plots) of any sets of predictions
# Flags used in prettyPlot ui file


###############################################################################
### Return various summaries of number of tables that have 1+ row(s) selected
pretty_plot_xyz_list <- reactive({
  x <- input$pretty_table_orig_out_rows_selected
  y <- input$pretty_table_over_out_rows_selected
  z <- input$pretty_table_ens_out_rows_selected
  
  list(x, y, z)
})

pretty_plot_xyz_null <- reactive({
  sapply(pretty_plot_xyz_list(), is.null)
})

pretty_plot_xyz_count <- reactive({
  sum(!pretty_plot_xyz_null())
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
  models.selected.num <- length(unlist(pretty_plot_xyz_list()))

  case_when(models.selected.num == 0 ~ 0,
            models.selected.num == 1 ~ 1,
            TRUE ~ 2)
})
outputOptions(output, "pretty_pred_selected_flag", suspendWhenHidden = FALSE)

###############################################################################
