### Code for creating high quality (plots) of any sets of predictions


###############################################################################
### Return number of tables that have at least one row selected
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
  list.models.all <- list(vals$models.ll, 
                          vals$overlaid.models, 
                          vals$ensemble.models)
  
  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "pretty_display_flag", suspendWhenHidden = FALSE)

### Flag for if any model predictions are selected
output$pretty_pred_selected_flag <- reactive({
  models.selected.num <- length(unlist(pretty_plot_xyz_list()))

  case_when(models.selected.num == 0 ~ 0,
            models.selected.num == 1 ~ 1,
            TRUE ~ 2)
})
outputOptions(output, "pretty_pred_selected_flag", suspendWhenHidden = FALSE)


###############################################################################
# Get sets of predictions to plot

### Process selected rows from tables of predictions
# Return list of [which tables have selected rows, a 3 element list of the...
# ...rows selected in those tables, a 3 element list of selected spdfs]
# '3 element lists' correspond to the 3 tables
pretty_model_toplot_list <- reactive({
  xyz.null <- pretty_plot_xyz_null()
  xyz.count <- pretty_plot_xyz_count()
  req(xyz.count == 1)
  
  validate(
    need(xyz.count == 1, "Pretty plots not ready for multiplots")
  )
  
  table.idx <- which(!xyz.null)
  model.idx.list <- pretty_plot_xyz_list()
  
  models.list.orig <- vals$models.ll[model.idx.list[[1]]]
  models.list.over <- vals$overlaid.models[model.idx.list[[2]]]
  models.list.ens <- vals$ensemble.models[model.idx.list[[3]]]
  
  models.all <- list(models.list.orig, models.list.over, models.list.ens)
  models.all.ll <- lapply(models.all, function(j.list) {
    lapply(j.list, function(j) {
      if (identical(crs(j), crs.ll)) { 
        j
      } else {
        spTransform(j, crs.ll)
      }
    })
  })
  
  list(table.idx, model.idx.list, models.all.ll)
})

### Return list of [table idx, pred idx, currently selected model predictions]
# Currently there should only be one model
pretty_model_toplot <- reactive({
  vals <- pretty_model_toplot_list()
  
  table.which <- vals[[1]]
  model.which <- vals[[2]][[table.which]]
  model.spdf <- vals[[3]][[table.which]][[1]]
  
  list(table.which, model.which, model.spdf)
})


###############################################################################
