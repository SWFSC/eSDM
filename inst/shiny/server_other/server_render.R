### Render various outputs for Ensemble App


###############################################################################
### Reactive functions that return tables are in server_render_tables.R


###############################################################################
##### Load Models tab #####

###########################################################
### Load saved environment output
output$load_envir_text <- renderText({
  load_envir()
})

###########################################################
# Created spdf messages

### Created spdf message for csv
output$create_sf_csv_text <- renderText({
  req(read_model_csv())
  create_sf_csv()
})

### Created spdf message for gis raster
output$create_sf_gis_raster_text <- renderText({
  req(read_model_gis_raster())
  create_sf_gis_raster()
})

### Created spdf message for gis shp
output$create_sf_gis_shp_text <- renderText({
  req(read_model_gis_shp())
  create_sf_gis_shp()
})

### Created spdf message for gis gdb
output$create_sf_gis_gdb_text <- renderText({
  req(read_model_gis_gdb())
  create_sf_gis_gdb()
})

###########################################################
# Tables

### Table of loaded original model preds
output$models_loaded_table <- DT::renderDataTable({
  table_orig()[, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'), selection = "multiple")

### Table of stats of loaded original model preds
output$models_loaded_table_stats <- DT::renderDataTable({
  table_orig_stats()
}, options = list(dom = 't'), selection = "none")

###########################################################
### Plot/preview of loaded, original model(s)
output$model_preview_plot <- renderPlot({
  model_preview_event()
})

output$model_preview_interactive_plot <- renderLeaflet({
  model_preview_interactive_event()
})


###############################################################################
##### Overlay tab #####

###########################################################
# Tables

### Table of loaded model predictions
output$overlay_loaded_table <- DT::renderDataTable({
  table_orig()[, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'), selection = "single")

### Table of stats of loaded model predictions
output$overlay_loaded_stats_table <- DT::renderDataTable({
  table_orig_stats()
}, options = list(dom = 't'), selection = "none")

###########################################################
# Polygon error outputs and loaded messages

### Boundary (study area) polygon error outputs
output$overlay_bound_csv_text     <- renderText(overlay_bound_csv())
output$overlay_bound_gis_shp_text <- renderText(overlay_bound_gis_shp())
output$overlay_bound_gis_gdb_text <- renderText(overlay_bound_gis_gdb())

### Boundary polygon loaded messages
output$overlay_bound_csv_message <- renderText({
  if (!is.null(vals$overlay.bound)) "A study area polygon is loaded"
})
output$overlay_bound_gis_shp_message <- renderText({
  if (!is.null(vals$overlay.bound)) "A study area polygon is loaded"
})
output$overlay_bound_gis_gdb_message <- renderText({
  if (!is.null(vals$overlay.bound)) "A study area polygon is loaded"
})

### Land polygon error outputs
output$overlay_land_prov_text    <- renderText(overlay_land_prov())
output$overlay_land_csv_text     <- renderText(overlay_land_csv())
output$overlay_land_gis_shp_text <- renderText(overlay_land_gis_shp())
output$overlay_land_gis_gdb_text <- renderText(overlay_land_gis_gdb())

### Land polygon loaded messages
output$overlay_land_prov_message <- renderText({
  if (!is.null(vals$overlay.land)) "A land polygon is loaded"
})
output$overlay_land_csv_message <- renderText({
  if (!is.null(vals$overlay.land)) "A land polygon is loaded"
})
output$overlay_land_gis_shp_message <- renderText({
  if (!is.null(vals$overlay.land)) "A land polygon is loaded"
})
output$overlay_land_gis_gdb_message <- renderText({
  if (!is.null(vals$overlay.land)) "A land polygon is loaded"
})

###########################################################
# Overlaying process outputs

output$overlay_overlaid_models_message <- renderText({
  if (length(vals$overlaid.models) > 0) "Overlaid models are created"
})

#######################################
### Samegrid overlay
output$overlay_samegrid_warning_text <- renderUI({
  HTML(overlay_samegrid_warning())
})
output$overlay_samegrid_all_text <- renderText({
  overlay_samegrid_all()
})

#######################################
### Standard overlay
output$overlay_overlay_all_text <- renderText({
  overlay_all()
})

###########################################################
# Previews

### Preview of base grid
# 'suspendWhenHidden = FALSE' in server_hide+show.R
output$overlay_preview_base <- renderLeaflet({
  overlay_preview_base_event()
})

### Preview of overlaid model predictions
# 'suspendWhenHidden = FALSE' in server_hide+show.R
output$overlay_preview_overlaid <- renderPlot({
  overlay_preview_overlaid_event()
})


###############################################################################
##### Create Ensembles tab #####

###########################################################
# Tables

### Display table of overlaid predictions and info
output$create_ens_table <- renderTable({
  table_overlaid()[, -3] #'[, -3]' is to remove Error column
}, rownames = TRUE)

### Datatable of overlaid predictions and info
output$create_ens_datatable <- DT::renderDataTable({
  table_overlaid()[, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'))

###########################################################
# Weights outputs

### Table of metric values to be used as weights
output$create_ens_weights_metric_table_out <- renderTable({
  create_ens_weights_metric_table()
}, rownames = FALSE, digits = 3)

### Table of if overlaid models have spatial pixel weights
output$create_ens_weights_pix_table_out <- renderTable({
  create_ens_weights_pix_table()
}, rownames = FALSE, align = "lr")

### Table summarizing overlaid models and their polygon weights
output$create_ens_weights_poly_table_out <- renderTable({
  create_ens_weights_poly_table()
}, rownames = FALSE)

### Preview plot of weight polygons
output$create_ens_weights_poly_preview_plot <- renderPlot({
  create_ens_weights_poly_preview()
})


### Text output for removing loaded weight polygons
output$create_ens_weights_poly_remove_text <- renderText({
  create_ens_weights_poly_remove()
})

### Output for adding polygon weight(s) to reactiveValues
output$create_ens_weights_poly_add_text <- renderText({
  create_ens_weights_poly_add()
})

###########################################################
### Create ensemble error/completion output
output$ens_create_ensemble_text <- renderUI({
  HTML(create_ensemble())
})

###########################################################
# Created ensemble things

### Table of created ensemble predictions
output$ens_datatable_ensembles <- DT::renderDataTable({
  table_ensembles()
},
options = list(dom = 't'))

### Remove ensemble error output
output$ens_remove_text <- renderUI({
  HTML(ens_remove())
})

### Plot preview of ensemble predictions
output$ens_preview_plot <- renderPlot({
  ens_preview_event()
})

### Table of abundances of created ensemble predictions
output$ens_abund_table_out <- renderTable({
  table_ens_abund()
}, rownames = TRUE, colnames = FALSE, align = "r")


###############################################################################
##### Model Evaluation Metrics tab #####

###########################################################
# Tables

### Table of orig model predictions
output$eval_models_table_orig_out <- DT::renderDataTable({
  table_orig()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'))

### Table of overlaid model predictions
output$eval_models_table_over_out <- DT::renderDataTable({
  table_overlaid()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'))

### Table of ensemble models
output$eval_models_table_ens_out <- DT::renderDataTable({
  table_ensembles()
}, options = list(dom = 't'))

###########################################################
# Presence/absence loaded message, error outputs, and table

# Presence and absence points
output$eval_data_message <- renderText({
  ifelse(inherits(vals$eval.data, "sf"), "Validation data loaded", "")
})

# Text outputs
output$eval_csv_data_text <- renderText(eval_data_csv())

output$eval_data_gis_text <- renderText({
  req(vals$eval.data.gis.info)
  req(vals$eval.data.gis.info[[3]] == input$eval_load_type)

  eval_data_gis()
})

output$eval_metrics_text <- renderText(eval_metrics())

# Validatoin data title
output$table_eval_pts_title <- renderText({
  req(vals$eval.data.specs)
  ifelse(vals$eval.data.specs[2] == 1,
         "Validation data (count) info",
         "Validation data (presence/absence) info")
})

# Validation data table
output$table_eval_pts_out <- renderTable({
  table_eval_pts()
}, colnames = FALSE)

###########################################################
### Metrics table
output$table_eval_metrics_out <- renderTable({
  table_eval_metrics()
}, rownames = FALSE, digits = 3)


###############################################################################
##### High Quality Maps #####

###########################################################
# Tables

### Table of orig model predictions
output$pretty_table_orig_out <- DT::renderDataTable({
  table_orig()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'))

### Table of overlaid model predictions
output$pretty_table_over_out <- DT::renderDataTable({
  table_overlaid()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'))

### Table of ensemble model predictions
output$pretty_table_ens_out <- DT::renderDataTable({
  table_ensembles()
}, options = list(dom = 't'))

###########################################################
# Outputs

### Color wheel for preview of color palette
output$pretty_plot_color_preview_plot <- renderPlot({
  pretty_plot_color_preview()
})

### Pretty plot error output
output$pretty_plot_values_event_text <- renderText({
  pretty_plot_values_event()
})

### Pretty plot
# 'suspendWhenHidden = FALSE' in server_hide+show.R
# output$pretty_plot_plot <- renderPlot({
#   print(pretty_plot_generate())
# })


###############################################################################
##### Export Model Predictions #####

###########################################################
# Tables

### Table of orig model predictions
output$export_table_orig_out <- DT::renderDataTable({
  table_orig()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'), selection = "single")

### Table of overlaid model predictions
output$export_table_over_out <- DT::renderDataTable({
  table_overlaid()[, 1:4][, -3] #'[, -3]' is to remove Error column
}, options = list(dom = 't'), selection = "single")

### Table of ensemble models
output$export_table_ens_out <- DT::renderDataTable({
  table_ensembles()
}, options = list(dom = 't'), selection = "single")


###############################################################################
# ##### Submit Feedback #####
# ### Feedback function text
# output$feedback_submit_text <- renderText({
#   feedback_submit()
# })
#
# ### Warning message if no internet connection is detected
# output$feedback_internet_connection_text <- renderText({
#   feedback_internet_connection()
# })
###############################################################################
