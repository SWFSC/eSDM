### Render various outputs for Ensemble App


###############################################################################
### Reactive functions that return tables are in server_render_tables.R
dt.list <- list(dom = 't', pageLength = 50)

###############################################################################
##### Load Models tab #####

#----------------------------------------------------------
### Load saved environment output
output$load_envir_text <- renderText({
  load_envir()
})

#----------------------------------------------------------
# Created model predictions messages

### Created model predictions message for csv
output$create_sf_csv_text <- renderText({
  req(read_model_csv())
  create_sf_csv()
})

### Created model predictions message for gis raster
output$create_sf_gis_raster_text <- renderText({
  req(read_model_gis_raster())
  create_sf_gis_raster()
})

### Created model predictions message for gis shp
output$create_sf_gis_shp_text <- renderText({
  req(read_model_gis_shp())
  create_sf_gis_shp()
})

### Created model predictions message for gis gdb
output$create_sf_gis_gdb_text <- renderText({
  req(read_model_gis_gdb())
  create_sf_gis_gdb()
})

#----------------------------------------------------------
# Tables

### Table of loaded original model preds
output$models_loaded_table <- renderDT({
  table_orig()
}, options = dt.list, selection = "multiple")

### Table of stats of loaded original model preds
output$models_loaded_table_stats <- renderDT({
  table_orig_stats()
}, options = dt.list, selection = "none")

#----------------------------------------------------------
### Plot/preview of loaded, original model(s)
output$model_preview_interactive_plot <- renderLeaflet({
  req(x <- vals$models.plot.leaf)

  preview_interactive(
    x$model.toplot, "Pred", x$perc.num, pal.esdm, leg.perc.esdm,
    x$plot.title, x$leg.title
  )
})

output$model_preview_plot <- renderPlot({
  req(x <- vals$models.plot)

  multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num,
    x$pal, leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6]
  )
})


###############################################################################
##### Overlay tab #####

#----------------------------------------------------------
# Tables

### Table of loaded model predictions
output$overlay_loaded_table <- renderDT({
  table_orig()
}, options = dt.list, selection = "single")

### Table of stats of loaded model predictions
output$overlay_loaded_stats_table <- renderDT({
  table_orig_stats()
}, options = dt.list, selection = "none")

#----------------------------------------------------------
# Polygon error outputs and loaded messages

### Boundary (study area) polygon error outputs
output$overlay_bound_csv_text     <- renderText(overlay_bound_csv())
output$overlay_bound_gis_shp_text <- renderText(overlay_bound_gis_shp())
output$overlay_bound_gis_gdb_text <- renderText(overlay_bound_gis_gdb())

### Boundary polygon loaded messages
output$overlay_bound_csv_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon is loaded"
})
output$overlay_bound_gis_shp_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon is loaded"
})
output$overlay_bound_gis_gdb_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon is loaded"
})

### Land polygon error outputs
output$overlay_land_prov_text    <- renderText(overlay_land_prov())
output$overlay_land_csv_text     <- renderText(overlay_land_csv())
output$overlay_land_gis_shp_text <- renderText(overlay_land_gis_shp())
output$overlay_land_gis_gdb_text <- renderText(overlay_land_gis_gdb())

### Land polygon loaded messages
output$overlay_land_prov_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is loaded"
})
output$overlay_land_csv_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is loaded"
})
output$overlay_land_gis_shp_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is loaded"
})
output$overlay_land_gis_gdb_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is loaded"
})

#----------------------------------------------------------
# Overlaying process outputs

### Error output for overlay process
output$overlay_overlay_all_text <- renderText({
  overlay_all()
})

### Message detailing that overlaid models are created
output$overlay_overlaid_models_message <- renderUI({
  req(length(vals$overlaid.models) > 0)

  HTML(paste0(
    "Overlaid models have been created using the following overlay options:",
    tags$br(),
    paste("1) Using the geometry of the",
          paste0("'", vals$models.names[vals$overlay.info[[1]]], "'"),
          "SDM as the base grid"),
    tags$br(),
    paste("2)", vals$overlay.info[[2]]) ,
    tags$br(),
    paste("3)", vals$overlay.info[[3]]),
    tags$br(),
    paste("4) With a percent overlap of",
          paste0(vals$overlay.info[[4]] * 100, "%"))
  ))
})


output$overlay_preview_base_create_text <- renderText({
  overlay_preview_base_create()
})

#----------------------------------------------------------
# Previews

### Preview of base grid
output$overlay_preview_base <- renderLeaflet({
  req(vals$overlay.plot)
})

### Preview of overlaid model predictions
output$overlay_preview_overlaid <- renderPlot({
  req(x <- vals$overlaid.plot)

  multiplot_layout(
    x$models.toplot, x$data.names, x$plot.titles, 1, pal.esdm, leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6]
  )
})


###############################################################################
##### Create Ensembles tab #####

#----------------------------------------------------------
# Tables

### Display table of overlaid predictions and info
output$create_ens_table <- renderTable({
  table_overlaid()
}, rownames = TRUE)

### Datatable of overlaid predictions and info
output$create_ens_datatable <- renderDT({
  table_overlaid()
}, options = dt.list)

#----------------------------------------------------------
# Weights outputs

### Table of metric values to be used as weights
output$create_ens_weights_metric_table_out <- renderTable({
  create_ens_weights_metric_table()
}, rownames = FALSE, digits = 3)

### Table of if overlaid models have spatial pixel weights
output$create_ens_weights_pix_table_out <- renderTable({
  create_ens_weights_pix_table()
}, rownames = FALSE, align = "lcc")

### Table summarizing overlaid models and their polygon weights
output$create_ens_weights_poly_table_out <- renderTable({
  create_ens_weights_poly_table()
}, rownames = FALSE)

### Preview plot of weight polygons
output$create_ens_weights_poly_preview_plot <- renderPlot({
  req(x <- vals$ens.over.wpoly.plot)

  plot(x[[1]], axes = TRUE, col = "black", border = NA)
  for(sf.toplot in vals$ens.over.wpoly.sf[[x[[2]]]]) {
    plot(st_geometry(sf.toplot), add = TRUE, col = NA, border = "red")
  }
})


### Text output for removing loaded weight polygons
output$create_ens_weights_poly_remove_text <- renderText({
  create_ens_weights_poly_remove()
})

### Output for adding polygon weight(s) to reactiveValues
output$create_ens_weights_poly_add_text <- renderText({
  create_ens_weights_poly_add()
})

#----------------------------------------------------------
### Create ensemble error/completion output
output$ens_create_ensemble_text <- renderUI({
  HTML(create_ensemble())
})

#----------------------------------------------------------
# Created ensemble things

### Table of created ensemble predictions
output$ens_datatable_ensembles <- renderDT({
  table_ensembles()
},
options = dt.list)

### Remove ensemble error output
output$ens_remove_text <- renderUI({
  HTML(ens_remove())
})

### Plot interactive preview of ensemble predictions
output$ens_preview_interactive_plot <- renderLeaflet({
  req(x <- vals$ensemble.plot.leaf)

  preview_interactive(
    x$model.toplot, "Pred.ens", x$perc.num, pal.esdm, leg.perc.esdm,
    x$plot.title, x$leg.title
  )
})

### Plot preview of ensemble predictions
output$ens_preview_plot <- renderPlot({
  req(x <- vals$ensemble.plot)

  multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num,
    x$pal, leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6]
  )
})

### Table of abundances of created ensemble predictions
output$ens_abund_table_out <- renderTable({
  req(abund_reac_flag())
  table_ens_abund()
}, rownames = FALSE, align = "r")


###############################################################################
##### Model Evaluation Metrics tab #####

#----------------------------------------------------------
# Tables

### Table of orig model predictions
output$eval_models_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list)

### Table of overlaid model predictions
output$eval_models_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = dt.list)

### Table of ensemble models
output$eval_models_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list)

#----------------------------------------------------------
# Validation data messages, error outputs, and table

# Validation data loaded message
output$eval_data_message <- renderText({
  req(vals$eval.data)
  paste(
    "Validation data loaded; data type:",
    ifelse(vals$eval.data.specs[2] == 1, "'count'", "'presence/absence'")
  )
})

# Text (error) outputs
output$eval_csv_data_text <- renderText(eval_data_csv())

output$eval_data_gis_text <- renderText({
  req(vals$eval.data.gis.info[[3]] == input$eval_load_type)

  eval_data_gis()
})

output$eval_metrics_text <- renderText(eval_metrics())

output$eval_metrics_message <- renderText({
  req(vals$eval.metrics)
  "Metrics calculated"
})

# Validation data info title
output$table_eval_pts_title <- renderText({
  req(vals$eval.data)
  ifelse(
    vals$eval.data.specs[2] == 1, "Validation data (count) info",
    "Validation data (presence/absence) info"
  )
})

# Validation data table
output$table_eval_pts_out <- renderTable({
  table_eval_pts()
}, colnames = FALSE)

#----------------------------------------------------------
### Metrics table
output$table_eval_metrics_out <- renderTable({
  table_eval_metrics()
}, rownames = FALSE, digits = 3)


output$eval_metrics_overlap_text <- renderText({
  req(table_eval_metrics())
  eval_metrics_overlap()
})

###############################################################################
##### High Quality Maps #####

#----------------------------------------------------------
# Tables

### Table of orig model predictions
output$pretty_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list, selection = "single")

### Table of overlaid model predictions
output$pretty_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = dt.list, selection = "single")

### Table of ensemble model predictions
output$pretty_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

#----------------------------------------------------------
# Outputs

### Color wheel for preview of color palette
output$pretty_plot_color_preview_plot <- renderPlot({
  pretty_plot_color_preview()
})

### Table of 'added' additional polys
output$pretty_plot_addobj_table_out <- renderTable({
  pretty_plot_addobj_table()
})

### Pretty plot add error output
output$pretty_plot_toplot_add_text <- renderText({
  pretty_plot_toplot_add()
})
output$pretty_plot_toplot_remove_text <- renderText({
  pretty_plot_toplot_remove()
})

### Pretty plot plot error output
output$pretty_plot_plot_text <- renderText({
  pretty_plot_plot()
})

### Pretty plot table
output$pretty_plot_toplot_table_out <- renderDT({
  pretty_plot_toplot_table()
}, options = dt.list, rownames = FALSE)

### Pretty plot
output$pretty_plot_plot_out <- renderPlot({
  req(p.list <- vals$pretty.plot.list)
  plot_pretty_top(p.list$dims, p.list$idx.list, p.list$params.list)
})


###############################################################################
##### Export Model Predictions #####

### Table of orig model predictions
output$export_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list, selection = "single")

### Table of overlaid model predictions
output$export_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = dt.list, selection = "single")

### Table of ensemble models
output$export_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

###############################################################################
