### Render various outputs for Ensemble App


###############################################################################
### Reactive functions that return tables are in server_render_tables.R


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
}, options = list(dom = 't'), selection = "multiple")

### Table of stats of loaded original model preds
output$models_loaded_table_stats <- renderDT({
  table_orig_stats()
}, options = list(dom = 't'), selection = "none")

#----------------------------------------------------------
### Plot/preview of loaded, original model(s)
output$model_preview_interactive_plot <- renderLeaflet({
  req(x <- vals$models.plot.leaf)

  eSDM::preview_interactive(
    x$model.toplot, "Pred", x$perc.num, pal.esdm, leg.perc.esdm,
    x$plot.title, x$leg.title
  )
})

output$model_preview_plot <- renderPlot({
  req(x <- vals$models.plot)

  eSDM::multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num, pal.esdm,
    leg.perc.esdm,
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
}, options = list(dom = 't'), selection = "single")

### Table of stats of loaded model predictions
output$overlay_loaded_stats_table <- renderDT({
  table_orig_stats()
}, options = list(dom = 't'), selection = "none")

#----------------------------------------------------------
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

#----------------------------------------------------------
# Overlaying process outputs

### Error output for overlay process
output$overlay_overlay_all_text <- renderText({
  overlay_all()
})

### Message detailing that overlaid models are created
output$overlay_overlaid_models_message <- renderText({
  if (length(vals$overlaid.models) > 0) {
    paste(
      "Overlaid models have been created using the geometry of the",
      paste0("'", vals$models.names[vals$overlay.info[[1]]], "'"),
      "SDM as the base grid and with a percent overlap of",
      paste0(vals$overlay.info[[2]] * 100, "%")
    )
  }
})


#----------------------------------------------------------
# Previews

### Preview of base grid
# 'suspendWhenHidden = FALSE' in server_hide+show.R
output$overlay_preview_base <- renderLeaflet({
  req(vals$overlay.plot)
})

### Preview of overlaid model predictions
# 'suspendWhenHidden = FALSE' in server_hide+show.R
output$overlay_preview_overlaid <- renderPlot({
  req(x <- vals$overlaid.plot)

  eSDM::multiplot_layout(
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
}, options = list(dom = 't'))

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
options = list(dom = 't'))

### Remove ensemble error output
output$ens_remove_text <- renderUI({
  HTML(ens_remove())
})

### Plot interactive preview of ensemble predictions
output$ens_preview_interactive_plot <- renderLeaflet({
  req(x <- vals$ensemble.plot.leaf)

  eSDM::preview_interactive(
    x$model.toplot, "Pred.ens", x$perc.num, pal.esdm, leg.perc.esdm,
    x$plot.title, x$leg.title
  )
})

### Plot preview of ensemble predictions
output$ens_preview_plot <- renderPlot({
  req(x <- vals$ensemble.plot)

  eSDM::multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num, pal.esdm,
    leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6]
  )
})

### Table of abundances of created ensemble predictions
output$ens_abund_table_out <- renderTable({
  table_ens_abund()
}, rownames = FALSE, align = "r") #, rownames = TRUE, colnames = FALSE)


###############################################################################
##### Model Evaluation Metrics tab #####

#----------------------------------------------------------
# Tables

### Table of orig model predictions
output$eval_models_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = list(dom = 't'))

### Table of overlaid model predictions
output$eval_models_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = list(dom = 't'))

### Table of ensemble models
output$eval_models_table_ens_out <- renderDT({
  table_ensembles()
}, options = list(dom = 't'))

#----------------------------------------------------------
# Presence/absence loaded message, error outputs, and table

# Presence and absence points
output$eval_data_message <- renderText({
  ifelse(inherits(vals$eval.data, "sf"), "Validation data loaded", "")
})

# Text outputs
output$eval_csv_data_text <- renderText(eval_data_csv())

output$eval_data_gis_text <- renderText({
  req(vals$eval.data.gis.info[[3]] == input$eval_load_type)

  eval_data_gis()
})

output$eval_metrics_text <- renderText(eval_metrics())

# Validation data title
output$table_eval_pts_title <- renderText({
  req(ifelse(
    vals$eval.data.specs[2] == 1, "Validation data (count) info",
    "Validation data (presence/absence) info"
  ))
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
}, options = list(dom = 't'))

### Table of overlaid model predictions
output$pretty_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = list(dom = 't'))

### Table of ensemble model predictions
output$pretty_table_ens_out <- renderDT({
  table_ensembles()
}, options = list(dom = 't'))

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

### Pretty plot error output
output$pretty_plot_values_event_text <- renderText({
  pretty_plot_values_event()
})

### Pretty plot
# observe({
#   output$pretty_plot_plot <- renderPlot({
#     pretty_plot_generate()
#   }, height = 500, width = pretty_plot_plot_width())
# })
output$pretty_plot_plot <- renderPlot({
  req(p.list <- vals$pretty.params.list)
  plot_pretty(
    p.list$model.toplot, p.list$data.name, p.list$plot.lim,
    p.list$title.ll, p.list$lab.x, p.list$lab.y,
    p.list$title.cex, p.list$lab.cex, p.list$axis.cex, p.list$axis.tcl,
    p.list$list.background, p.list$list.colorscheme, p.list$list.addobj
  )
}, height = 500)

# pretty_plot_plot_width <- reactive({
#   if (isTruthy(vals$pretty.params.list)) {
#     xy.ratio <- unname(
#       diff(vals$pretty.params.list$plot.lim[1:2]) /
#         diff(vals$pretty.params.list$plot.lim[3:4])
#     )
#     min(500, 500 * xy.ratio)
#
#   } else {
#     500
#   }
# })


###############################################################################
##### Export Model Predictions #####

#----------------------------------------------------------
# Tables

### Table of orig model predictions
output$export_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = list(dom = 't'), selection = "single")

### Table of overlaid model predictions
output$export_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = list(dom = 't'), selection = "single")

### Table of ensemble models
output$export_table_ens_out <- renderDT({
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
