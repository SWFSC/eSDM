### Render various non-UI outputs for GUI


###############################################################################
###############################################################################
### Reactive functions that return tables are in server_render_tables.R
dt.list <- list(dom = 't', pageLength = 50)

###############################################################################
###############################################################################
##### Import Predictions tab #####

#----------------------------------------------------------
### Load saved workspace output
output$load_envir_text <- renderText(load_envir())

#----------------------------------------------------------
# Imported predictions messages

### Imported predictions message for csv
output$create_sf_csv_text <- renderText({
  req(read_model_csv())
  create_sf_csv()
})

### Imported predictions message for raster
output$create_sf_gis_raster_text <- renderText({
  req(read_model_gis_raster())
  create_sf_gis_raster()
})

### Imported predictions message for shp
output$create_sf_gis_shp_text <- renderText({
  req(read_model_gis_shp())
  create_sf_gis_shp()
})

### Imported predictions message for gdb
output$create_sf_gis_gdb_text <- renderText({
  req(read_model_gis_gdb())
  create_sf_gis_gdb()
})

#----------------------------------------------------------
# Tables

### Table of imported original predictions
output$models_loaded_table <- renderDT({
  table_orig()
}, options = dt.list, selection = "multiple")

### Table of stats of imported original predictions
output$models_loaded_table_stats <- renderDT({
  table_orig_stats()
}, options = dt.list, selection = "none")

#----------------------------------------------------------
# Remove imported predictions output
output$model_remove_text <- renderText(model_remove())

#----------------------------------------------------------
### Plot/preview of imported original predictions
output$model_preview_interactive_plot <- renderLeaflet({
  x <- req(vals$models.plot.leaf)

  preview_interactive(
    x$model.toplot, x$data.names, x$plot.title, x$perc.num, x$pal,
    leg.perc.esdm, x$leg.title
  )
})

output$model_preview_plot <- renderPlot({
  x <- req(vals$models.plot)

  multiplot_layout(
    x$models.toplot, x$data.names, x$plot.titles, x$perc.num,
    x$pal, leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6], x$plot.dims[7:10],
    x$var.key
  )
})


###############################################################################
###############################################################################
##### Overlay Predictions tab #####

#----------------------------------------------------------
# Tables

### Table of imported original predictions
output$overlay_loaded_table <- renderDT({
  validate(
    need(table_orig(), "Import predictions to select a base geometry"),
    errorClass = "validation2"
  )
  table_orig()
}, options = dt.list, selection = "single")

### Table of stats of imported original predictions
output$overlay_loaded_stats_table <- renderDT({
  table_orig_stats()
}, options = dt.list, selection = "none")

#----------------------------------------------------------
# Polygon error outputs and imported messages

### Study area (boundary) polygon error outputs
output$overlay_bound_csv_text     <- renderText(overlay_bound_csv())
output$overlay_bound_gis_shp_text <- renderText(overlay_bound_gis_shp())
output$overlay_bound_gis_gdb_text <- renderText(overlay_bound_gis_gdb())

### Study area polygon imported message
# h5() inside renderUI() makes for more whitespace than renderText() in span()
output$overlay_bound_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon has been imported"
})

### Erasing (land) polygon error outputs
output$overlay_land_prov_text    <- renderText(overlay_land_prov())
output$overlay_land_csv_text     <- renderText(overlay_land_csv())
output$overlay_land_gis_shp_text <- renderText(overlay_land_gis_shp())
output$overlay_land_gis_gdb_text <- renderText(overlay_land_gis_gdb())

### Erasing polygon imported message
# h5() inside renderUI() makes for more whitespace than renderText() in span()
output$overlay_land_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon has been imported"
})

#----------------------------------------------------------
# Overlay process outputs

### Error output for overlay process
output$overlay_overlay_all_text <- renderText(overlay_all())

### Overlaid predictions info message
output$overlay_overlaid_models_message <- renderUI({
  req(length(vals$overlaid.models) > 0)

  HTML(paste0(
    "Overlaid predictions have been created using the following overlay options:",
    tags$br(),
    paste("1)", vals$overlay.info[[2]]) ,
    tags$br(),
    paste("2) Using the geometry of the",
          paste0("'", vals$overlay.info[[1]], "'"),
          "SDM as the base geometry"),
    tags$br(),
    paste("3)", vals$overlay.info[[3]]),
    tags$br(),
    paste("4) With a percent overlap threshold of",
          paste0(vals$overlay.info[[4]], "%"))
  ))
})

### Error output for creating base geometry preview
output$overlay_preview_base_create_text <- renderText({
  overlay_preview_base_create()
})

#----------------------------------------------------------
# Previews

### Base geometry preview
output$overlay_preview_base <- renderLeaflet(req(vals$overlay.plot))

### Overlaid predictions preview
output$overlay_preview_overlaid <- renderPlot({
  x <- req(vals$overlaid.plot)

  multiplot_layout(
    x$models.toplot, x$data.names, x$plot.titles, x$perc.num, x$pal,
    leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6], x$plot.dims[7:10]
  )
})


###############################################################################
###############################################################################
##### Create Ensemble Predictions tab #####

#----------------------------------------------------------
# Message about base geometry
output$create_ens_base_message <- renderText({
  req(length(vals$overlaid.models) > 0)

  paste(
    "Overlaid predictions were created using the geometry of the",
    paste0("'", vals$overlay.info[[1]], "'"), "SDM as the base geometry.",
    "See the 'Overlay Predictions' tab for other overlay information."
  )
})

#----------------------------------------------------------
# Tables

### Display table of overlaid predictions and info
output$create_ens_table <- renderTable(table_overlaid(), rownames = TRUE)

### Datatable of overlaid predictions and info
output$create_ens_datatable <- renderDT(table_overlaid(), options = dt.list)


#----------------------------------------------------------
# Exclusion polygon outputs
### Table summarizing overlaid preds and their exclusion polygons
output$create_ens_reg_table_out <- renderTable({
  create_ens_reg_table()
}, rownames = FALSE)

### Preview plot of exclusion polygons
output$create_ens_reg_preview_plot <- renderPlot({
  x <- req(vals$ens.over.wpoly.plot)

  plot(x[[1]], axes = TRUE, col = "black", border = NA,
       main = paste("Overlaid", x[[2]]))
  for(sf.toplot in vals$ens.over.wpoly.sf[[x[[2]]]]) {
    plot(st_geometry(sf.toplot), add = TRUE, col = NA, border = "red")
  }
})


### Text output for removing imported exclusion polygon(s)
output$create_ens_reg_remove_text <- renderText(create_ens_reg_remove())

### Output for adding exclusion polygon(s) to reactiveValues
output$create_ens_reg_add_text <- renderText(create_ens_reg_add())


#----------------------------------------------------------
# Weights outputs (tables funcs in server_3_createEns_create_weighted.R)

### Table of metric values to be used as weights
output$create_ens_weights_metric_table_out <- renderTable({
  create_ens_weights_metric_table()
}, rownames = FALSE, digits = 3)

### Table of if overlaid preds have spatial pixel weights
output$create_ens_weights_pix_table_out <- renderTable({
  create_ens_weights_pix_table()
}, rownames = FALSE, align = "lccccc", digits = -2)

### Table of if overlaid preds have associated uncertainty values
output$create_ens_weights_var_table_out <- renderTable({
  create_ens_weights_var_table()
}, rownames = FALSE, align = "lccccc", digits = -2)


#----------------------------------------------------------
### Create ensemble error/completion output
output$ens_create_ensemble_text <- renderUI(HTML(create_ensemble()))

#----------------------------------------------------------
# Created ensemble things

### Table of created ensemble predictions
output$ens_datatable_ensembles <- renderDT(
  table_ensembles(), selection = "multiple", options = dt.list
)

### Stats table of created ensemble predictions
output$ens_datatable_ensembles_stats <- renderDT(
  table_ensembles_stats(), selection = "none", options = dt.list
)

#----------------------------------------------------------
# Created ensemble things options

### Remove ensemble predictions output
output$ens_remove_text <- renderText(ens_remove())

### Plot interactive preview of ensemble predictions
output$ens_preview_interactive_plot <- renderLeaflet({
  x <- req(vals$ensemble.plot.leaf)

  preview_interactive(
    x$model.toplot, x$data.names, x$plot.title, x$perc.num, x$pal,
    leg.perc.esdm, x$leg.title
  )
})

### Plot preview of ensemble predictions
output$ens_preview_plot <- renderPlot({
  x <- req(vals$ensemble.plot)

  multiplot_layout(
    x$models.toplot, x$data.names, x$plot.titles, x$perc.num, x$pal,
    leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6], x$plot.dims[7:10],
    x$var.key
  )
})

### Table of abundances of created ensemble predictions
output$ens_abund_table_out <- renderTable({
  req(abund_reac_flag())
  table_ens_abund()
}, rownames = FALSE, align = "r")


###############################################################################
###############################################################################
##### Evaluation Metrics tab #####

#----------------------------------------------------------
# Tables

### Table of original predictions; [, 1:4] removes pred type
output$eval_models_table_orig_out <- renderDT({
  table_orig()[, 1:4]
}, options = dt.list)

### Table of overlaid predictions; [, 1:4] removes pred type
output$eval_models_table_over_out <- renderDT({
  table_overlaid()[, 1:4]
}, options = dt.list)

### Table of ensemble predictions
output$eval_models_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list)

#----------------------------------------------------------
# Validation data messages, error outputs, and table

# Validation data loaded message
output$eval_data_message <- renderText({
  req(vals$eval.data)
  "Validation data successfully imported"
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

# Validation data filename and table
# Filename separate in case filename is extra long
output$table_eval_pts_filename_out <- renderText(table_eval_pts_filename())
output$table_eval_pts_out <- renderTable(table_eval_pts(), colnames = FALSE)

#----------------------------------------------------------
### Metrics table
output$table_eval_metrics_out <- renderTable({
  table_eval_metrics()
}, rownames = FALSE, digits = 3)


output$eval_metrics_overlap_text <- renderText({
  req(vals$eval.metrics)[[2]]
})


###############################################################################
###############################################################################
##### High Quality Maps #####

#----------------------------------------------------------
# Tables

### Table of original predictions; [, 1:4] removes pred type
output$pretty_table_orig_out <- renderDT({
  table_orig()[, 1:4]
}, options = dt.list, selection = "single")

### Table of overlaid predictions; [, 1:4] removes pred type
output$pretty_table_over_out <- renderDT({
  table_overlaid()[, 1:4]
}, options = dt.list, selection = "single")

### Table of ensemble predictions
output$pretty_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

#----------------------------------------------------------
# Map control outputs

### Pretty plot manage to-plot
# Add map output
output$pretty_toplot_add_text <- renderText(pretty_toplot_add())
# Remove map output
output$pretty_toplot_remove_text <- renderText(pretty_toplot_remove())

### Pretty plot update
# Table
output$pretty_update_table_out <- renderDT({
  pretty_toplot_table()
}, options = dt.list, rownames = FALSE, selection = "single")

### Pretty plot plot/download
# Table
output$pretty_toplot_table_out <- renderDT({
  pretty_toplot_table()
}, options = dt.list, rownames = FALSE)

# Error output
output$pretty_plot_text <- renderText(pretty_plot())

#----------------------------------------------------------
# Coordinate grid marks and labels
### Message when ticks and labels inside frame are specified
output$pretty_tick_label_message <- renderText({
  req(input$pretty_tick, input$pretty_tick_which,
      input$pretty_tick_label_inout)

  req((2 %in% input$pretty_tick_which) & (input$pretty_tick_label_inout == 1))

  "Tick marks will not be plotted when labels are inside the frame"
})

#----------------------------------------------------------
# Additional object section
### Text output for adding additional object
output$pretty_addobj_add_out <- renderText(pretty_addobj_add())

### Text output for removing additional objects
output$pretty_addobj_remove_out <- renderText(pretty_addobj_remove())

### Table of 'added' additional objects
output$pretty_addobj_table_out <- renderDT({
  pretty_addobj_table()
}, options = dt.list, selection = "single", rownames = FALSE)

#----------------------------------------------------------
# Other outputs

### Text for update range values button
output$pretty_range_lastmap_text <- renderText(pretty_range_lastmap_execute())

### Color wheel for preview of color palette
output$pretty_color_preview_plot <- renderPlot(pretty_color_preview())


#----------------------------------------------------------
# Update section
output$pretty_update_message <- renderText({
  d <- req(val.pretty.update.mess())
  d.id <- paste0("'", d[[2]], "'")

  paste(
    ifelse(d[[1]] == 1, "Update cancelled;", "Update successful;"),
    "changes to parameters of map", d.id,
    ifelse(d[[1]] == 1, "were discarded", "were saved")
  )
})

#----------------------------------------------------------
# Pretty plot, plotting

### Display box, render-ed each time so that box can be resized
output$pretty_display <- renderUI({
  box(
    title = "High Quality Maps", solidHeader = TRUE, status = "primary", width = 12, align = "center",
    height = ifelse(isTruthy(vals$pretty.plot), vals$pretty.plot$dims["height"], 4 * 96) + 60,
    # shinycssloaders::withSpinner(plotOutput("pretty_plot_out"), type = 1)
    plotOutput("pretty_plot_out")
  )
})

### Section with button to save map; spinning wheel displayed during param prep
# All needs to be within box() so single 'object' is returned
output$pretty_save_map <- renderUI({
  # Want 'Map ID default to display if no predictions are selected
  req(pretty_models_idx_count() <= 1)

  # 0-360 maps take a while to render map range widgets
  # This makes the wheel spin during ^ when changing btw predictions,
  #   but not when changing range with same predictions selected
  if (pretty_models_idx_count() == 1) isolate(pretty_range())

  # UI code
  box(
    width = 12,
    uiOutput("pretty_toplot_add_id_uiOut_text"),
    helpText("Note that most plot parameters below (including loaded additional objects)",
             "will stay the same unless changed by user, even when a different set of predictions is selected.",
             "Thus, be sure to check the parameters before saving a new map"),
    tags$br(),
    uiOutput("pretty_toplot_se_uiOut_check"),
    uiOutput("pretty_toplot_se_uiOut_text"),
    actionButton("pretty_toplot_add_execute", "Save map"),
    tags$br(), tags$br(),
    tags$span(textOutput("pretty_toplot_add_text"), style = "color: blue;")
  )
})


### Pretty plot dimension warnings
output$pretty_plot_dim_warnings_out <- renderText(pretty_plot_dim_warnings())

### Pretty plot; observe() allows for calling vals$
observe({
  output$pretty_plot_out <- renderPlot({
    p.list <- vals$pretty.plot
    validate(
      need(p.list, "High quality map(s) will be displayed here"),
      errorClass = "validation3"
    )

    plot_pretty_top(p.list$dims, p.list$idx.list, p.list$params.list)
  },
  width = ifelse(isTruthy(vals$pretty.plot), vals$pretty.plot$dims["width"], 8 * 96),
  height = ifelse(isTruthy(vals$pretty.plot), vals$pretty.plot$dims["height"], 4 * 96),
  res = 98)
})


###############################################################################
###############################################################################
##### Export Predictions #####

### Table of original predictions; [, 1:4] removes pred type
output$export_table_orig_out <- renderDT({
  table_orig()[, 1:4]
}, options = dt.list, selection = "single")

### Table of overlaid predictions; [, 1:4] removes pred type
output$export_table_over_out <- renderDT({
  table_overlaid()[, 1:4]
}, options = dt.list, selection = "single")

### Table of ensemble predictions
output$export_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

###############################################################################
###############################################################################
