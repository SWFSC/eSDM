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
# Created predictions messages

### Created predictions message for csv
output$create_sf_csv_text <- renderText({
  req(read_model_csv())
  create_sf_csv()
})

### Created predictions message for gis raster
output$create_sf_gis_raster_text <- renderText({
  req(read_model_gis_raster())
  create_sf_gis_raster()
})

### Created predictions message for gis shp
output$create_sf_gis_shp_text <- renderText({
  req(read_model_gis_shp())
  create_sf_gis_shp()
})

### Created predictions message for gis gdb
output$create_sf_gis_gdb_text <- renderText({
  req(read_model_gis_gdb())
  create_sf_gis_gdb()
})

#----------------------------------------------------------
# Tables

### Table of loaded original preds
output$models_loaded_table <- renderDT({
  table_orig()
}, options = dt.list, selection = "multiple")

### Table of stats of loaded original preds
output$models_loaded_table_stats <- renderDT({
  table_orig_stats()
}, options = dt.list, selection = "none")

#----------------------------------------------------------
# Remove loaded models
output$model_remove_text <- renderText(model_remove())

#----------------------------------------------------------
### Plot/preview of loaded, original model(s)
output$model_preview_interactive_plot <- renderLeaflet({
  x <- req(vals$models.plot.leaf)

  preview_interactive(
    x$model.toplot, "Pred", x$plot.title, x$perc.num, x$pal, leg.perc.esdm,
    x$leg.title
  )
})

output$model_preview_plot <- renderPlot({
  x <- req(vals$models.plot)

  multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num,
    x$pal, leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6], x$plot.dims[7:10]
  )
})


###############################################################################
##### Overlay tab #####

#----------------------------------------------------------
# Tables

### Table of loaded predictions
output$overlay_loaded_table <- renderDT({
  table_orig()
}, options = dt.list, selection = "single")

### Table of stats of loaded predictions
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
  "A study area polygon is imported"
})
output$overlay_bound_gis_shp_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon is imported"
})
output$overlay_bound_gis_gdb_message <- renderText({
  req(vals$overlay.bound)
  "A study area polygon is imported"
})

### Erasing polygon error outputs
output$overlay_land_prov_text    <- renderText(overlay_land_prov())
output$overlay_land_csv_text     <- renderText(overlay_land_csv())
output$overlay_land_gis_shp_text <- renderText(overlay_land_gis_shp())
output$overlay_land_gis_gdb_text <- renderText(overlay_land_gis_gdb())

### Erasing polygon loaded messages
output$overlay_land_prov_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is imported"
})
output$overlay_land_csv_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is imported"
})
output$overlay_land_gis_shp_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is imported"
})
output$overlay_land_gis_gdb_message <- renderText({
  req(vals$overlay.land)
  "An erasing polygon is imported"
})

#----------------------------------------------------------
# Overlaying process outputs

### Error output for overlay process
output$overlay_overlay_all_text <- renderText({
  overlay_all()
})

### Message detailing that overlaid predictions have been created
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
          paste0(vals$overlay.info[[4]] * 100, "%"))
  ))
})

### Error output for just base - used if previewing base
output$overlay_preview_base_create_text <- renderText({
  overlay_preview_base_create()
})

#----------------------------------------------------------
# Previews

### Preview of base grid
output$overlay_preview_base <- renderLeaflet({
  req(vals$overlay.plot)
})

### Preview of overlaid predictions
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
##### Create Ensembles tab #####

#----------------------------------------------------------
# Message about base grid
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

### Table of if overlaid preds have spatial pixel weights
output$create_ens_weights_pix_table_out <- renderTable({
  create_ens_weights_pix_table()
}, rownames = FALSE, align = "lcc")

### Table summarizing overlaid preds and their polygon weights
output$create_ens_reg_table_out <- renderTable({
  create_ens_reg_table()
}, rownames = FALSE)

### Preview plot of weight polygons
output$create_ens_reg_preview_plot <- renderPlot({
  x <- req(vals$ens.over.wpoly.plot)

  plot(x[[1]], axes = TRUE, col = "black", border = NA)
  for(sf.toplot in vals$ens.over.wpoly.sf[[x[[2]]]]) {
    plot(st_geometry(sf.toplot), add = TRUE, col = NA, border = "red")
  }
})


### Text output for removing loaded weight polygons
output$create_ens_reg_remove_text <- renderText({
  create_ens_reg_remove()
})

### Output for adding polygon weight(s) to reactiveValues
output$create_ens_reg_add_text <- renderText({
  create_ens_reg_add()
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
output$ens_remove_text <- renderText(ens_remove())

### Plot interactive preview of ensemble predictions
output$ens_preview_interactive_plot <- renderLeaflet({
  x <- req(vals$ensemble.plot.leaf)

  preview_interactive(
    x$model.toplot, "Pred.ens", x$plot.title, x$perc.num, x$pal, leg.perc.esdm,
    x$leg.title
  )
})

### Plot preview of ensemble predictions
output$ens_preview_plot <- renderPlot({
  x <- req(vals$ensemble.plot)

  multiplot_layout(
    x$models.toplot, x$data.name, x$plot.titles, x$perc.num, x$pal,
    leg.perc.esdm,
    x$plot.dims[1], x$plot.dims[2], x$plot.dims[3], x$plot.dims[4],
    x$plot.dims[5], x$plot.dims[6], x$plot.dims[7:10]
  )
})

### Table of abundances of created ensemble predictions
output$ens_abund_table_out <- renderTable({
  req(abund_reac_flag())
  table_ens_abund()
}, rownames = FALSE, align = "r")


###############################################################################
##### Evaluation Metrics tab #####

#----------------------------------------------------------
# Tables

### Table of orig predictions
output$eval_models_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list)

### Table of overlaid predictions
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

### Table of orig predictions
output$pretty_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list, selection = "single")

### Table of overlaid predictions
output$pretty_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = dt.list, selection = "single")

### Table of ensemble predictions
output$pretty_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

#----------------------------------------------------------
# Map control outputs

### Pretty plot manage to-plot
# Add map output
output$pretty_toplot_add_text <- renderText({
  pretty_toplot_add()
})
# Remove map output
output$pretty_toplot_remove_text <- renderText({
  pretty_toplot_remove()
})

### Pretty plot update
# Table
output$pretty_update_table_out <- renderDT({
  pretty_toplot_table()
}, options = dt.list, rownames = FALSE, selection = "single")

# Message
output$pretty_update_text <- renderText({
  pretty_update()
})

### Pretty plot plot/download
# Table
output$pretty_toplot_table_out <- renderDT({
  pretty_toplot_table()
}, options = dt.list, rownames = FALSE)

# Error output
output$pretty_plot_text <- renderText({
  pretty_plot()
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

### Color wheel for preview of color palette
output$pretty_color_preview_plot <- renderPlot({
  pretty_color_preview()
})

#----------------------------------------------------------
# Pretty plot, plotting

### Display box, render-ed each time so that box can be resized
output$pretty_display <- renderUI({
  box(
    title = "High Quality Maps", solidHeader = TRUE, status = "primary", width = 12, align = "center",
    height = ifelse(isTruthy(vals$pretty.plot), vals$pretty.plot$dims["height"], 4 * 96) + 60,
    shinycssloaders::withSpinner(plotOutput("pretty_plot_out"), type = 1)
  )
})

### Section with button to save map
# All needs to be within box() so single 'object' is returned
output$pretty_save_map <- renderUI({
  # if () keeps req() within pretty_range() from being called
  if (pretty_models_idx_count() == 1) pretty_range()

  box(
    width = 12,
    uiOutput("pretty_toplot_add_id_uiOut_text"),
    tags$br(),
    helpText("Note that most plot parameters below (including loaded additional objects)",
             "will stay the same unless changed by user, even when a different set of predictions is selected.",
             "Thus, be sure to check the parameters before saving a new map"),
    actionButton("pretty_toplot_add_execute", "Save map"),
    tags$br(), tags$br(),
    tags$span(textOutput("pretty_toplot_add_text"), style = "color: blue;")
  )
})


### Pretty plot dimension warnings
output$pretty_plot_dim_warnings_out <- renderText({
  pretty_plot_dim_warnings()
})

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
##### Export Predictions #####

### Table of orig predictions
output$export_table_orig_out <- renderDT({
  table_orig()[, 1:3]
}, options = dt.list, selection = "single")

### Table of overlaid predictions
output$export_table_over_out <- renderDT({
  table_overlaid()[, 1:3]
}, options = dt.list, selection = "single")

### Table of ensemble models
output$export_table_ens_out <- renderDT({
  table_ensembles()
}, options = dt.list, selection = "single")

###############################################################################
