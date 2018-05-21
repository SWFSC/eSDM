### Code for selecting an overlay grid and overlaying loaded models


###############################################################################

### Flag for whether or not to display overlay tab items
output$overlay_display_flag <- reactive({
  length(vals$models.ll) != 0
})
outputOptions(output, "overlay_display_flag", suspendWhenHidden = FALSE)


### Flag for if overlaid models have been created
output$overlay_preview_display_flag <- reactive({
  length(vals$overlaid.models) != 0
})
outputOptions(output, "overlay_preview_display_flag",
              suspendWhenHidden = FALSE)


### Remove boundary polygon if 'include boundary' box is unchecked
observeEvent(input$overlay_bound, {
  if (!input$overlay_bound) {
    vals$overlay.bound <- NULL

    shinyjs::reset("overlay_bound_csv_file")
    shinyjs::reset("overlay_bound_gis_shp_files")
    shinyjs::reset("overlay_bound_gis_gdb_path")
    shinyjs::reset("overlay_bound_gis_gdb_name")
    shinyjs::reset("overlay_preview_include_bound")
  }
})

### Remove land polygon if 'include land' box is unchecked
observeEvent(input$overlay_land, {
  if (!input$overlay_land){
    vals$overlay.land <- NULL

    shinyjs::reset("overlay_land_csv_file")
    shinyjs::reset("overlay_land_gis_shp_files")
    shinyjs::reset("overlay_land_gis_gdb_path")
    shinyjs::reset("overlay_land_gis_gdb_name")
    shinyjs::reset("overlay_preview_include_land")
  }
})


###############################################################################
# Prep for overlay preview window

###########################################################
### Get selected model with crs of crs.ll
overlay_preview_base_model <- reactive({
  base.which <- as.numeric(input$overlay_loaded_table_rows_selected)

  validate(
    need(length(base.which) == 1,
         paste("Error: Please select exactly one model from the",
               "table to use as grid for preview"))
  )

  vals$models.ll[[base.which]]
})

### Crop land by bbox of selected model
# For overlay base preview
overlay_preview_base_land <- reactive({
  suppressMessages(
    st_crop(vals$overlay.land, st_bbox(overlay_preview_base_model()))
  )
})

###############################################################################
