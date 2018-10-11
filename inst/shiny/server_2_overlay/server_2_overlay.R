### Select base geometry and perform overlay


###############################################################################
### Flag for whether or not to display overlay tab items
output$overlay_display_flag <- reactive({
  (length(vals$models.ll) > 0) | length(vals$overlaid.models) > 0
})
outputOptions(output, "overlay_display_flag", suspendWhenHidden = FALSE)

### Flag for if overlaid predictions have been created
output$overlay_preview_display_flag <- reactive({
  length(vals$overlaid.models) > 0
})
outputOptions(
  output, "overlay_preview_display_flag", suspendWhenHidden = FALSE
)


###############################################################################
### Remove study area (boundary) poly if 'import study area' box is unchecked
observeEvent(input$overlay_bound, {
  if (!input$overlay_bound) {
    vals$overlay.bound <- NULL

    shinyjs::reset("overlay_bound_csv_file")
    shinyjs::reset("overlay_bound_gis_shp_files")
    shinyjs::reset("overlay_bound_gis_gdb_path")
    shinyjs::reset("overlay_bound_gis_gdb_name")
  }
})

### Remove erasing (land) poly if 'import erasing polygon' box is unchecked
observeEvent(input$overlay_land, {
  if (!input$overlay_land){
    vals$overlay.land <- NULL

    shinyjs::reset("overlay_land_csv_file")
    shinyjs::reset("overlay_land_gis_shp_files")
    shinyjs::reset("overlay_land_gis_gdb_path")
    shinyjs::reset("overlay_land_gis_gdb_name")
  }
})

### Reset erasing polygon widget info as necessary
observeEvent(input$overlay_land_load_type, {
  shinyjs::reset("overlay_land_csv_file")
  shinyjs::reset("overlay_land_gis_shp_files")
})

observeEvent(input$overlay_land_file_type, {
  shinyjs::reset("overlay_land_csv_file")
  shinyjs::reset("overlay_land_gis_shp_files")
})


###############################################################################
# Prep for base geometry preview

###########################################################
### Get predictions selected for base geometry in crs.ll
overlay_preview_base_model <- reactive({
  base.which <- as.numeric(input$overlay_loaded_table_rows_selected)

  validate(
    need(length(base.which) == 1,
         paste("Error: Please select exactly one set of predictions from the",
               "table to use as the base geometry"))
  )

  vals$models.ll[[base.which]]
})

### Crop (clip) erasing polygon by bbox of base geometry
overlay_preview_base_land <- reactive({
  base.bbox <- st_bbox(overlay_preview_base_model())
  base.bbox[1:2] <- base.bbox[1:2] - 2
  base.bbox[3:4] <- base.bbox[3:4] + 2

  suppressMessages(
    st_crop(vals$overlay.land, base.bbox)
  )
})

###############################################################################
