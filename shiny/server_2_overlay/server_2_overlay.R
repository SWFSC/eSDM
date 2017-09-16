### Code for selecting an overlay grid and overlaying loaded models


###############################################################################

### Flag for whether or not to display overlay page items
output$overlay_display_flag <- reactive({
  length(vals$models.ll) != 0
})
outputOptions(output, "overlay_display_flag", suspendWhenHidden = FALSE)


### Remove boundary polygon if 'include boundary' box is unchecked
observeEvent(input$overlay_bound, {
  if(!input$overlay_bound) {
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
  if(!input$overlay_land){
    vals$overlay.land <- NULL
    
    shinyjs::reset("overlay_land_csv_file")
    shinyjs::reset("overlay_land_gis_shp_files")
    shinyjs::reset("overlay_land_gis_gdb_path")
    shinyjs::reset("overlay_land_gis_gdb_name")
    shinyjs::reset("overlay_preview_include_land")
  }
})


###############################################################################
# Helper reaction functions for plot_overlay_preview()

### Get selected model with crs of crs.ll
overlay_preview_model <- reactive({
  idx.model <- as.numeric(input$overlay_loaded_table_rows_selected)
  validate(
    need(length(idx.model) == 1, 
         paste("Please select exactly one model from the", 
               "table to use as grid for preview"))
  )
  
  model.toplot <- vals$models.ll[[idx.model]]
  
  # If boundary polygon is loaded, get intersection of model and boundary poly
  sp.clipper <- vals$overlay.bound
  if(is.null(sp.clipper) | !("1" %in% input$overlay_preview_options)) {
    return(model.toplot)
  }
  
  model.intersect.try <- try(gIntersection(model.toplot, sp.clipper, 
                                           byid = TRUE), 
                             silent = TRUE)
  
  if(class(model.intersect.try) == "try-error") {
    model.toplot
  } else {
    model.intersect.try
  }
})

### Clip land by extent of boundary (if provided) or selected model
# For overlay base preview
overlay_preview_land <- reactive({
  overlay.land <- vals$overlay.land
  
  # If boundary polygon is loaded, clip land by boundary poly extent
  sp.clipper <- vals$overlay.bound
  if(is.null(sp.clipper)  | !("1" %in% input$overlay_preview_options)) {
    sp.clipper <- overlay_preview_model()
  }

  land.clip.try <- try(gClipExtent(overlay.land, sp.clipper), 
                       silent = TRUE)
  
  if(class(land.clip.try) == "try-error") {
    overlay.land
  } else {
    land.clip.try
  }
})


###############################################################################
### Generate list of SPixDF objects for overlaid model predictions preview
overlay_preview_overlaid_pix <- reactive({
  overlaid.which <- as.numeric(input$overlay_preview_overlaid_models)
  
  validate(
    need(length(overlaid.which) > 0,
         paste("Please select at least one set of overlaid", 
               "model predictions to preview"))
  )

  overlaid.spdf.list <- vals$overlaid.models[overlaid.which]
  
  # Get SPixDF object, whose data being rasterized pixel indices
  # Pixel object was originally made for ensemble previews,...
  # ...but works for overlaid previews too
  overlaid.pix <- vals$ensemble.pix
  names(overlaid.pix) <- "Pred.overlaid.pix"
  overlaid.pix.idx <- vals$ensemble.pix$pix
  
  overlaid.pix.list <- lapply(overlaid.spdf.list, function(i) {
    overlaid.pix.curr <- overlaid.pix
    overlaid.pix.curr$Pred.overlaid.pix <- i$Pred.overlaid[overlaid.pix.idx]
    names(overlaid.pix.curr) <- "Pred.overlaid"
    
    overlaid.pix.curr
  })
  
  overlaid.pix.list
})