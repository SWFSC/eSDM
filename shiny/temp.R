### temp.R


################### TEMP ######################
# Save current vals (temp)
observeEvent(input$save_all, {
  vals.out <<- reactiveValuesToList(vals)
  print("reactiveVars saved to 'vals.out' list")
}, 
ignoreInit = T, ignoreNULL = T)

observe({
  vals$models.pix
  vals$models.ll
  vals$models.orig
  vals$models.names
  vals$models.data.name
  vals$models.pred.type
  vals$models.specs
  vals$overlay.bound
  vals$overlay.land
  vals$overlay.crs
  vals$overlay.base.idx
  vals$overlay.base.sp
  vals$overlay.base.specs
  vals$overlaid.models
  vals$overlaid.models.specs
  vals$ensemble.pix
  vals$ensemble.models
  vals$ensemble.method
  vals$ensemble.weights
  vals$ensemble.rescaling
  vals$ensemble.overlaid.idx
  vals$ensemble.wpoly.filename
  vals$ensemble.wpoly.spdf
  vals$ensemble.wpoly.coverage
  vals$eval.models.idx
  vals$eval.data.list
  vals$eval.data.specs
  vals$eval.data.gis.file.1
  vals$eval.data.gis.file.2p
  vals$eval.data.gis.file.2a
  vals$eval.metrics
  vals$eval.metrics.names
  vals$pretty.params.list
  
  xxxyz <- reactiveValuesToList(vals)
  if(length(xxxyz) != 33) {
    warning(paste("There are now", length(xxxyz), "reactiveValues"))
  }
})

# Load 'default' saved data
observeEvent(input$load_all, {
  withProgress(message = "Loading default", value = 0.3, {
    load("saved_app_vars/Ens_App_Save_Envir_weightedpoly.RDATA")
    incProgress(0.4)
    
    vals$models.pix        <- vals.save[["models.pix"]]
    vals$models.ll         <- vals.save[["models.ll"]]
    vals$models.orig       <- vals.save[["models.orig"]]
    vals$models.names      <- vals.save[["models.names"]]
    vals$models.data.names <- vals.save[["models.data.names"]]
    vals$models.pred.type  <- vals.save[["models.pred.type"]]
    vals$models.specs      <- vals.save[["models.specs"]]
    
    vals$overlay.bound         <- vals.save[["overlay.bound"]]
    vals$overlay.land          <- vals.save[["overlay.land"]]
    vals$overlay.crs           <- vals.save[["overlay.crs"]]
    vals$overlay.base.idx      <- vals.save[["overlay.base.idx"]]
    vals$overlay.base.sp       <- vals.save[["overlay.base.sp"]]
    vals$overlay.base.specs    <- vals.save[["overlay.base.specs"]]
    vals$overlaid.models       <- vals.save[["overlaid.models"]]
    vals$overlaid.models.specs <- vals.save[["overlaid.models.specs"]]
    
    vals$ensemble.pix <- vals.save[["ensemble.pix"]]
    
    vals$ensemble.models         <- vals.save[["ensemble.models"]]
    vals$ensemble.method         <- vals.save[["ensemble.method"]]
    vals$ensemble.weights        <- vals.save[["ensemble.weights"]]
    vals$ensemble.rescaling      <- vals.save[["ensemble.rescaling"]]
    vals$ensemble.overlaid.idx   <- vals.save[["ensemble.overlaid.idx"]]
    vals$ensemble.wpoly.filename <- vals.save[["ensemble.wpoly.filename"]]
    vals$ensemble.wpoly.spdf     <- vals.save[["ensemble.wpoly.spdf"]]
    vals$ensemble.wpoly.coverage <- vals.save[["ensemble.wpoly.coverage"]]
    
    vals$eval.models.idx       <- vals.save[["eval.models.idx"]]
    vals$eval.data.list        <- vals.save[["eval.data.list"]]
    vals$eval.data.specs       <- vals.save[["eval.data.specs"]]
    vals$eval.data.gis.file.1  <- vals.save[["eval.data.gis.file.1"]]
    vals$eval.data.gis.file.2p <- vals.save[["eval.data.gis.file.2p"]]
    vals$eval.data.gis.file.2a <- vals.save[["eval.data.gis.file.2a"]]
    vals$eval.metrics          <- vals.save[["eval.metrics"]]
    vals$eval.metrics.names    <- vals.save[["eval.metrics.names"]]
    
    vals$pretty.params.list <- vals.save[["pretty.params.list"]]
    
    
    # Update variable defaults
    if(!is.null(vals$overlay.bound))
      updateCheckboxInput(session, "overlay_bound_gis", value = TRUE)
    if(!is.null(vals$overlay.land))
      updateCheckboxInput(session, "overlay_land_gis", value = TRUE)
  })
})