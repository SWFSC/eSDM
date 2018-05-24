### 'Initialize' both general reactiveValues and tab-specific reactiveVal's
### Reactive funcs to load/save vals object


###############################################################################
# reactiveVal's used in specific tabs

### Pretty plot tabs
val.pretty.color.num <- reactiveVal(value = NULL)

###############################################################################
# 'Initialize' all 32 elements of vals

vals <- reactiveValues(
  # Objects that store loaded models and related info
  models.ll          = list(),    # List of models; crs is crs.ll
  models.orig        = list(),    # List of models; crs is crs of predictions when loaded
  models.names       = NULL,      # Vector of model names
  models.data.names  = NULL,      # List of vectors of model, error, and weights names
  models.pred.type   = NULL,      # Vector of prediction type (absolute vs relative)
  models.specs       = NULL,      # List of vectors of res, num of cells/preds, abund, and extent
  models.plotted.idx = NULL,      # Vector of the indicies of currently previewed original models

  # Objects that store data for and from overlay section
  overlay.bound         = NULL,   # Boundary sfc object; crs is crs.ll; always of length 1
  overlay.land          = NULL,   # Coastline/land SpatialPolygons; crs is crs.ll
  overlay.crs           = NULL,   # Class crs object of projection for overlay process
  overlay.base.idx      = NULL,   # Index of model used as base grid
  overlay.base.sfc      = NULL,   # sfc object of base grid
  overlay.base.specs    = NULL,   # models.spec info about base grid
  overlaid.models       = list(), # List of overlaid models
  overlaid.models.specs = NULL,   # vals$models.spec info about overlaid models

  # Objects that store elements used by ensemble and overlaid models
  ens.over.wpoly.filename = NULL, # List of filenames of polygons with weights; index corresponds to overlaid pred index
  ens.over.wpoly.sf       = NULL, # List of polygons with weights; index corresponds to overlaid pred index
  ens.over.wpoly.coverage = NULL, # List of overlap perc for weight to be applied; index corresponds to overlaid pred index

  # Objects that store spdfs of and data on created ensembles
  ensemble.models       = list(), # Ensemble model predictions
  ensemble.method       = NULL,   # Vector of ensembling methods used
  ensemble.weights      = NULL,   # Strings of weights used (if any)
  ensemble.rescaling    = NULL,   # Vector of rescaling methods used
  ensemble.overlaid.idx = NULL,   # Strings of indices of overlaid model predictions used
  ensemble.plotted.idx  = NULL,   # Vector of the indicies of currently previewed ensemble models

  # Objects that store data for evaluation metrics section
  eval.data           = NULL,     # Validation data (sf obj) with 'count' and 'sight' columns
  eval.data.specs     = NULL,     # Data type (1 = counts, 2 = p/a)
  eval.data.gis.info  = NULL,     # List with loaded gis validation data (sf obj) and shp/gdb indicator (num 2 or 3)
  eval.models.idx     = NULL,     # List of indices of evaluated models
  eval.metrics        = NULL,     # Metric values
  eval.metrics.names  = NULL,     # Names of metrics calculated

  # Objects that store data for high quality (pretty) plots
  pretty.params.list = NULL,      # List of parameters to use in high quality plots
  pretty.plotted.idx = NULL       # List of vectors of the indicies of currently pretty-plotted models
)


###############################################################################
# Load and save current data

### Load data
load_envir <- eventReactive(input$load_app_envir_file, {
  req(input$load_app_envir_file)

  file.load <- input$load_app_envir_file
  validate(
    need((substr_right(input$load_app_envir_file$name, 6) == ".RDATA" &
            input$load_app_envir_file$type == ""),
         "Error: Please load a file with the extension '.RDATA'")
  )

  withProgress(message = "Loading saved environment", value = 0.4, {
    load(file.load$datapath)
    validate(
      need(exists("vals.save"),
           paste0("Error: Loaded .RDATA file does not contain an envirnment",
                  "saved using this app"))
    )
    incProgress(0.4)

    vals$models.ll          <- vals.save[["models.ll"]]
    vals$models.orig        <- vals.save[["models.orig"]]
    vals$models.names       <- vals.save[["models.names"]]
    vals$models.data.names  <- vals.save[["models.data.names"]]
    vals$models.pred.type   <- vals.save[["models.pred.type"]]
    vals$models.specs       <- vals.save[["models.specs"]]
    vals$models.plotted.idx <- vals.save[["models.plotted.idx"]]

    vals$overlay.bound         <- vals.save[["overlay.bound"]]
    vals$overlay.land          <- vals.save[["overlay.land"]]
    vals$overlay.crs           <- vals.save[["overlay.crs"]]
    vals$overlay.base.idx      <- vals.save[["overlay.base.idx"]]
    vals$overlay.base.sfc      <- vals.save[["overlay.base.sfc"]]
    vals$overlay.base.specs    <- vals.save[["overlay.base.specs"]]
    vals$overlaid.models       <- vals.save[["overlaid.models"]]
    vals$overlaid.models.specs <- vals.save[["overlaid.models.specs"]]

    vals$ens.over.wpoly.filename <- vals.save[["ens.over.wpoly.filename"]]
    vals$ens.over.wpoly.sf       <- vals.save[["ens.over.wpoly.sf"]]
    vals$ens.over.wpoly.coverage <- vals.save[["ens.over.wpoly.coverage"]]

    vals$ensemble.models       <- vals.save[["ensemble.models"]]
    vals$ensemble.method       <- vals.save[["ensemble.method"]]
    vals$ensemble.weights      <- vals.save[["ensemble.weights"]]
    vals$ensemble.rescaling    <- vals.save[["ensemble.rescaling"]]
    vals$ensemble.overlaid.idx <- vals.save[["ensemble.overlaid.idx"]]
    vals$ensemble.plotted.idx  <- vals.save[["ensemble.plotted.idx"]]

    vals$eval.data          <- vals.save[["eval.data"]]
    vals$eval.data.specs    <- vals.save[["eval.data.specs"]]
    vals$eval.data.gis.info <- vals.save[["eval.data.gis.info"]]
    vals$eval.models.idx    <- vals.save[["eval.models.idx"]]
    vals$eval.metrics       <- vals.save[["eval.metrics"]]
    vals$eval.metrics.names <- vals.save[["eval.metrics.names"]]

    vals$pretty.params.list <- vals.save[["pretty.params.list"]]
    vals$pretty.plotted.idx <- vals.save[["pretty.plotted.idx"]]

    incProgress(0.1)

    # Update variable defaults
    if(!is.null(vals$overlay.bound)) {
      updateCheckboxInput(session, "overlay_bound", value = TRUE)
    }
    if(!is.null(vals$overlay.land)) {
      updateCheckboxInput(session, "overlay_land", value = TRUE)
    }

    incProgress(0.1)
  })

  Sys.sleep(0.5)

  return(paste("App data loaded from", file.load$name))
})

### This is here so that the selected saved app environment loads
###   even if user isn't on first page
observe({
  load_envir()
})


### Save data
# There is nothing to validate or return, so we don't need eventReactive
output$save_app_envir <- downloadHandler(
  filename = function() {
    input$save_app_envir_name
  },
  content = function(file) {
    withProgress(message = "Saving app data", value = 0.3, {
      vals.save <- reactiveValuesToList(vals)
      incProgress(0.5)

      save(vals.save, file = file)
      incProgress(0.2)
    })
  }
)

###############################################################################
### Make sure no extra reactive values get added and length(vals) == 32
observe({
  vals$models.ll
  vals$models.orig
  vals$models.names
  vals$models.data.name
  vals$models.pred.type
  vals$models.specs
  vals$models.plotted.idx
  vals$overlay.bound
  vals$overlay.land
  vals$overlay.crs
  vals$overlay.base.idx
  vals$overlay.base.sfc
  vals$overlay.base.specs
  vals$overlaid.models
  vals$overlaid.models.specs
  vals$ens.over.wpoly.filename
  vals$ens.over.wpoly.sf
  vals$ens.over.wpoly.coverage
  vals$ensemble.models
  vals$ensemble.method
  vals$ensemble.weights
  vals$ensemble.rescaling
  vals$ensemble.overlaid.idx
  vals$ensemble.plotted.idx
  vals$eval.models.idx
  vals$eval.data
  vals$eval.data.specs
  vals$eval.data.gis.info
  vals$eval.metrics
  vals$eval.metrics.names
  vals$pretty.params.list
  vals$pretty.plotted.idx


  if (length(reactiveValuesToList(vals)) != 32) {
    text.message <-
      shinyjs::alert(paste0(
        "There was an error in eSDM data storage and processing.",
        "\n",
        "Please restart the app and report this error via ",
        "the 'Submit Feedback' tab."
      ))
  }
})
