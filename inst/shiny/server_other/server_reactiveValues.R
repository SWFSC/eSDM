### 'Initialize' both general reactiveValues and tab-specific reactiveVal's
# Note 'importing predictions' was 'loading models' when code was first written


###############################################################################
# reactiveVal's used in specific tabs

# Load workspace
### Flag used while loading saved workspace
val.load <- reactiveVal(value = FALSE)
val.workspace <- reactiveVal(value = list())

# Pretty plot
### Number of colors
val.pretty.color.num <- reactiveVal(value = NULL)
### Update params of hqm loaded additional objects
val.pretty.addobj.update <- reactiveVal(value = NULL)
### Update params of hqm saved maps
val.pretty.toplot.update <- reactiveVal(value = NULL)
### Update message
val.pretty.update.mess <- reactiveVal(value = NULL)


###############################################################################
# 'Initialize' all 45 elements of vals

vals <- reactiveValues(
  # Objects that store imported predictions and related info
  models.ll             = list(),  # List of original predictions; crs is crs.ll
  models.orig           = list(),  # List of original predictions; crs is native crs of preds
  models.names          = NULL,    # Vector of prediction file names
  models.data.names     = NULL,    # List of vectors of prediction and weight names
  models.pred.type      = NULL,    # Vector of prediction type (absolute vs relative vs abundance)
  models.specs          = NULL,    # List of vectors of res, num of cells & non-NA preds, abund, and extent
  models.plot.leaf      = NULL,    # Plot info of currently interactively previewed original predictions
  models.plot.leaf.idx  = NULL,    # Plot index of currently interactively previewed original predictions
  models.plot           = NULL,    # Plot info of currently static-previewed original predictions
  models.plot.idx       = NULL,    # Plot index of currently static-previewed original predictions

  # Objects that store data for and from overlay section
  overlay.bound         = NULL,    # Study area (boundary) polygon as sfc object; crs is crs.ll
  overlay.land          = NULL,    # Erasing (land) polygon as sfc object; crs is crs.ll
  overlay.plot          = NULL,    # Plot info for overlay base geometry preview
  overlay.crs           = NULL,    # Class crs object of crs for overlay process
  overlay.info          = NULL,    # List of 1) index of predictions used as base geometry and 2) percent overlap threshold
  overlay.base.sfc      = NULL,    # Base geometry (sfc object)

  overlaid.models       = list(),  # List of overlaid predictions. List elements are data frames; create sf object using overlay.base.sfc
  overlaid.specs        = NULL,    # Info about overlaid models; same info types as models.specs
  overlaid.plot         = NULL,    # Plot info of currently previewed overlaid predictions

  # Objects that store regional exclusion polygon info (used to be 'weight polygon', hence wpoly)
  ens.over.wpoly.filename = NULL,  # List of lists of filenames of exclusion polygons; index corresponds to overlaid pred index
  ens.over.wpoly.sf       = NULL,  # List of lists of exclusion polygons; index corresponds to overlaid pred index
  ens.over.wpoly.coverage = NULL,  # List of lists of overlap perc for poly to be applied; index corresponds to overlaid pred index
  ens.over.wpoly.plot     = NULL,  # Plot info of currently previewed exclusion polygons

  # Objects that store created ensembles and their information
  ensemble.models        = list(), # Ensemble predictions. List elements are data frames; create sf object using overlay.base.sfc
  ensemble.overlaid.res  = NULL,   # List of data frames of overlaid preds post-rescaling and regional exclusion for each ensemble
  ensemble.specs         = NULL,   # List of vectors with ensemble info: idx, rescaling, RegExc, ens method, weights, uncertainty method
  # ensemble.overlaid.idx  = NULL,   # Strings of indices of overlaid predictions used in ensemble
  # ensemble.rescaling     = NULL,   # Vector of rescaling methods used
  # ensemble.method        = NULL,   # Vector of ensembling methods used
  # ensemble.weights       = NULL,   # Vector of strings of weights used (if any)
  ensemble.plot.leaf     = NULL,   # Plot info of currently interactively previewed ensemble predictions
  ensemble.plot.leaf.idx = NULL,   # Plot index of currently interactively previewed ensemble predictions
  ensemble.plot          = NULL,   # Plot info of currently static-previewed ensemble predictions
  ensemble.plot.idx      = NULL,   # Plot index of currently static-previewed ensemble predictions

  # Objects that store data for evaluation metrics section
  eval.data          = NULL,       # Validation data (sf obj) with 'count' and 'sight' columns
  eval.data.specs    = NULL,       # Validation data type (1 = count, 2 = pres/abs)
  eval.data.gis.info = NULL,       # List with loaded GIS validation data (sf obj) and shp/gdb indicator (num 2 or 3, respectively)
  eval.models.idx    = NULL,       # List of indices of evaluated predictions
  eval.metrics       = NULL,       # List of metric values and message detailing overlap
  eval.metrics.names = NULL,       # Names of metrics calculated

  # Objects that store data for high quality (pretty) plots
  pretty.addobj        = NULL,     # List of objects and descriptor strings to be included in next saved map
  pretty.params.toplot = NULL,     # List of lists of parameters to use to create high quality plots
  pretty.toplot.idx    = NULL,     # List of lists of 3 elements (2 NULL, 1 an idx) representing the 3 tables
  pretty.plot          = NULL      # List of plot dimensions, pretty.toplot.idx, and pretty.params.list
)


###############################################################################
### Make sure no extra reactive values get added and length(vals) == 42
observe({
  vals$models.ll
  vals$models.orig
  vals$models.names
  vals$models.data.name
  vals$models.pred.type
  vals$models.specs
  vals$models.plot.leaf
  vals$models.plot.leaf.idx
  vals$models.plot
  vals$models.plot.idx

  vals$overlay.bound
  vals$overlay.land
  vals$overlay.plot
  vals$overlay.crs
  vals$overlay.info
  vals$overlay.base.sfc
  vals$overlaid.models
  vals$overlaid.specs
  vals$overlaid.plot

  vals$ens.over.wpoly.filename
  vals$ens.over.wpoly.sf
  vals$ens.over.wpoly.coverage
  vals$ens.over.wpoly.plot

  vals$ensemble.models
  vals$ensemble.overlaid.res
  vals$ensemble.specs
  vals$ensemble.plot.leaf
  vals$ensemble.plot.leaf.idx
  vals$ensemble.plot
  vals$ensemble.plot.idx

  vals$eval.models.idx
  vals$eval.data
  vals$eval.data.specs
  vals$eval.data.gis.info
  vals$eval.metrics
  vals$eval.metrics.names

  vals$pretty.addobj
  vals$pretty.params.toplot
  vals$pretty.toplot.idx
  vals$pretty.plot


  if (length(reactiveValuesToList(vals)) != 40) {
    showModal(modalDialog(
      title = "Error in eSDM GUI data storage and processing",

      tags$h5("There was an error in eSDM GUI data storage and processing.",
              "Please report this as an issue at",
              tags$a("https://github.com/smwoodman/eSDM/issues",
                     href = "https://github.com/smwoodman/eSDM/issues"),
              "and then restart the GUI."),

      footer = tagList(actionButton("close_gui_error", "Close GUI"))
    ))
  }
})

# observeEvent(input$close_gui_error, {}) is in 'server.R'

###############################################################################
