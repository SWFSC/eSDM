### 'Initialize' both general reactiveValues and tab-specific reactiveVal's
### Reactive funcs to load/save vals object


###############################################################################
# reactiveVal's used in specific tabs

# Clicked to other tabs
val.tabs <- reactiveVal(value = FALSE)

# Pretty ploy
### Number of colors
val.pretty.color.num <- reactiveVal(value = NULL)

### Ppdate params of loaded additional objects
val.pretty.addobj.update <- reactiveVal(value = NULL)

### Update params of saved maps (to-plot list items)
val.pretty.toplot.update <- reactiveVal(value = NULL)

###############################################################################
# 'Initialize' all 42 elements of vals

vals <- reactiveValues(
  # Objects that store loaded models and related info
  models.ll             = list(),  # List of models; crs is crs.ll
  models.orig           = list(),  # List of models; crs is crs of predictions when loaded
  models.names          = NULL,    # Vector of model names
  models.data.names     = NULL,    # List of vectors of model, error, and weights names
  models.pred.type      = NULL,    # Vector of prediction type (absolute vs relative)
  models.specs          = NULL,    # List of vectors of res, num of cells/preds, abund, and extent
  models.plot.leaf      = NULL,    # Plot info of currently interactively previewed original models
  models.plot.leaf.idx  = NULL,    # Plot index of currently interactively previewed original models
  models.plot           = NULL,    # Plot info of currently static-previewed original models
  models.plot.idx       = NULL,    # Plot index of currently static-previewed original models

  # Objects that store data for and from overlay section
  overlay.bound         = NULL,    # Boundary sfc object; crs is crs.ll; always of length 1
  overlay.land          = NULL,    # Coastline/land sfc object; crs is crs.ll
  overlay.plot          = NULL,    # Plot info for overlay base preview
  overlay.crs           = NULL,    # Class crs object of projection for overlay process
  overlay.info          = NULL,    # List of index of model used as base grid and overlap percentage
  overlay.base.sfc      = NULL,    # sfc object that is base grid
  overlaid.models       = list(),  # List of overlaid models
  overlaid.models.specs = NULL,    # models.spec info about overlaid models
  overlaid.plot         = NULL,    # Plot info of currently previewed overlaid models

  # Objects that store elements used by ensemble and overlaid models
  ens.over.wpoly.filename = NULL,  # List of filenames of polygons with weights; index corresponds to overlaid pred index
  ens.over.wpoly.sf       = NULL,  # List of polygons with weights; index corresponds to overlaid pred index
  ens.over.wpoly.coverage = NULL,  # List of overlap perc for weight to be applied; index corresponds to overlaid pred index
  ens.over.wpoly.plot     = NULL,  # Plot info of currently previewed weighted polygons

  # Objects that store created ensembles and their data
  ensemble.models        = list(), # Ensemble model predictions
  ensemble.method        = NULL,   # Vector of ensembling methods used
  ensemble.weights       = NULL,   # Vector of strings of weights used (if any)
  ensemble.rescaling     = NULL,   # Vector of rescaling methods used
  ensemble.overlaid.idx  = NULL,   # Strings of indices of overlaid model predictions used
  ensemble.plot.leaf     = NULL,   # Plot info of currently interactively previewed ensemble models
  ensemble.plot.leaf.idx = NULL,   # Plot index of currently interactively previewed ensemble models
  ensemble.plot          = NULL,   # Plot info of currently previewed ensemble models
  ensemble.plot.idx      = NULL,   # Plot index of currently previewed ensemble models

  # Objects that store data for evaluation metrics section
  eval.data          = NULL,       # Validation data (sf obj) with 'count' and 'sight' columns
  eval.data.specs    = NULL,       # Data type (1 = counts, 2 = p/a)
  eval.data.gis.info = NULL,       # List with loaded gis validation data (sf obj) and shp/gdb indicator (num 2 or 3)
  eval.models.idx    = NULL,       # List of indices of evaluated models
  eval.metrics       = NULL,       # Metric values
  eval.metrics.names = NULL,       # Names of metrics calculated

  # Objects that store data for high quality (pretty) plots
  pretty.addobj        = NULL,     # List of objects and descriptor strings to be plotted
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
  vals$overlaid.models.specs
  vals$overlaid.plot
  vals$ens.over.wpoly.filename
  vals$ens.over.wpoly.sf
  vals$ens.over.wpoly.coverage
  vals$ens.over.wpoly.plot
  vals$ensemble.models
  vals$ensemble.method
  vals$ensemble.weights
  vals$ensemble.rescaling
  vals$ensemble.overlaid.idx
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


  if (length(reactiveValuesToList(vals)) != 42) {
    #TODO? Change this to a modal whose button closes the app?
    shinyjs::alert(paste(
      "There was an error in the eSDM GUI data storage and processing;",
      "please restart the GUI and report this as an issue."
    ))

    browser() #TODO: remove
  }
})
