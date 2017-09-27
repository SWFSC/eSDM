### temp.R


################### TEMP ######################
# Make sure no extra reactive values get added
observe({
  vals$models.pix
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
  vals$overlay.base.sp
  vals$overlay.base.specs
  vals$overlaid.models
  vals$overlaid.models.specs
  vals$ens.over.pix
  vals$ens.over.wpoly.filename
  vals$ens.over.wpoly.spdf
  vals$ens.over.wpoly.coverage
  vals$ensemble.models
  vals$ensemble.method
  vals$ensemble.weights
  vals$ensemble.rescaling
  vals$ensemble.overlaid.idx
  vals$ensemble.plotted.idx
  vals$eval.models.idx
  vals$eval.data.list
  vals$eval.data.specs
  vals$eval.data.gis.file.1
  vals$eval.data.gis.file.2p
  vals$eval.data.gis.file.2a
  vals$eval.metrics
  vals$eval.metrics.names
  vals$pretty.params.list
  vals$pretty.plotted.idx
  
  xxxyz <- reactiveValuesToList(vals)
  if(length(xxxyz) != 36) {
    warning(paste("There are now", length(xxxyz), "reactiveValues"))
  }
})
