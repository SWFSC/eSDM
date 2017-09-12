### Reactive plotting code for Ensemble App divided by tab


############################################################################### 
# Load Model Predictions tab

#################################################
### Get preview of predictions to plot in-app
model_pix_preview_event <- eventReactive(input$model_preview_execute, {
  model.pix <- model_pix()
  
  plot.multi.display(model.pix)
})

### Get preview of predictions to download
model_pix_download <- reactive({
  model.pix <- model_pix()
  
  # Plot
  plot.multi.download(model.pix)
})


#################################################
### Create list of objects to be used for preview of orig model predictions
model_pix <- reactive({
  if(length(vals$models.pix) == 0) return()
  
  idx.selected <- sort(as.numeric(input$models_loaded_table_rows_selected))
  validate(
    need(length(idx.selected) > 0, 
         "Please select at least one model from table to preview")
  )
  
  models.toplot <- vals$models.pix[idx.selected]
  
  plot.titles <- sapply(idx.selected, function(i) {
    paste0("Model file: ", vals$models.names[i], "\n", 
           "Data header: ", vals$models.data.names[[i]][1])
  })
  perc.ind <- input$model_select_action
  if(perc.ind == 1) perc.num <- input$model_preview_perc
  if(perc.ind == 2) perc.num <- input$model_download_preview_perc
  
  list(models.toplot = models.toplot, data.name = "Pred", 
       plot.titles = plot.titles, perc.num = perc.num)
})


###############################################################################
# Overlay tab

### Generate preview of overlay grid to plot in-app
plot_overlay_preview <- eventReactive(input$overlay_preview_execute, {
  p.options <- input$overlay_preview_options
  b.inc <- (("1" %in% p.options) & !is.null(vals$overlay.bound))
  l.inc <- (("2" %in% p.options) & !is.null(vals$overlay.land))
  
  validate(
    if(input$overlay_bound_gis) 
      need(!is.null(vals$overlay.bound),
           "Please either uncheck boundary box or load a boundary polygon"),
    if(input$overlay_land_gis) 
      need(!is.null(vals$overlay.land),
           "Please either uncheck land box or load a land polygon")
  )
  
  model.toplot <- overlay_preview_model()  # Func in ensOverlay.R
  if(b.inc) {
    bound.toplot <- vals$overlay.bound
    plot.extent <- extent(bound.toplot)
  } else {
    plot.extent <- extent(model.toplot)
    
  }
  plot.xlim <- c(plot.extent@xmin, plot.extent@xmax)
  plot.ylim <- c(plot.extent@ymin, plot.extent@ymax)
  
  plot(model.toplot, xlim = plot.xlim, ylim = plot.ylim, axes = T)
  if(l.inc) {
    plot(overlay_preview_land(), add = T, border = NA, col = "tan")
    # Func in ensOverlay.R
  }
  if(b.inc) {
    plot(vals$overlay.bound, add = T, border = "red", col = NA, lwd = 2)
  }
})


###############################################################################
# Create Ensembles tab

#################################################
# Functions for plotting in-app and for downloading

### Get preview of ensemble predictions to plot in-app
ens_pix_preview_event <- eventReactive(input$ens_preview_execute, {
  ens.pix <- ens_pix()
  
  plot.multi.display(ens.pix)
})

#######################################
### Get preview of ensemble predictions to download
ens_pix_download <- reactive({
  ens.pix <- ens_pix()
  
  # Plot
  plot.multi.download(ens.pix)
})


#################################################
### Create list of objects to be used for preview of ensemble predictions
ens_pix <- reactive({
  req(length(vals$ensemble.models) != 0)
  
  ensemble.which <- sort(input$ens_datatable_ensembles_rows_selected)
  validate(
    need(length(ensemble.which) > 0,
         "Please select at least one model from table to preview")
  )
  
  models.toplot <- create_ens_preview_model()
  
  plot.titles <- sapply(ensemble.which, function(i) {
    paste0("Ensembling method: ", vals$ensemble.method[i], "\n", 
           "Rescaling method: ", vals$ensemble.rescaling[i])
  })

  perc.ind <- input$ens_select_action
  if(perc.ind == 1) perc.num <- input$ens_preview_perc
  if(perc.ind == 2) perc.num <- input$ens_download_preview_perc
  
  list(models.toplot = models.toplot, data.name = "Pred.ens", 
       plot.titles = plot.titles, perc.num = perc.num)
})

###############################################################################
