### Reactive plotting code for Ensemble App divided by tab


############################################################################### 
# Load Model Predictions tab

#################################################
### Get preview of predictions to plot in-app
model_pix_preview_event <- eventReactive(input$model_pix_preview_execute, {
  req(length(vals$models.pix) > 0)
  
  perc.num <- as.numeric(input$model_preview_perc)
  
  #----------------------------------------------
  # Same code as in model_pix_download()
  models.idx <- sort(as.numeric(input$models_loaded_table_rows_selected))
  
  validate(
    need(length(models.idx) > 0, 
         "Error: Please select at least one model from table to preview")
  )
  
  models.toplot <- vals$models.pix[models.idx]
  
  plot.titles <- sapply(models.idx, function(i) {
    paste0("Model file: ", vals$models.names[i], "\n", 
           "Data header: ", vals$models.data.names[[i]][1])
  })
  
  model.pix.list <- list(models.toplot = models.toplot, data.name = "Pred", 
                         plot.titles = plot.titles, perc.num = perc.num)
  #----------------------------------------------
  
  vals$models.plotted.idx <- models.idx
  plot.multi.display(model.pix.list)
})


#################################################
### Get preview of predictions to download
model_pix_download <- reactive({
  req(length(vals$models.pix) > 0)
  
  perc.num <- as.numeric(input$model_download_preview_perc)
  
  #----------------------------------------------
  # Same code as in model_pix_download()
  models.idx <- sort(as.numeric(input$models_loaded_table_rows_selected))
  
  validate(
    need(length(models.idx) > 0, 
         "Error: Please select at least one model from table to preview")
  )
  
  models.toplot <- vals$models.pix[models.idx]
  
  plot.titles <- sapply(models.idx, function(i) {
    paste0("Model file: ", vals$models.names[i], "\n", 
           "Data header: ", vals$models.data.names[[i]][1])
  })
  
  model.pix.list <- list(models.toplot = models.toplot, data.name = "Pred", 
                         plot.titles = plot.titles, perc.num = perc.num)
  #----------------------------------------------
  
  plot.multi.download(model.pix.list)
})


###############################################################################
# Overlay tab

#################################################
### Generate preview of base grid to plot in-app
#
plot_overlay_preview_base <- eventReactive(
  input$overlay_preview_base_execute, 
  {
    b.inc <- !is.null(vals$overlay.bound)
    l.inc <- !is.null(vals$overlay.land)
    
    validate(
      if (input$overlay_bound) 
        need(b.inc,
             paste("Error: Please either uncheck boundary box or", 
                   "load a boundary polygon")), 
      if (input$overlay_land) 
        need(l.inc,
             "Error: Please either uncheck land box or load a land polygon")
    )
    
    # overlay_preview_base_model() is in server_2_overlay.R
    model.toplot <- overlay_preview_base_model()
    
    if (b.inc) {
      bound.toplot <- vals$overlay.bound
      plot.extent <- extent(bound.toplot)
    } else {
      plot.extent <- extent(model.toplot)
      
    }
    plot.xlim <- c(plot.extent@xmin, plot.extent@xmax)
    plot.ylim <- c(plot.extent@ymin, plot.extent@ymax)
    
    plot(model.toplot, xlim = plot.xlim, ylim = plot.ylim, axes = TRUE)
    if (l.inc) {
      plot(overlay_preview_base_land(), add = TRUE, border = NA, col = "tan")
      # overlay_preview_base_land() is in server_2_overlay.R
    }
    if (b.inc) {
      plot(vals$overlay.bound, add = TRUE, border = "red", col = NA, lwd = 2)
    }
  }
)


#################################################
### Generate preview of overlaid model predictions to plot in-app
#
plot_overlay_preview_overlaid <- eventReactive(
  input$overlay_preview_overlaid_execute, 
  {
    models.toplot <- overlay_preview_overlaid_pix()
    overlaid.idx <- input$overlay_preview_overlaid_models
    
    plot.titles <- paste("Overlaid", overlaid.idx)
    
    overlaid.pix.list <- list(models.toplot = models.toplot, 
                              data.name = "Pred.overlaid", 
                              plot.titles = plot.titles, perc.num = 1)
    
    plot.multi.display(overlaid.pix.list)
  }
)


###############################################################################
# Create Ensembles tab

#################################################
### Get preview of ensemble predictions to plot in-app
# 
ens_pix_preview_event <- eventReactive(input$ens_preview_execute, {
  perc.num <- input$ens_preview_perc
  
  #----------------------------------------------
  # Same code as in ens_pix_download()
  ensemble.idx <- sort(input$ens_datatable_ensembles_rows_selected)
  validate(
    need(length(ensemble.idx) > 0,
         "Error: Please select at least one model from table to preview")
  )
  
  models.toplot <- create_ens_preview_model()
  
  plot.titles <- sapply(ensemble.idx, function(i) {
    paste0("Ensembling method: ", vals$ensemble.method[i], "\n", 
           "Rescaling method: ", vals$ensemble.rescaling[i])
  })
  
  ens.pix.list <- list(models.toplot = models.toplot, data.name = "Pred.ens", 
                       plot.titles = plot.titles, perc.num = perc.num)
  #----------------------------------------------
  
  vals$ensemble.plotted.idx <- ensemble.idx
  plot.multi.display(ens.pix.list)
})


#################################################
### Get preview of ensemble predictions to download
ens_pix_download <- reactive({
  perc.num <- input$ens_download_preview_perc
  
  #----------------------------------------------
  # Same code as in ens_pix_preview_event()
  ensemble.idx <- sort(input$ens_datatable_ensembles_rows_selected)
  validate(
    need(length(ensemble.idx) > 0,
         "Error: Please select at least one model from table to preview")
  )
  
  models.toplot <- create_ens_preview_model()
  
  plot.titles <- sapply(ensemble.idx, function(i) {
    paste0("Ensembling method: ", vals$ensemble.method[i], "\n", 
           "Rescaling method: ", vals$ensemble.rescaling[i])
  })
  
  ens.pix.list <- list(models.toplot = models.toplot, data.name = "Pred.ens", 
                       plot.titles = plot.titles, perc.num = perc.num)
  #----------------------------------------------

  plot.multi.download(ens.pix.list)
})

###############################################################################
