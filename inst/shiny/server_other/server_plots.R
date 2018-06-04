### Reactive plotting code for Ensemble App divided by tab


###############################################################################
# Load Model Predictions tab

#################################################
### Generate interactive preview of predictions to display in-app
observeEvent(input$model_preview_interactive_execute, {
  req(length(vals$models.ll) > 0)

  perc.num <- as.numeric(input$model_preview_interactive_perc)
  model.idx <- as.numeric(input$models_loaded_table_rows_selected)

  vals$models.plot.leaf.idx <- model.idx
  vals$models.plot.leaf <- list(
    model.toplot = vals$models.ll[[model.idx]], perc.num = perc.num
  )
})


#################################################
### Generate static preview of predictions to display in-app
observeEvent(input$model_preview_execute, {
  req(length(vals$models.ll) > 0)

  perc.num <- as.numeric(input$model_preview_perc)
  models.idx <- as.numeric(input$models_loaded_table_rows_selected)
  models.num <- length(models.idx)

  models.toplot <- vals$models.ll[models.idx]
  stopifnot(models.num == length(models.toplot))

  plot.titles <- sapply(models.idx, function(i) {
    paste(vals$models.names[i], "|", vals$models.data.names[[i]][1])
  })

  vals$models.plot.idx <- models.idx
  vals$models.plot <- list(
    models.toplot = models.toplot, data.name = rep("Pred", models.num),
    plot.titles = plot.titles, perc.num = perc.num,
    plot.dims = multiplot_inapp(models.num)
  )
})


#################################################
### Download predictions code is in server_plots_download.R


###############################################################################
# Overlay tab

#################################################
### Generate preview of base grid to plot in-app
# Helper reactive functions are in server_2_overlay_plot.R
observeEvent(input$overlay_preview_base_execute, {
  withProgress(message = "Preparing base grid preview", value = 0.3, {
    b.inc <- !is.null(vals$overlay.bound)
    l.inc <- !is.null(vals$overlay.land)

    model.toplot <- overlay_preview_base_model()

    leaf.map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
      addPolygons(
        data = st_geometry(model.toplot),
        stroke = TRUE, color = "black", fillColor = "lightskyblue",
        fillOpacity = 1, group = "Base grid") %>%
      mapview::addMouseCoordinates()
    overlay.groups <- "Base grid"
    incProgress(0.5)

    if (l.inc) {
      land.toplot <- overlay_preview_base_land()
      leaf.map <- leaf.map %>%
        addPolygons(
          data = land.toplot, fillColor = "tan",
          stroke = FALSE, fillOpacity = 0.7, group = "Land")
      overlay.groups <- c(overlay.groups, "Land")
    }
    incProgress(0.1)
    if (b.inc) {
      leaf.map <- leaf.map %>%
        addPolygons(
          data = vals$overlay.bound, fillColor = "transparent",
          stroke = TRUE, color = "red", group = "Study area")
      overlay.groups <- c(overlay.groups, "Study area")
    }
    incProgress(0.1)

    leaf.map <- leaf.map %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = overlay.groups,
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      )

    vals$overlay.plot <- leaf.map
  })
})


#################################################
### Generate preview of overlaid model predictions to plot in-app
#
observeEvent(input$overlay_preview_overlaid_execute, {
  overlaid.idx <- as.numeric(input$overlay_preview_overlaid_models)
  models.toplot <- vals$overlaid.models[overlaid.idx]
  models.num <- length(models.toplot)

  plot.titles <- paste("Overlaid", overlaid.idx)

  vals$overlaid.plot <- list(
    models.toplot = models.toplot,
    data.names = rep("Pred.overlaid", models.num),
    plot.titles = plot.titles, plot.dims = multiplot_inapp(models.num)
  )
})


###############################################################################
# Create Ensembles tab

#################################################
### Get preview of ensemble predictions to plot in-app
#
ens_preview_event <- eventReactive(input$ens_preview_execute, {
  req(length(vals$ensemble.models) > 0)

  perc.num <- as.numeric(input$ens_preview_perc)

  #----------------------------------------------
  models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  models.num <- length(models.idx)

  validate(
    need(models.num > 0,
         "Error: Please select at least one model from table to preview")
  )

  models.toplot <- vals$ensemble.models[models.idx]
  stopifnot(models.num == length(models.toplot))

  plot.titles <- sapply(models.idx, function(i) {
    paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
          "|", vals$ensemble.overlaid.idx[i])
  })

  #----------------------------------------------


  vals$ensemble.plotted.idx <- models.idx
  plot.dims <- multiplot_inapp(models.num)

  eSDM::multiplot_layout(
    models.toplot, rep("Pred.ens", models.num), plot.titles,
    perc.num, pal.esdm, leg.perc.esdm,
    plot.dims[1], plot.dims[2], plot.dims[3], plot.dims[4], plot.dims[5]
  )
})


#################################################
### Get preview of ensemble predictions to download
ens_preview_download <- reactive({
  perc.num <- input$ens_download_preview_perc

  # #----------------------------------------------
  # # Same code as in ens_pix_preview_event()
  # ensemble.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  # validate(
  #   need(length(ensemble.idx) > 0,
  #        "Error: Please select at least one model from table to preview")
  # )
  #
  # models.toplot <- create_ens_preview_model()
  #
  # plot.titles <- sapply(ensemble.idx, function(i) {
  #   # paste0("Ensembling method: ", vals$ensemble.method[i], "\n",
  #   #        "Rescaling method: ", vals$ensemble.rescaling[i])
  #   paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
  #         "|", vals$ensemble.overlaid.idx[i])
  # })
  #
  # ens.pix.list <- list(models.toplot = models.toplot, data.name = "Pred.ens",
  #                      plot.titles = plot.titles, perc.num = perc.num)
  # #----------------------------------------------
  #
  # plot.multi.download(ens.pix.list)
})

###############################################################################
