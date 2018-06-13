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
    model.toplot = vals$models.ll[[model.idx]], perc.num = perc.num,
    plot.title = paste(vals$models.names[model.idx], "|",
                       vals$models.data.names[[model.idx]][1]),
    leg.title = ifelse(perc.num, "Relative prediction value",
                       "Absolute prediction value")
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
        fillOpacity = 0.8, group = "Base grid") %>%
      mapview::addMouseCoordinates(style = "basic")
    overlay.groups <- "Base grid"
    incProgress(0.5)

    if (l.inc) {
      land.toplot <- overlay_preview_base_land()
      leaf.map <- leaf.map %>%
        addPolygons(
          data = land.toplot, fillColor = "tan",
          stroke = FALSE, fillOpacity = 0.6, group = "Land")
      overlay.groups <- c(overlay.groups, "Land")
    }
    incProgress(0.1)
    if (b.inc) {
      leaf.map <- leaf.map %>%
        addPolygons(
          data = vals$overlay.bound, fillColor = "transparent",
          stroke = TRUE, color = "red", fillOpacity = 0.8,
          group = "Study area")
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


#################################################
### Download model preview is in 'server_plots_download.R'


###############################################################################
# Create Ensembles tab

#################################################
### Preview overlaid model predictions with assigned weight polygons
observeEvent(input$create_ens_weights_poly_preview_execute, {
  req(vals$ens.over.wpoly.filename)
  overlaid.which <- as.numeric(input$create_ens_weights_poly_preview_model)

  vals$ens.over.wpoly.plot <- list(
    st_geometry(vals$overlaid.models[[overlaid.which]]), overlaid.which
  )
})


#################################################
### Generate interactive preview of ensemble predictions to display in-app
observeEvent(input$ens_preview_interactive_execute, {
  req(length(vals$ensemble.models) > 0)

  perc.num <- as.numeric(input$ens_preview_interactive_perc)
  model.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)

  vals$ensemble.plot.leaf.idx <- model.idx
  vals$ensemble.plot.leaf <- list(
    model.toplot = vals$ensemble.models[[model.idx]], perc.num = perc.num,
    plot.title = paste(
      vals$ensemble.method[model.idx], "|", vals$ensemble.rescaling[model.idx],
      "|", vals$ensemble.overlaid.idx[model.idx]
    ),
    leg.title = ifelse(
      perc.num, "Relative prediction value", "Absolute prediction value"
    )
  )
})


#################################################
### Get preview of ensemble predictions to plot in-app
observeEvent(input$ens_preview_execute, {
  req(length(vals$ensemble.models) > 0)

  perc.num <- as.numeric(input$ens_preview_perc)

  models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  models.num <- length(models.idx)

  models.toplot <- vals$ensemble.models[models.idx]
  stopifnot(models.num == length(models.toplot))

  plot.titles <- sapply(models.idx, function(i) {
    paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
          "|", vals$ensemble.overlaid.idx[i])
  })

  vals$ensemble.plot.idx <- models.idx
  vals$ensemble.plot <- list(
    models.toplot = models.toplot, data.name = rep("Pred.ens", models.num),
    plot.titles = plot.titles, perc.num = perc.num,
    plot.dims = multiplot_inapp(models.num)
  )
})


#################################################
### Download ensemble preview is in 'server_plots_download.R'

###############################################################################
