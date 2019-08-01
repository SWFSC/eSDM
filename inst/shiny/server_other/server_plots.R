# Reactive plotting code for eSDM GUI organized by tab
# Download code is in server_plots_download.R
# High Quality Maps code is in server_5_prettyPlot folder


###############################################################################
# Import Predictions tab

#------------------------------------------------------------------------------
### Generate interactive preview of original predictions to plot in-app
observeEvent(input$model_preview_interactive_execute, {
  req(length(vals$models.ll) > 0)

  perc.num <- as.numeric(input$model_preview_interactive_perc)
  model.idx <- as.numeric(input$models_loaded_table_rows_selected)

  d <- vals$models.data.names[[model.idx]]
  data.names <- c(
    ifelse(is.na(d[1]), NA, "Pred"), ifelse(is.na(d[2]), NA, "SE"),
    ifelse(is.na(d[3]), NA, "Weight")
  ); rm(d)

  vals$models.plot.leaf.idx <- model.idx

  vals$models.plot.leaf <- list(
    model.toplot = vals$models.ll[[model.idx]], data.names = data.names,
    plot.title = paste("Original", model.idx), perc.num = perc.num,
    pal = switch(perc.num, pal.esdm, NA),
    leg.title = switch(
      perc.num, "Relative prediction value", "Absolute prediction value"
    )
  )
})


#------------------------------------------------------------------------------
### Generate static preview of original predictions to plot in-app
observeEvent(input$model_preview_execute, {
  req(length(vals$models.ll) > 0)

  perc.num <- as.numeric(input$model_preview_perc)
  models.idx <- as.numeric(input$models_loaded_table_rows_selected)
  models.num <- length(models.idx)

  models.toplot <- vals$models.ll[models.idx]
  data.names <- rep("Pred", models.num)
  plot.titles <- paste("Original", models.idx)
  var.key <- NULL

  if (input$model_preview_var == 2) {
    models.toplot <- c(models.toplot, models.toplot)
    data.names <- c(data.names, rep("SE", models.num))
    plot.titles <- c(plot.titles, paste("Original", models.idx, "- SE"))

    if (perc.num == 2) var.key <- c(rep(NA, models.num), seq_len(models.num))
    models.num <- models.num * 2
  }

  stopifnot(models.num == length(models.toplot))

  vals$models.plot.idx <- models.idx
  vals$models.plot <- list(
    models.toplot = models.toplot, data.names = data.names,
    plot.titles = plot.titles, perc.num = perc.num,
    pal = switch(perc.num, pal.esdm, NA),
    plot.dims = multiplot_inapp(models.num), var.key = var.key
  )
})


###############################################################################
# Overlay Predictions tab

#------------------------------------------------------------------------------
### Generate preview of base geometry to plot in-app
# Helper reactive functions are in server_2_overlay_plot.R
overlay_preview_base_create <- eventReactive(input$overlay_preview_base_execute, {
  vals$overlay.plot <- NULL

  withProgress(message = "Preparing base geometry preview", value = 0.3, {
    b.inc <- !is.null(vals$overlay.bound)
    l.inc <- !is.null(vals$overlay.land)
    model.toplot <- overlay_preview_base_model()


    # Validation checks
    if (b.inc) {
      temp <- suppressMessages(
        st_intersects(st_union(vals$overlay.bound), model.toplot)
      )
      validate(
        need(length(temp[[1]]) > 0,
             paste("Error: The study area polygon and specified base",
                   "geometry do not intersect"))
      )
      rm(temp)
    }
    if (l.inc) {
      temp <- suppressMessages(
        st_intersects(st_union(overlay_preview_base_land()), model.toplot)
      )
      validate(
        need(length(temp[[1]]) > 0,
             paste("Error: The erasing polygon and specified base geometry",
                   "do not intersect"))
      )
      rm(temp)
    }

    # Create leaflet
    model.toplot.360 <- check_360(model.toplot)
    model.toplot <- check_preview360_split(model.toplot)

    leaf.map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
      addPolygons(
        data = st_geometry(model.toplot),
        stroke = TRUE, color = "black", fillColor = "lightskyblue",
        fillOpacity = 0.8, group = "Base geometry")

    if (requireNamespace("leafem", quietly = TRUE)) {
      leaf.map <- leaf.map %>% leafem::addMouseCoordinates()
    }

    overlay.groups <- "Base geometry"
    incProgress(0.5)

    if (l.inc) {
      erasing.toplot <- check_preview360_split(
        overlay_preview_base_land(), force.360 = model.toplot.360
      )
      leaf.map <- leaf.map %>%
        addPolygons(
          data = erasing.toplot, fillColor = "tan",
          stroke = FALSE, fillOpacity = 0.6, group = "Erasing polygon")
      overlay.groups <- c(overlay.groups, "Erasing polygon")
    }
    incProgress(0.1)
    if (b.inc) {
      # Use st_union because study area is only on polygon
      bound.toplot <- st_union(check_preview360_split(
        vals$overlay.bound, force.360 = model.toplot.360
      ))
      leaf.map <- leaf.map %>%
        addPolygons(
          data = bound.toplot, fillColor = "transparent",
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

  ""
})


#------------------------------------------------------------------------------
### Generate preview of overlaid predictions to plot in-app
observeEvent(input$overlay_preview_overlaid_execute, {
  perc.num <- as.numeric(input$overlay_preview_overlaid_models_perc)
  models.idx <- as.numeric(input$overlay_preview_overlaid_models)
  models.num <- length(models.idx)

  models.toplot <- lapply(vals$overlaid.models[models.idx], function(i) {
    st_transform(
      st_sf(i, geometry = vals$overlay.base.sfc, agr = "constant"), 4326
    )
  })
  data.names <- rep("Pred", models.num)
  plot.titles <- paste("Overlaid", models.idx)
  var.key <- NULL

  if (input$overlay_preview_overlaid_models_var == 2) {
    models.toplot <- c(models.toplot, models.toplot)
    data.names <- c(data.names, rep("SE", models.num))
    plot.titles <- c(plot.titles, paste("Overlaid", models.idx, "- SE"))

    if (perc.num == 2) var.key <- c(rep(NA, models.num), seq_len(models.num))
    models.num <- models.num * 2
  }

  vals$overlaid.plot <- list(
    models.toplot = models.toplot,
    data.names = data.names,
    plot.titles = plot.titles, perc.num = perc.num,
    pal = switch(perc.num, pal.esdm, NA),
    plot.dims = multiplot_inapp(models.num), var.key = var.key
  )
})


###############################################################################
# Create Ensemble Predictions tab

#------------------------------------------------------------------------------
### Preview overlaid predictions with assigned exclusion polygons
observeEvent(input$create_ens_reg_preview_execute, {
  req(vals$ens.over.wpoly.filename)

  overlaid.which <- as.numeric(input$create_ens_reg_preview_model)
  overlaid.toplot <- check_preview360_split(vals$overlay.base.sfc)

  vals$ens.over.wpoly.plot <- list(overlaid.toplot, overlaid.which)
})


#------------------------------------------------------------------------------
### Generate interactive preview of ensemble predictions to plot in-app
observeEvent(input$ens_preview_interactive_execute, {
  req(length(vals$ensemble.models) > 0)

  perc.num <- as.numeric(input$ens_preview_interactive_perc)
  model.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  req(length(model.idx) == 1)

  model.toplot <- st_transform(st_sf(
    vals$ensemble.models[[model.idx]], geometry = vals$overlay.base.sfc,
    agr = "constant"
  ), 4326)

  vals$ensemble.plot.leaf.idx <- model.idx
  vals$ensemble.plot.leaf <- list(
    model.toplot = model.toplot, data.names = c("Pred_ens", "SE_ens", NA),
    plot.title = paste("Ensemble", model.idx),
    perc.num = perc.num, pal = switch(perc.num, pal.esdm, NA),
    leg.title = switch(
      perc.num, "Relative prediction value", "Absolute prediction value"
    )
  )
})


#------------------------------------------------------------------------------
### Generate static preview of ensemble predictions to plot in-app
observeEvent(input$ens_preview_execute, {
  req(length(vals$ensemble.models) > 0)

  perc.num <- as.numeric(input$ens_preview_perc)
  models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  models.num <- length(models.idx)

  models.toplot <- lapply(vals$ensemble.models[models.idx], function(i) {
    st_transform(
      st_sf(i, geometry = vals$overlay.base.sfc, agr = "constant"), 4326
    )
  })
  data.names <- rep("Pred_ens", models.num)
  plot.titles <- paste("Ensemble", models.idx)
  var.key <- NULL

  if (input$ens_preview_var == 2) {
    models.toplot <- c(models.toplot, models.toplot)
    data.names <- c(data.names, rep("SE_ens", models.num))
    plot.titles <- c(plot.titles, paste("Ensemble", models.idx, "- SE"))

    if (perc.num == 2) var.key <- c(rep(NA, models.num), seq_len(models.num))
    models.num <- models.num * 2
  }

  stopifnot(models.num == length(models.toplot))


  vals$ensemble.plot.idx <- models.idx
  vals$ensemble.plot <- list(
    models.toplot = models.toplot, data.names = data.names,
    plot.titles = plot.titles, perc.num = perc.num,
    pal = switch(perc.num, pal.esdm, NA),
    plot.dims = multiplot_inapp(models.num), var.key = var.key
  )
})

###############################################################################
