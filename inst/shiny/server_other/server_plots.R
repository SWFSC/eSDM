### Reactive plotting code for Ensemble App divided by tab


###############################################################################
# Load Model Predictions tab

#################################################
### Generate interactive preview of predictions to display in-app
model_pix_preview_interactive_event <- eventReactive(
  input$model_pix_preview_interactive_execute,
  {
    req(length(vals$models.ll) > 0)

    # Show/hide is here because for some reason observeEvent didn't run before
    #   model_pix_preview_interactive_event()
    shinyjs::hide("model_pix_preview_plot", time = 0)
    shinyjs::show("model_pix_preview_interactive_plot", time = 0)

    perc.num <- as.numeric(input$model_preview_interactive_perc)

    model.idx <- as.numeric(input$models_loaded_table_rows_selected)
    validate(
      need(length(model.idx)  == 1,
           paste("Error: Please ensure that exactly one model from the table",
                 "is selected to preview"))
    )
    model.toplot <- vals$models.ll[[model.idx]]

    vals$models.plotted.idx <- model.idx

    eSDM::preview_interactive(model.toplot, "Pred", perc.num, pal.esdm,
                              leg.perc.esdm)
  }
)


#################################################
### Generate static preview of predictions to display in-app
model_pix_preview_event <- eventReactive(input$model_pix_preview_execute, {
  req(length(vals$models.ll) > 0)

  perc.num <- as.numeric(input$model_preview_perc)

  #----------------------------------------------
  models.idx <- as.numeric(input$models_loaded_table_rows_selected)
  models.num <- length(models.idx)

  validate(
    need(length(models.idx) > 0,
         "Error: Please select at least one model from table to preview")
  )

  models.toplot <- vals$models.ll[models.idx]
  stopifnot(models.num == length(models.toplot))

  plot.titles <- sapply(models.idx, function(i) {
    paste(vals$models.names[i], "|", vals$models.data.names[[i]][1])
  })

  #----------------------------------------------


  vals$models.plotted.idx <- models.idx
  plot.dims <- multiplot_inapp(models.num)

  eSDM::multiplot_layout(
    models.toplot, rep("Pred", models.num), plot.titles,
    perc.num, pal.esdm, leg.perc.esdm,
    plot.dims[1], plot.dims[2], plot.dims[3], plot.dims[4], plot.dims[5]
  )
})


#################################################
### Download predictions code is in server_plots_download.R


###############################################################################
# Overlay tab

#################################################
### Generate preview of base grid to plot in-app
# Helper reactive functions are in server_2_overlay_plot.R
plot_overlay_preview_base <- eventReactive(
  input$overlay_preview_base_execute,
  {
    shinyjs::show("overlay_preview_base", time = 0)

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

    model.toplot <- overlay_preview_base_model()

    leaf.map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Topo") %>%
      addPolygons(
        data = st_geometry(model.toplot),
        stroke = TRUE, color = "black", fillColor = "lightskyblue",
        fillOpacity = 1, group = "Base map") %>%
      mapview::addMouseCoordinates()
    overlay.groups <- c("Base map")

    if (l.inc) {
      land.toplot <- overlay_preview_base_land()
      leaf.map <- leaf.map %>%
        addPolygons(
          data = land.toplot, fillColor = "tan",
          stroke = FALSE, fillOpacity = 0.7, group = "Land")
      overlay.groups <- c(overlay.groups, "Land")
    }
    if (b.inc) {
      # boundary.toplot <- vals$overlay.bound
      leaf.map <- leaf.map %>%
        addPolygons(
          data = vals$overlay.bound, fillColor = "transparent",
          stroke = TRUE, color = "red", group = "Study area")
      overlay.groups <- c(overlay.groups, "Study area")
    }

    leaf.map <- leaf.map %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI Topo"),
        overlayGroups = overlay.groups,
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      )

    leaf.map
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

    plot.multi.download(overlaid.pix.list) # because it is a square shape
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
  ensemble.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  validate(
    need(length(ensemble.idx) > 0,
         "Error: Please select at least one model from table to preview")
  )

  models.toplot <- create_ens_preview_model()

  plot.titles <- sapply(ensemble.idx, function(i) {
    paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
          "|", vals$ensemble.overlaid.idx[i])
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
  ensemble.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  validate(
    need(length(ensemble.idx) > 0,
         "Error: Please select at least one model from table to preview")
  )

  models.toplot <- create_ens_preview_model()

  plot.titles <- sapply(ensemble.idx, function(i) {
    # paste0("Ensembling method: ", vals$ensemble.method[i], "\n",
    #        "Rescaling method: ", vals$ensemble.rescaling[i])
    paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
          "|", vals$ensemble.overlaid.idx[i])
  })

  ens.pix.list <- list(models.toplot = models.toplot, data.name = "Pred.ens",
                       plot.titles = plot.titles, perc.num = perc.num)
  #----------------------------------------------

  plot.multi.download(ens.pix.list)
})

###############################################################################
