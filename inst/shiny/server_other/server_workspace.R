# Save and load workspace

###############################################################################
### Save data
# There is nothing to validate or return, so we don't need eventReactive
output$save_app_envir <- downloadHandler(
  filename = function() {
    input$save_app_envir_name
  },

  content = function(file) {
    withProgress(message = "Preparing workspace to be saved", value = 0.3, {
      # Reset plot info
      vals$models.plot.leaf <- NULL
      vals$models.plot.leaf.idx <- NULL
      vals$models.plot <- NULL
      vals$models.plot.idx <- NULL
      vals$overlay.plot <- NULL
      vals$overlaid.plot <- NULL
      vals$ens.over.wpoly.plot <- NULL
      vals$ensemble.plot.leaf <- NULL
      vals$ensemble.plot.leaf.idx <- NULL
      vals$ensemble.plot <- NULL
      vals$ensemble.plot.idx <- NULL
      vals$pretty.plot <- NULL

      # Convert reactiveValues to list and save
      vals.save <- reactiveValuesToList(vals)
      incProgress(0.5)

      # Create list of current input values
      inputs.save <- list(
        "model_load_type" = input$model_load_type,
        "model_csv_pt_loc" = input$model_csv_pt_loc
      )

      save(vals.save, file = file)
      incProgress(0.2)
    })
  }
)


###############################################################################
### Load data
observe(load_envir()) # So loading happens even if user left first page

load_envir <- eventReactive(input$load_app_envir_file, {
  req(input$load_app_envir_file)

  file.load <- input$load_app_envir_file
  file.load.ext <- substr_right(input$load_app_envir_file$name, 6)
  validate(
    need((file.load.ext %in% c(".RDATA", ".RData") &
            input$load_app_envir_file$type == ""),
         "Error: Please load a file with the extension '.RDATA' or '.RData'")
  )

  withProgress(message = "Loading saved workspace", value = 0.4, {
    load(file.load$datapath)
    validate(
      need(exists("vals.save"),
           paste0("Error: The loaded .RDATA file does not contain",
                  "a workspace saved using the eSDM GUI"))
    )
    incProgress(0.4)

    vals$models.ll             <- vals.save[["models.ll"]]
    vals$models.orig           <- vals.save[["models.orig"]]
    vals$models.names          <- vals.save[["models.names"]]
    vals$models.data.names     <- vals.save[["models.data.names"]]
    vals$models.pred.type      <- vals.save[["models.pred.type"]]
    vals$models.specs          <- vals.save[["models.specs"]]
    vals$models.plot.leaf      <- vals.save[["models.plot.leaf"]]
    vals$models.plot.leaf.idx  <- vals.save[["models.plot.leaf.idx"]]
    vals$models.plot           <- vals.save[["models.plot"]]
    vals$models.plot.idx       <- vals.save[["models.plot.idx"]]

    vals$overlay.bound         <- vals.save[["overlay.bound"]]
    vals$overlay.land          <- vals.save[["overlay.land"]]
    vals$overlay.plot          <- vals.save[["overlay.plot"]]
    vals$overlay.crs           <- vals.save[["overlay.crs"]]
    vals$overlay.info          <- vals.save[["overlay.info"]]
    vals$overlay.base.sfc      <- vals.save[["overlay.base.sfc"]]
    vals$overlaid.models       <- vals.save[["overlaid.models"]]
    vals$overlaid.models.specs <- vals.save[["overlaid.models.specs"]]
    vals$overlaid.plot         <- vals.save[["overlaid.plot"]]

    vals$ens.over.wpoly.filename <- vals.save[["ens.over.wpoly.filename"]]
    vals$ens.over.wpoly.sf       <- vals.save[["ens.over.wpoly.sf"]]
    vals$ens.over.wpoly.coverage <- vals.save[["ens.over.wpoly.coverage"]]
    vals$ens.over.wpoly.plot     <- vals.save[["ens.over.wpoly.plot"]]

    vals$ensemble.models        <- vals.save[["ensemble.models"]]
    vals$ensemble.method        <- vals.save[["ensemble.method"]]
    vals$ensemble.weights       <- vals.save[["ensemble.weights"]]
    vals$ensemble.rescaling     <- vals.save[["ensemble.rescaling"]]
    vals$ensemble.overlaid.idx  <- vals.save[["ensemble.overlaid.idx"]]
    vals$ensemble.plot.leaf     <- vals.save[["ensemble.plot.leaf"]]
    vals$ensemble.plot.leaf.idx <- vals.save[["ensemble.plot.leaf.idx"]]
    vals$ensemble.plot          <- vals.save[["ensemble.plot"]]
    vals$ensemble.plot.idx      <- vals.save[["ensemble.plot.idx"]]

    vals$eval.data          <- vals.save[["eval.data"]]
    vals$eval.data.specs    <- vals.save[["eval.data.specs"]]
    vals$eval.data.gis.info <- vals.save[["eval.data.gis.info"]]
    vals$eval.models.idx    <- vals.save[["eval.models.idx"]]
    vals$eval.metrics       <- vals.save[["eval.metrics"]]
    vals$eval.metrics.names <- vals.save[["eval.metrics.names"]]

    vals$pretty.addobj        <- vals.save[["pretty.addobj"]]
    vals$pretty.params.toplot <- vals.save[["pretty.params.toplot"]]
    vals$pretty.toplot.idx    <- vals.save[["pretty.toplot.idx"]]
    vals$pretty.plot          <- vals.save[["pretty.plot"]]

    incProgress(0.1)

    # Update variable defaults if necessary
    if (isTruthy(vals$overlay.bound)) {
      updateCheckboxInput(session, "overlay_bound", value = TRUE)
    } else {
      updateCheckboxInput(session, "overlay_bound", value = FALSE)
    }

    if (isTruthy(vals$overlay.land)) {
      updateCheckboxInput(session, "overlay_land", value = TRUE)
    } else {
      updateCheckboxInput(session, "overlay_land", value = FALSE)
    }

    if (isTruthy(vals$pretty.addobj)) {
      updateCheckboxInput(session, "pretty_addobj", value = TRUE)
    } else {
      updateCheckboxInput(session, "pretty_addobj", value = FALSE)
    }

    # TODO Update all widgets

    incProgress(0.1)
  })

  paste("Workspace loaded from", file.load$name)
})

###############################################################################
