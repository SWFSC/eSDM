# Save and load workspace

###############################################################################
###############################################################################
### Save workspace
# Nothing to validate or return, so we don't need eventReactive
# Potential future todo: save input values
output$save_app_envir <- downloadHandler(
  filename = function() paste0(input$save_app_envir_name, ".RDATA"),

  content = function(file) {
    withProgress(message = "Preparing workspace to be saved", value = 0.3, {
      # Convert reactiveValues
      vals.save <- reactiveValuesToList(vals)

      # Remove models.ll to save space
      vals.save$models.ll <- list()

      # Reset plotting info in vals.save (not vals)
      vals.save$models.plot.leaf <- NULL
      vals.save$models.plot.leaf.idx <- NULL
      vals.save$models.plot <- NULL
      vals.save$models.plot.idx <- NULL
      vals.save$overlay.plot <- NULL
      vals.save$overlaid.plot <- NULL
      vals.save$ens.over.wpoly.plot <- NULL
      vals.save$ensemble.plot.leaf <- NULL
      vals.save$ensemble.plot.leaf.idx <- NULL
      vals.save$ensemble.plot <- NULL
      vals.save$ensemble.plot.idx <- NULL
      vals.save$ensemble.plot.var <- NULL
      vals.save$ensemble.plot.var.idx <- NULL
      vals.save$pretty.plot <- NULL
      incProgress(0.5)

      # Save data
      save(vals.save, file = file)
      incProgress(0.2)
    })
  }
)


###############################################################################
###############################################################################
# Load saved workspace

#------------------------------------------------------------------------------
# Upload file with saved workspace and open modal
observeEvent(input$load_app_envir_file, {
  req(input$load_app_envir_file)

  file.load <- input$load_app_envir_file
  file.load.ext <- substr_right(input$load_app_envir_file$name, 6)
  temp <- toupper(file.load.ext) %in% c(".RDATA") &
    # temp <- file.load.ext %in% c(".RDATA", ".RData", ".rdata") &
    input$load_app_envir_file$type == ""

  if (temp) {
    load(file.load$datapath)
    temp <- exists("vals.save")
  }

  # If either
  if (length(vals$models.ll) > 0 || !temp) {
    showModal(load_workspace_modal(failed = !temp))
  } else {
    val.load(!val.load())
  }
})


#------------------------------------------------------------------------------
# Modal asking if user wants to overwrite current workspace
load_workspace_modal <- function(failed = FALSE) {
  if (failed) {
    modalDialog(
      tags$strong("Error: Please load a file with the extension '.RDATA' or '.RData'.",
                  "This file must contain a workspace saved using the eSDM GUI",
                  style = "color: red;"),
      tags$br(),
      footer = tagList(actionButton("workspace_cancel", "Cancel"))
    )

  } else {
    modalDialog(
      title = "Do you want to save your current workspace before loading a new one?",
      tags$h5("Loading a new workspace will overwrite your current workspace.",
              "If you wish to save your current workspace first, click 'Cancel'.",
              "If you wish to proceed with loading the workspace from the specified",
              "file, click 'Proceed'"),

      footer = tagList(
        actionButton("workspace_cancel", "Cancel"),
        actionButton("workspace_load", "Proceed")
      )
    )
  }
}


#------------------------------------------------------------------------------
# Options from close modal buttons: cancel load or trigger val.load
observeEvent(input$workspace_cancel, {
  removeModal()
  shinyjs::reset("load_app_envir_file")
})

observeEvent(input$workspace_load, {
  removeModal()
  val.load(!val.load())
})


#------------------------------------------------------------------------------
# Ensures workspace is loaded and processed even if user quickly changes tabs
observe(load_envir())

# Load and process saved workspace when val.load is triggered
load_envir <- eventReactive(val.load(), {
  file.load <-  req(input$load_app_envir_file)

  withProgress(message = "Loading saved workspace", value = 0.4, {
    #------------------------------------------------------
    load(file.load$datapath)
    validate(
      need(exists("vals.save"),
           paste0("Error: The loaded .RDATA file does not contain",
                  "a workspace saved using the eSDM GUI"))
    )
    incProgress(0.3)

    #------------------------------------------------------
    vals$models.ll            <- lapply(vals.save[["models.orig"]], st_transform, 4326)
    vals$models.orig          <- vals.save[["models.orig"]]
    vals$models.names         <- vals.save[["models.names"]]
    vals$models.data.names    <- vals.save[["models.data.names"]]
    vals$models.pred.type     <- vals.save[["models.pred.type"]]
    vals$models.specs         <- vals.save[["models.specs"]]
    vals$models.plot.leaf     <- vals.save[["models.plot.leaf"]]
    vals$models.plot.leaf.idx <- vals.save[["models.plot.leaf.idx"]]
    vals$models.plot          <- vals.save[["models.plot"]]
    vals$models.plot.idx      <- vals.save[["models.plot.idx"]]

    vals$overlay.bound    <- vals.save[["overlay.bound"]]
    vals$overlay.land     <- vals.save[["overlay.land"]]
    vals$overlay.plot     <- vals.save[["overlay.plot"]]
    vals$overlay.crs      <- vals.save[["overlay.crs"]]
    vals$overlay.info     <- vals.save[["overlay.info"]]
    vals$overlay.base.sfc <- vals.save[["overlay.base.sfc"]]
    vals$overlaid.models  <- vals.save[["overlaid.models"]]
    vals$overlaid.specs   <- vals.save[["overlaid.specs"]]
    vals$overlaid.plot    <- vals.save[["overlaid.plot"]]

    vals$ens.over.wpoly.filename <- vals.save[["ens.over.wpoly.filename"]]
    vals$ens.over.wpoly.sf       <- vals.save[["ens.over.wpoly.sf"]]
    vals$ens.over.wpoly.coverage <- vals.save[["ens.over.wpoly.coverage"]]
    vals$ens.over.wpoly.plot     <- vals.save[["ens.over.wpoly.plot"]]

    vals$ensemble.models        <- vals.save[["ensemble.models"]]
    vals$ensemble.overlaid.res  <- vals.save[["ensemble.overlaid.res"]]
    vals$ensemble.specs         <- vals.save[["ensemble.specs"]]
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


    #------------------------------------------------------
    # Update several checkboxes based on vals contents
    updateCheckboxInput(session, "overlay_bound",  value = isTruthy(vals$overlay.bound))
    updateCheckboxInput(session, "overlay_land",   value = isTruthy(vals$overlay.land))
    updateCheckboxInput(session, "create_ens_reg", value = any(sapply(vals$ens.over.wpoly.filename, isTruthy)))
    updateCheckboxInput(session, "pretty_addobj",  value = isTruthy(vals$pretty.addobj))

    incProgress(0.2)
  })

  paste("Workspace loaded from", file.load$name)
}, ignoreInit = TRUE)

###############################################################################
