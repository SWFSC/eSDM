# Save and load workspace

###############################################################################
###############################################################################
### Save data
# There is nothing to validate or return, so we don't need eventReactive
output$save_app_envir <- downloadHandler(
  filename = function() input$save_app_envir_name,

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
      # inputs.save <- list(
      #   # "model_load_type" = input$model_load_type,
      #   # "model_csv_pt_loc" = input$model_csv_pt_loc
      #   "pretty_table_orig_out_rows_selected" = input$pretty_table_orig_out_rows_selected,
      #   "pretty_table_over_out_rows_selected" = input$pretty_table_over_out_rows_selected,
      #   "pretty_table_ens_out_rows_selected" = input$pretty_table_ens_out_rows_selected,
      #   "pretty_proj_ll" = input$pretty_proj_ll,
      #   "pretty_proj_method" = input$pretty_proj_method,
      #   "pretty_proj_idx" = input$pretty_proj_idx,
      #   "pretty_proj_epsg" = input$pretty_proj_epsg,
      #   "pretty_range_xmin" = input$pretty_range_xmin,
      #   "pretty_range_xmax" = input$pretty_range_xmax,
      #   "pretty_range_ymin" = input$pretty_range_ymin,
      #   "pretty_range_ymax" = input$pretty_range_ymax
      # )

      # save(vals.save, inputs.save, file = file)
      save(vals.save, file = file)
      incProgress(0.2)
    })
  }
)


###############################################################################
###############################################################################
# Load data

#------------------------------------------------------------------------------
observeEvent(input$tabs, val.tabs(TRUE), ignoreInit = TRUE)

observeEvent(input$workspace_load, {
  removeModal()
  val.load(!val.load())
})


#------------------------------------------------------------------------------
observeEvent(input$load_app_envir_file, {
  req(input$load_app_envir_file)

  file.load <- input$load_app_envir_file
  file.load.ext <- substr_right(input$load_app_envir_file$name, 6)
  temp <- file.load.ext %in% c(".RDATA", ".RData") &
    input$load_app_envir_file$type == ""

  if (val.tabs() | !temp) {
    showModal(load_workspace_modal(failed = !temp))
  } else {
    val.load(!val.load())
  }
})


#------------------------------------------------------------------------------
load_workspace_modal <- function(failed = FALSE) {
  if (failed) {
    modalDialog(
      tags$strong("Error: Please load a file with the extension '.RDATA' or '.RData'",
                  "and ensure that this file contains a workspace saved using the eSDM GUI",
                  style = "color: red;"),
      tags$br(),
      footer = tagList(modalButton("Cancel"))
    )

  } else {
    modalDialog(
      title = "Do you want to save your current workspace before loading a new one?",
      tags$h5("Loading a new workspace will overwrite your current workspace.",
              "If you wish to save your current workspace first, click 'Cancel'.",
              "If you wish to proceed with loading the workspace from the specified",
              "file, click 'Proceed'"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("workspace_load", "Proceed")
      )
    )
  }
}


#------------------------------------------------------------------------------
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

    # browser()
    # val.workspace(inputs.save)

    #------------------------------------------------------
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


    #------------------------------------------------------
    # Update several checkboxes based on vals contents
    updateCheckboxInput(session, "overlay_bound",  value = isTruthy(vals$overlay.bound))
    updateCheckboxInput(session, "overlay_land",   value = isTruthy(vals$overlay.land))
    updateCheckboxInput(session, "create_ens_reg", value = any(sapply(vals$ens.over.wpoly.filename, isTruthy)))
    updateCheckboxInput(session, "pretty_addobj",  value = isTruthy(vals$pretty.addobj))

    incProgress(0.1)


    #------------------------------------------------------
    # TODO Non-reactive widgets
    # updateCheckboxInput(session, "pretty_proj_ll", value = )
    incProgress(0.1)
  })

  paste("Workspace loaded from", file.load$name)
}, ignoreInit = TRUE)

observe(load_envir())

###############################################################################
