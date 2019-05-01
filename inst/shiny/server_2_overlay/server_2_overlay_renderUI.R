# Items that are rendered for Overlay Predictions tab

###############################################################################
### Widget for user to select file with projection to use in overlay
output$overlay_proj_sdm_uiOut_select <- renderUI({
  req(!input$overlay_proj_native, input$overlay_proj_method == 2)

  choices.list.names <- as.list(vals$models.names)
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("overlay_proj_sdm",
              tags$h5("Perform the overlay in the coordinate system of the",
                      "selected original predictions"),
              choices = choices.list, selected = 1)
})


###############################################################################
### Widget (button) to preview base geometry with erasing and study area polys
output$overlay_preview_base_execute_uiOut_button <- renderUI({
  validate(
    need(isTruthy(input$overlay_loaded_table_rows_selected),
         "Select a set of original predictions to use as the base geometry"),
    errorClass = "validation2"
  )

  validate(
    if (input$overlay_bound)
      need(isTruthy(vals$overlay.bound),
           paste("Either uncheck the 'study area polygon' checkbox or",
                 "import a study area polygon to preview the base geometry")),
    if (input$overlay_land)
      need(isTruthy(vals$overlay.land),
           paste("Either uncheck the 'erasing polygon' checkbox or",
                 "import an erasing polygon to preview the base geometry")),
    errorClass = "validation2"
  )

  actionButton("overlay_preview_base_execute", "Preview base geometry")
})


###############################################################################
### Widgets for user to select overlaid predictions and value type to plot
output$overlay_preview_overlaid_models_uiOut_selectize <- renderUI({
  validate(
    need(length(vals$overlaid.models) > 0,
         "Create overlaid predictions to use this section of the GUI"),
    errorClass = "validation2"
  )

  choices.list <- seq_along(vals$overlaid.models)
  names(choices.list) <- paste("Overlaid", choices.list)

  selectizeInput(
    "overlay_preview_overlaid_models",
    tags$h5("Select overlaid predictions to preview; 'Overlaid' numbers",
            "correspond to 'Original' numbers in the table above"),
    choices = choices.list, selected = NULL, multiple = TRUE
  )
})

output$overlay_preview_message <- renderUI({
  validate(
    "Create overlaid predictions to use this section of the GUI",
    errorClass = "validation2"
  )
})

###############################################################################
