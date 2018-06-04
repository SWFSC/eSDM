### Items that are rendered for Overlay section of App


### Widget for user to select file with projection to use in overlay
output$overlay_proj_which_uiOut_select <- renderUI({
  req(!input$overlay_proj_native)

  choices.list.names <- as.list(vals$models.names)
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("overlay_proj_which",
              tags$h5("Use the coordinate system of the selected model",
                      "predictions during the overlay process"),
              choices = choices.list, selected = 1)
})

###
output$overlay_preview_base_execute_uiOut_button <- renderUI({
  req(isTruthy(input$overlay_loaded_table_rows_selected))

  validate(
    if (input$overlay_bound)
      need(isTruthy(vals$overlay.bound),
           paste("Please either uncheck the boundary box or",
                 "load a boundary polygon to preview the base grid")),
    if (input$overlay_land)
      need(isTruthy(vals$overlay.land),
           paste("Please either uncheck the land box or load a land polygon",
                 "to preview the base grid"))
  )

  actionButton("overlay_preview_base_execute", "Preview base grid")
})

### Widget for user to select overlaid model(s) to plot
output$overlay_preview_overlaid_models_uiOut_selectize <- renderUI({
  req(length(vals$overlaid.models) > 0)

  choices.list <- seq_along(vals$overlaid.models)
  names(choices.list) <- paste("Overlaid", choices.list)

  selectizeInput("overlay_preview_overlaid_models",
                 tags$h5("Overlaid model predictions to preview.",
                         "'Overlaid' numbers correspond to 'Original' numbers",
                         "in the table above"),
                 choices = choices.list, selected = NULL,
                 multiple = TRUE)
})


