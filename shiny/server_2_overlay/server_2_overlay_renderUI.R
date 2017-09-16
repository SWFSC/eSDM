### Items that are rendered for Overlay section of App


###############################################################################
### Widget for user to select file with projection to use in overlay
output$overlay_proj_which_uiOut_select <- renderUI({
  req(!input$overlay_proj_ll)
  
  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names
  
  selectInput("overlay_proj_which", 
              h5("Use the selected model predictions' projection", 
                 "during the overlay process"), 
              choices = choices.list, selected = 1)
})


### Widget for user to select overlaid model(s) to plot
output$overlay_preview_overlaid_models_uiOut_selectize <- renderUI({
  req(length(vals$overlaid.models) != 0)
  # browser()
  
  choices.list <- seq_along(vals$overlaid.models)
  names(choices.list) <- paste("Overlaid", choices.list)
  
  selectizeInput("overlay_preview_overlaid_models", 
                 h5("Overlaid model predictions to preview.", 
                    "'Overlaid' numbers correspond to 'Original' numbers", 
                    "in the table above"), 
                 choices = choices.list, selected = NULL, 
                 multiple = TRUE)
})
###############################################################################
