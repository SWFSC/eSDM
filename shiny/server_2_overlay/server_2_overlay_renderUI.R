### Items that are rendered for Overlay section of App


###############################################################################
### Widget for user to select file with projection to use in overlay
output$overlay_proj_which_uiOut_select <- renderUI({
  req(!input$overlay_proj_ll)

  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names
  
  selectInput("overlay_proj_which", 
              h5("Use projection from selected model predictions in", 
                 "overlay process"), 
              choices = choices.list, 
              selected = 1)
})

