### Items that are rendered for Overlay section of App


###############################################################################
### Checkboxes for including boundary or land in preview
output$overlay_preview_options_uiOut_checkGroup <- renderUI ({
  req(table_orig())
  
  choices.list <- NULL
  if(!is.null(vals$overlay.bound)) 
    choices.list <- c(choices.list, "Include boundary polygon in preview" = 1)
  if(!is.null(vals$overlay.land)) 
    choices.list <- c(choices.list, "Include land polygon in preview" = 2)
  
  req(choices.list)
  
  checkboxGroupInput("overlay_preview_options", 
                h5("Display options for preview of overlay base"), 
                choices = choices.list, selected = choices.list)
})


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

###############################################################################
