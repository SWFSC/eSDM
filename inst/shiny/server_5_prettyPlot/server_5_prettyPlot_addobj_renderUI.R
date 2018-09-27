# Code relating to renderUI()'s for add object section of pretty plot

###############################################################################
# Helper function: generates labels for widgets given various parameters
# "Object type" = 1, "Draw object before or after SDM" = 2,
# "Make polygon fill color transparent" = 3, "Polygon fill color" = 4,
# "Make polygon border(s) transparent" = 5, "Polygon border color" = 6,
# "Line type of polygon border(s)" = 7,
# "Line width of polygon border(s)" = 8
addobj_render_lab <- function(ui.which, addobj.which, addobj.type = NULL) {
  #--------------------------------------------------------
  if (ui.which == 3) {
    if (addobj.which == 3) {
      "Make presence points transparent"
    } else {
      ifelse(
        addobj.type == 1,
        "Make points transparent", "Make polygon fill color transparent"
      )
    }

    #------------------------------------------------------
  } else if (ui.which == 4) {
    if (addobj.which == 3) {
      "Click to select presence point color"
    } else {
      ifelse(
        addobj.type == 1,
        "Click to select point color", "Click to select polygon fill color"
      )
    }

    #------------------------------------------------------
  } else if (ui.which == 5) {
    ifelse(
      addobj.which == 3,
      "Make absence points transparent", "Make polygon borders transparent"
    )

    #------------------------------------------------------
  } else if (ui.which == 6) {
    ifelse(
      addobj.which == 3,
      "Click to select absence point color",
      "Click to select polygon border color"
    )

    #------------------------------------------------------
  } else if (ui.which == 7) {
    ifelse(addobj.type == 1, "Point type", "Line type of polygon borders")

    #------------------------------------------------------
  } else if (ui.which == 8) {
    ifelse(addobj.type == 1, "Point size", "Line width of polygon borders")

    #------------------------------------------------------
  } else {
    validate(need(FALSE, "addobj_render_lab() input issue"))
  }
}

###############################################################################
# renderUI()'s

#------------------------------------------------------------------------------
### Button widget to remove additional polygon
output$pretty_addobj_update_show_uiOut_button <- renderUI({
  req(vals$pretty.addobj)
  actionButton("pretty_addobj_update_show",
               "Update parameters of selected object")
})


#------------------------------------------------------------------------------
### Button widget to remove additional polygon
output$pretty_addobj_remove_execute_uiOut_button <- renderUI({
  req(vals$pretty.addobj)
  input.lab <- "Remove selected object"
  actionButton("pretty_addobj_remove_execute", input.lab)
})


#------------------------------------------------------------------------------
### 0: Select widget for including other polygons
output$pretty_addobj_which_uiOut_select <- renderUI({
  choices.list <- list(
    "Study area polygon (previously loaded in 'Overlay' tab)" = 1,
    "Erasing polygon (previously loaded in 'Overlay' tab)" = 2,
    "Validation points (previously loaded in 'Evaluation Metrics' tab)" = 3
  )
  choices.list.bool <- sapply(
    list(vals$overlay.bound, vals$overlay.land, vals$eval.data), isTruthy
  )
  choices.list <- c(choices.list[choices.list.bool], "Upload new object" = 4)

  input.lab <- tags$h5(
    tags$strong("Additional object source"),
    "Either select previously loaded object or upload new object.",
    "Then, specify the desired plot parameters and load the additional object."
  )

  selectInput("pretty_addobj_which", input.lab,
              choices = choices.list, selected = NULL)
})

#------------------------------------------------------------------------------
### 1: Object type
output$pretty_addobj_type_uiOut_radio <- renderUI({
  req(input$pretty_addobj_which)

  if (input$pretty_addobj_which == 4) {
    choices.list <- list("Point(s)" = 1, "Polygon(s)" = 2)
    choices.selected <- 1

  } else if (input$pretty_addobj_which == 3) {
    choices.list <- list("Point(s)" = 1)
    choices.selected <- 1

  } else { #polygons
    choices.list <- list("Polygon(s)" = 2)
    choices.selected <- 2
  }

  radioButtons("pretty_addobj_type", tags$h5("Object type:"),
               choices = choices.list, selected = choices.selected)
})

#------------------------------------------------------------------------------
### 3: Point or fill color transparent checkbox
output$pretty_addobj_color_ptfillcheck_uiOut_check <- renderUI({
  req(input$pretty_addobj_which, input$pretty_addobj_type)

  input.lab <- addobj_render_lab(
    3, input$pretty_addobj_which, input$pretty_addobj_type
  )
  input.default <- ifelse(input$pretty_addobj_which == 1, TRUE, FALSE)

  checkboxInput("pretty_addobj_color_ptfillcheck", input.lab, value = input.default)
})

#----------------------------------------------------------
### 4: Point or fill color; reset to default when (3) is checked
output$pretty_addobj_color_ptfill_uiOut_colour <- renderUI({
  req(input$pretty_addobj_color_ptfillcheck == FALSE)

  input.lab <- addobj_render_lab(
    4, input$pretty_addobj_which, input$pretty_addobj_type
  )
  input.default <- switch(
    as.numeric(input$pretty_addobj_which), "red", "tan", "blue", "black"
  )

  colourpicker::colourInput(
    "pretty_addobj_color_ptfill", tags$h5(input.lab),
    showColour = "background", value = input.default
  )
})


#----------------------------------------------------------
#----------------------------------------------------------
### 5: Validation absence point or border color transparent checkbox
output$pretty_addobj_color_absbordercheck_uiOut_check <- renderUI({
  req(input$pretty_addobj_which, input$pretty_addobj_type)

  if (input$pretty_addobj_which == 4 & input$pretty_addobj_type == 1) {
    NULL

  } else {
    input.lab <- addobj_render_lab(5, input$pretty_addobj_which)

    checkboxInput("pretty_addobj_color_absbordercheck", input.lab, value = FALSE)
  }
})

#----------------------------------------------------------
### 6: Validation absence point or border color; reset to default when (5) is checked
output$pretty_addobj_color_absborder_uiOut_colour <- renderUI({
  req(input$pretty_addobj_color_absbordercheck == FALSE)

  if (input$pretty_addobj_which == 4 & input$pretty_addobj_type == 1) {
    NULL

  } else {
    input.lab <- addobj_render_lab(6, input$pretty_addobj_which)
    input.default <- switch(
      as.numeric(input$pretty_addobj_which),
      "red", "black", "red", "black"
    )

    colourpicker::colourInput(
      "pretty_addobj_color_absborder", tags$h5(input.lab),
      showColour = "background", value = input.default
    )
  }
})


#------------------------------------------------------------------------------
### 7: Point type / line type
output$pretty_addobj_pchlty_uiOut_select <- renderUI({
  req(input$pretty_addobj_which, input$pretty_addobj_type)

  input.lab <- addobj_render_lab(7, NULL, input$pretty_addobj_type)

  # Set list of choices based on object type
  if (input$pretty_addobj_type == 1) {
    choices.list <- choices.list.pch
    choices.selected <- 19

  } else {
    choices.list <- choices.list.lty
    choices.selected <- 1
  }

  selectizeInput("pretty_addobj_pchlty", tags$h5(input.lab),
                 choices = choices.list, selected = choices.selected,
                 multiple = FALSE)
})


#------------------------------------------------------------------------------
### 8: Point size / line width
output$pretty_addobj_cexlwd_uiOut_numeric <- renderUI({
  req(input$pretty_addobj_which, input$pretty_addobj_type)

  input.lab <- addobj_render_lab(8, NULL, input$pretty_addobj_type)
  input.default <- switch(
    as.numeric(input$pretty_addobj_which), 1.5, 0.3, 0.5, 1
  )

  numericInput("pretty_addobj_cexlwd", tags$h5(input.lab),
               value = input.default, step = 0.1)
})

###############################################################################
