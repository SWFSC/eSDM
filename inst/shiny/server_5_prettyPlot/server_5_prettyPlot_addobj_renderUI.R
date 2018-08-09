###############################################################################
# renderUI()'s

#------------------------------------------------------------------------------
### Button widget to remove additional polygon
output$pretty_plot_addobj_remove_execute_uiOut_button <- renderUI({
  req(vals$pretty.addobj)

  actionButton("pretty_plot_addobj_remove_execute",
               "Remove selected additional object")
})


#------------------------------------------------------------------------------
### Select widget for including other polygons
output$pretty_plot_addobj_which_uiOut_select <- renderUI({
  choices.list <- list("Import new object" = 4)

  if (isTruthy(vals$eval.data)) {
    choices.list <- c("Validation points" = 3, choices.list)
  }
  if (isTruthy(vals$overlay.land)) {
    choices.list <- c("Erasing polygon" = 2, choices.list)
  }
  if (isTruthy(vals$overlay.bound)) {
    choices.list <- c("Study area polygon" = 1, choices.list)
  }

  selectInput("pretty_plot_addobj_which", tags$h5("Add polygon to map"),
              choices = choices.list, selected = NULL)
})

#------------------------------------------------------------------------------
### Object type
output$pretty_plot_addobj_type_uiOut_radio <- renderUI({
  req(input$pretty_plot_addobj_which)

  if (input$pretty_plot_addobj_which == 4) {
    choices.list <- list("Point(s)" = 1, "Polygon(s)" = 2)
    choices.selected <- 1

  } else if (input$pretty_plot_addobj_which == 3) {
    choices.list <- list("Point(s)" = 1)
    choices.selected <- 1

  } else { #polygons
    choices.list <- list("Polygon(s)" = 2)
    choices.selected <- 2
  }

  radioButtons("pretty_plot_addobj_type", tags$h5("Object type:"),
               choices = choices.list, selected = choices.selected)
})

#------------------------------------------------------------------------------
### Point or fill color transparent checkbox
output$pretty_plot_addobj_color_ptfillcheck_uiOut_check <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  if (input$pretty_plot_addobj_which == 3) {
    input.lab <- "Make absence points transparent"

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_type == 1,
      "Make points transparent",
      "Make polygon fill color transparent"
    )
  }

  checkboxInput("pretty_plot_addobj_color_ptfillcheck", input.lab, value = FALSE)
})

#----------------------------------------------------------
### Point or fill color
output$pretty_plot_addobj_color_ptfill_uiOut_colour <- renderUI({
  req(input$pretty_plot_addobj_color_ptfillcheck == FALSE)

  if (input$pretty_plot_addobj_which == 3) {
    input.lab <- "Click to select present point color"

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_type == 1,
      "Click to select point color", "Click to select polygon fill color"
    )
  }

  input.default <- switch(
    as.numeric(input$pretty_plot_addobj_which), "red", "tan", "blue", "black"
  )

  colourpicker::colourInput(
    "pretty_plot_addobj_color_ptfill", tags$h5(input.lab),
    showColour = "background", value = input.default
  )
})


#----------------------------------------------------------
#----------------------------------------------------------
### Validation absence point or border color transparent checkbox
output$pretty_plot_addobj_color_absbordercheck_uiOut_check <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  if (input$pretty_plot_addobj_which == 4 & input$pretty_plot_addobj_type == 1) {
    NULL

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_which == 3,
      "Make absence points transparent", "Make polygon border(s) transparent"
    )

    checkboxInput("pretty_plot_addobj_color_absbordercheck", input.lab, value = FALSE)
  }
})

#----------------------------------------------------------
### Validation absence point or border color
output$pretty_plot_addobj_color_absborder_uiOut_colour <- renderUI({
  req(input$pretty_plot_addobj_color_absbordercheck == FALSE)

  if (input$pretty_plot_addobj_which == 4 & input$pretty_plot_addobj_type == 1) {
    NULL

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_which == 3,
      "Click to select absence point color",
      "Click to select polygon border color"
    )


    input.default <- switch(
      as.numeric(input$pretty_plot_addobj_which),
      "red", "black", "red", "black"
    )

    colourpicker::colourInput(
      "pretty_plot_addobj_color_absborder",
      tags$h5(input.lab),
      showColour = "background", value = input.default
    )
  }
})


#------------------------------------------------------------------------------
### Point type / line type
output$pretty_plot_addobj_pchlty_uiOut_select <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  # Set label based on which object
  input.lab <- ifelse(
    input$pretty_plot_addobj_type == 1,
    "Point type", "Line type of polygon border(s)"
  )

  # Set list of choices based on object type
  if (input$pretty_plot_addobj_type == 1) {
    choices.list <- list(
      "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
      "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
      "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
      "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
      "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
      "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
      "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
    )
    choices.selected <- 19

  } else {
    choices.list <- list(
      "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, "Long dash" = 5,
      "Dot-long dash" = 6
    )
    choices.selected <- 1
  }

  selectInput("pretty_plot_addobj_pchlty", tags$h5(input.lab),
              choices = choices.list, selected = choices.selected)
})


#------------------------------------------------------------------------------
### Point size / line width
output$pretty_plot_addobj_cexlwd_uiOut_numeric <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  input.lab <- ifelse(
    input$pretty_plot_addobj_type == 1,
    "Point size", "Line width of polygon border(s)"
  )
  input.default <- switch(
    as.numeric(input$pretty_plot_addobj_which), 1.5, 0.3, 0.5, 1
  )

  numericInput("pretty_plot_addobj_cexlwd", tags$h5(input.lab),
               value = input.default, step = 0.1)
})

###############################################################################
