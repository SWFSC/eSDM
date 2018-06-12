###############################################################################
# Add additional poly information to reactive values
observeEvent(input$pretty_plot_addobj_execute, {
  vals$pretty.addobj.list <- c(
    vals$pretty.addobj.list,
    list(list(
      obj = switch(
        as.numeric(input$pretty_plot_other_obj_which),
        vals$overlay.bound, vals$overlay.land, vals$eval.data
      ),
      obj.text = as.numeric(input$pretty_plot_other_obj_which),
      col = input$pretty_plot_addobj_color,
      cex = input$pretty_plot_addobj_cex,
      pre.sdm = input$pretty_plot_addobj_order == 1
    ))
  )
})


# Display table for 'added' additional polygons
pretty_plot_addobj_table <- reactive({
  validate(
    need(vals$pretty.addobj.list,
         "No additional objects have been loaded"),
    errorClass = "validation2"
  )

  x <- data.frame(do.call(
    rbind,
    lapply(vals$pretty.addobj.list, function(i) {
      c(
        switch(
          i$obj.text,
          "Study area polygon", "Land polygon", "Validation data points"
        ),
        unlist(i[3:5], use.names = FALSE)
      )
    })
  ), stringsAsFactors = FALSE)

  purrr::set_names(x, c("Object", "Color", "CEX", "Draw before SDM"))
})


observeEvent(input$pretty_plot_addobj, {
  if (!input$pretty_plot_addobj) {
    vals$pretty.addobj.list <- NULL
  }
})

###############################################################################
# renderUI()'s

#----------------------------------------------------------
### Message for if not useable polygons are loaded
output$pretty_plot_addobj_which_uiOut_message <- renderUI({
  validate(
    need(isTruthy(vals$overlay.bound) | isTruthy(vals$overlay.land) |
           isTruthy(vals$eval.data),
         paste("You currently can only include the study area polygon,",
               "land polygon, and validation data points as additional",
               "polygons.",
               "Please load at least one of these to use this section")),
    errorClass = "validation2"
  )

  NULL
})


#----------------------------------------------------------
### Checkbox for including other polygons
# For this beta version: hardcoded for only study area and land polys
output$pretty_plot_addobj_which_uiOut_select <- renderUI({
  bound.poly <- vals$overlay.bound
  land.poly <- vals$overlay.land
  valid.pts <- vals$eval.data

  req(isTruthy(bound.poly) | isTruthy(land.poly) | isTruthy(valid.pts))

  choices.list <- list()
  if (isTruthy(bound.poly)) {
    choices.list <- c(choices.list, "Study area polygon" = 1)
  }
  if (isTruthy(land.poly)) {
    choices.list <- c(choices.list, "Land polygon" = 2)
  }
  if (isTruthy(valid.pts)) {
    choices.list <- c(choices.list, "Validation data" = 3)
  }

  selectInput("pretty_plot_other_obj_which", tags$h5("Add polygon to map"),
              choices = choices.list, selected = NULL)
})


#----------------------------------------------------------
### Color of additional object
output$pretty_plot_addobj_color_uiOut_colour <- renderUI({
  req(input$pretty_plot_other_obj_which)

  input.lab <- switch(
    as.numeric(input$pretty_plot_other_obj_which),
    "Color of study area border", "Color of land",
    "Color of all validation points"
  )

  input.default <- switch(
    as.numeric(input$pretty_plot_other_obj_which), "red", "tan", "black"
  )

  colourpicker::colourInput(
    "pretty_plot_addobj_color", tags$h5(input.lab),
    showColour = "background", value = input.default
  )
})


#----------------------------------------------------------
### Cex of additional object
output$pretty_plot_addobj_cex_uiOut_numeric <- renderUI({
  req(input$pretty_plot_other_obj_which)

  input.lab <- switch(
    as.numeric(input$pretty_plot_other_obj_which),
    "Line width of study area border", "Line width of land border",
    "Size of all validation points"
  )

  input.default <- switch(
    as.numeric(input$pretty_plot_other_obj_which), 1.5, 0.3, 1
  )

  numericInput("pretty_plot_addobj_cex", tags$h5(input.lab),
               value = input.default, step = 0.1)
})


#----------------------------------------------------------
### Draw order of additional object
output$pretty_plot_addobj_order_uiOut_radio <- renderUI({
  req(input$pretty_plot_other_obj_which)

  radioButtons("pretty_plot_addobj_order",
               tags$h5("Draw object before or after SDM"),
               choices = list("Before" = 1, "After" = 2), selected = 2)
})


#----------------------------------------------------------
### Button to 'add/load' additional polygons (objects)
output$pretty_plot_addobj_execute_uiOut_button <- renderUI({
  req(input$pretty_plot_other_obj_which)

  actionButton("pretty_plot_addobj_execute", "Add additional object")
})

###############################################################################
