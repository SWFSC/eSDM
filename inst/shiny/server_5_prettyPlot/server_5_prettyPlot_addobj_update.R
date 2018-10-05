# Code for modal dialog window for updating params of loaded additional objects

###############################################################################
# Show modal when button is clicked.
observeEvent(input$pretty_addobj_update_show, {
  showModal(addobj_update_modal(
    failed = !isTruthy(input$pretty_addobj_table_out_rows_selected)
  ))
})


###############################################################################
addobj_update_modal <- function(failed) {
  if (failed) {
    modalDialog(
      tags$strong("Error: You must select a row from the 'Loaded additional objects' table",
                  style = "color: red;"),
      tags$br(),
      tags$h5("Click 'Cancel' to close this window and then select a row from the table"),
      footer = tagList(modalButton("Cancel"))
    )

  } else {
    x <- req(input$pretty_addobj_table_out_rows_selected)
    val.pretty.addobj.update(vals$pretty.addobj[[x]])

    modalDialog(
      tags$h4("Additional object parameter update window"),
      tags$h5("Select the parameter you wish to update and change it as desired in the window that appears.",
              "Then click 'Save parameter', and the newly saved parameter will be updated in the table below.",
              "After the table reflects the desired parameter values, click 'Done - save the updated parameters'."),
      tags$br(),
      tags$strong(paste("Polygon:", vals$pretty.addobj[[x]]$obj.text)),
      fluidRow(
        column(6, uiOutput("pretty_addobj_update_which_uiOut_select")),
        column(
          width = 6,
          uiOutput("pretty_addobj_update_thing1_uiOut_mult"),
          uiOutput("pretty_addobj_update_thing2_uiOut_mult")
        )
      ),
      actionButton("pretty_addobj_update_execute", "Save parameter"),
      tags$br(), tags$br(), tags$br(),
      tags$h5("Saved parameters for selected additional object. The color values will be 'NA' if transparent;",
              "otherwise they are displayed as hexadecimals."),
      tableOutput("pretty_addobj_update_table_out"),

      footer = tagList(
        actionButton("pretty_addobj_update_cancel", "Cancel and discard the updated parameters"),
        actionButton("pretty_addobj_update_done", "Done - save the updated parameters")
      )
    )
  }
}


###############################################################################
# Functions used to create info table about additional object
pretty_addobj_update_names_func <- function(addobj.params) {
  lab.col1 <- ifelse(
    addobj.params$obj.which == 3, "Presence point color",
    ifelse(addobj.params$obj.type == 1, "Point color", "Polygon fill color")
  )
  lab.col2 <- ifelse(
    addobj.params$obj.which == 3, "Absence point color",
    ifelse(addobj.params$obj.type == 1, "N/A", "Polygon border color")
  )

  c("Object type", "Object draw order", lab.col1, lab.col2,
    addobj_render_lab(7, NULL, addobj.params$obj.type),
    addobj_render_lab(8, NULL, addobj.params$obj.type))
}

pretty_addobj_update_vals_func <- function(addobj.params) {
  c(
    ifelse(addobj.params$obj.type == 1, "Point(s)", "Polygon(s)"),
    ifelse(addobj.params$obj.order == 1, "Behind SDM", "In front of SDM"),
    addobj.params$col.ptfill, addobj.params$col.absborder,
    ifelse(
      addobj.params$obj.type == 1,
      names(choices.list.pch)[addobj.params$pchlty + 1], #first pch val is 0
      names(choices.list.lty)[addobj.params$pchlty]
    ),
    as.character(addobj.params$cexlwd)
  )
}


###############################################################################
#------------------------------------------------------------------------------
# Get names of parameter widgets for selections and table
pretty_addobj_update_names <- reactive({
  pretty_addobj_update_names_func(req(val.pretty.addobj.update()))
})



#------------------------------------------------------------------------------
# renderUI() for which parameter to update
output$pretty_addobj_update_which_uiOut_select <- renderUI({
  choices.list <- as.list(1:6)
  isolate(names(choices.list) <- req(pretty_addobj_update_names()))

  selectInput("pretty_addobj_update_which",
              tags$h5("Choose parameter to update"),
              choices = choices.list, selected = 1)
})


#------------------------------------------------------------------------------
# renderUI() for 'update' widget #1
output$pretty_addobj_update_thing1_uiOut_mult <- renderUI({
  y <- req(val.pretty.addobj.update())
  z <- req(input$pretty_addobj_update_which)

  if (z == 1) {
    #-------------------------------------------------
    tags$h5("You cannot change the object type of an additional object",
            style = "color: red;")

    #-------------------------------------------------
  } else if (z == 2) {
    val.curr <- y$obj.order
    radioButtons("pretty_addobj_update_thing1", tags$h5("Object draw order:"),
                 choices = list("Draw object behind SDM" = 1,
                                "Draw object in front of SDM" = 2),
                 selected = val.curr)
    #-------------------------------------------------
  } else if (z == 3) {
    val.curr <- is.na(y$col.ptfill)
    input.lab <- addobj_render_lab(3, y$obj.which, y$obj.type)
    checkboxInput("pretty_addobj_update_thing1", input.lab, value = val.curr)

    #-------------------------------------------------
  }  else if (z == 4) {
    if (y$obj.which == 4 & y$obj.type == 1) {
      tags$h5("This parameter does not apply when the additional object is a new object",
              "of type 'Point(s)'", style = "color: red;")

    } else {
      val.curr <- is.na(y$col.absborder)
      input.lab <- addobj_render_lab(5, y$obj.which, y$obj.type)
      checkboxInput("pretty_addobj_update_thing1", input.lab, value = val.curr)
    }

    #-------------------------------------------------
  } else if (z == 5) {
    val.curr <- y$pchlty
    input.lab <- addobj_render_lab(7, NULL, y$obj.type)

    # Set list of choices based on object type
    if (y$obj.type == 1) {
      choices.list <- choices.list.pch
    } else {
      choices.list <- choices.list.lty
    }

    selectizeInput("pretty_addobj_update_thing1", tags$h5(input.lab),
                   choices = choices.list, selected = val.curr,
                   multiple = FALSE)

    #-------------------------------------------------
  } else if (z == 6) {
    val.curr <- y$cexlwd
    input.lab <- addobj_render_lab(8, NULL, y$obj.type)

    numericInput("pretty_addobj_update_thing1", tags$h5(input.lab),
                 value = val.curr, step = 0.1)

    #-------------------------------------------------
  } else {
    validate(need(FALSE, "update modal error"))
  }
})


#------------------------------------------------------------------------------
# renderUI() for 'update' widget #2: colourInput's for when not transparent
output$pretty_addobj_update_thing2_uiOut_mult <- renderUI({
  y <- req(val.pretty.addobj.update())
  z <- req(input$pretty_addobj_update_which)

  req(z %in% 3:4, is.logical(input$pretty_addobj_update_thing1))
  req(!input$pretty_addobj_update_thing1)

  if (z == 3) {
    val.curr <- y$col.ptfill
    input.lab <- addobj_render_lab(4, y$obj.which, y$obj.type)

    colourpicker::colourInput(
      "pretty_addobj_update_thing2", tags$h5(input.lab),
      showColour = "background", value = val.curr
    )

    #-------------------------------------------------
  } else if (z == 4) {
    val.curr <- y$col.absborder
    input.lab <- addobj_render_lab(6, y$obj.which, y$obj.type)

    colourpicker::colourInput(
      "pretty_addobj_update_thing2", tags$h5(input.lab),
      showColour = "background", value = val.curr
    )
  }
})


###############################################################################
output$pretty_addobj_update_table_out <- renderTable({
  y <- req(val.pretty.addobj.update())

  data.frame(
    "Name" =  pretty_addobj_update_names(),
    "Value" = pretty_addobj_update_vals_func(y),
    stringsAsFactors = FALSE
  )
})


###############################################################################
observeEvent(input$pretty_addobj_update_execute, {
  y <- val.pretty.addobj.update()
  z <- req(input$pretty_addobj_update_which)

  if (z == 2) {
    y$obj.order <- input$pretty_addobj_update_thing1

  } else if (z == 3) {
    if (input$pretty_addobj_update_thing1) {
      y$col.ptfill <- NA
    } else {
      y$col.ptfill <- input$pretty_addobj_update_thing2
    }

  } else if (z == 4) {
    if (input$pretty_addobj_update_thing1) {
      y$col.absborder <- NA
    } else {
      y$col.absborder <- input$pretty_addobj_update_thing2
    }

  } else if (z == 5) {
    y$pchlty <- as.numeric(input$pretty_addobj_update_thing1)

  } else if (z == 6) {
    y$cexlwd <- req(input$pretty_addobj_update_thing1)

  } else {
    validate("Error updating additional object - please report an issue")
  }

  val.pretty.addobj.update(y)
})


###############################################################################
### Cancel and discard updated parameters
observeEvent(input$pretty_addobj_update_cancel, {
  removeModal()

  val.pretty.addobj.update(NULL)
})


### Done - save updated parameters
observeEvent(input$pretty_addobj_update_done, {
  removeModal()

  x <- input$pretty_addobj_table_out_rows_selected

  vals$pretty.addobj[[x]] <- val.pretty.addobj.update()
  val.pretty.addobj.update(NULL)
})

###############################################################################
