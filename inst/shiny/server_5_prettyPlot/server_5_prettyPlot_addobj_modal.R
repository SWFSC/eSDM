# Code for modal dialog window for updating params of loaded additional objects

###############################################################################
# Show modal when button is clicked.
observeEvent(input$show, { #TODO change to eventReactive
  showModal(dataModal(
    failed = !isTruthy(input$pretty_plot_addobj_table_out_rows_selected)
  ))
})


###############################################################################
dataModal <- function(failed) {
  if (failed) {
    modalDialog(
      tags$div(
        tags$strong("You must select a row from the 'Loaded additional objects table"),
        tags$br(),
        tags$h5("Click 'Cancel' to close this window and then select a row from the table"),
        style = "color: red;"
      ),
      footer = tagList(modalButton("Cancel"))
    )

  } else {
    x <- input$pretty_plot_addobj_table_out_rows_selected
    val.pretty.addobj.update(vals$pretty.addobj[[x]])

    choices.list <- list(
      "Object type" = 1, "Draw object before or after SDM" = 2,
      "Make polygon fill color transparent" = 3,
      "Polygon fill color" = 4,
      "Make polygon border(s) transparent" = 5,
      "Polygon border color" = 6,
      "Line type of polygon border(s)" = 7,
      "Line width of polygon border(s)" = 8
    )

    modalDialog(
      tags$h5("(Try the name of a valid data object like",
              "then a name of a non-existent object like)"),
      tags$br(),
      tags$strong(paste("Polygon:", vals$pretty.addobj[[x]]$obj.text)),
      fluidRow(
        column(6, selectInput("pretty_plot_addobj_update_which", tags$h5("Choose thing to update"),
                              choices = choices.list, selected = 1)),
        column(6, uiOutput("pretty_plot_addobj_update_thing_uiOut_mult"))
      ),
      actionButton("pretty_plot_addobj_update_execute", "Update stored parameter"),
      tags$br(), tags$br(),
      tags$h5("Stored parameters for selected additional object"),

      tableOutput("pretty_plot_addobj_update_table_out"),

      footer = tagList(actionButton("pretty_plot_addobj_update_done", "Done"))
    )
  }
}


###############################################################################
output$pretty_plot_addobj_update_thing_uiOut_mult <- renderUI({
  y <- val.pretty.addobj.update()
  z <- req(input$pretty_plot_addobj_update_which)


  if (z == 1) {
    #----------------------------------------
    if (y$obj.text %in% c("Study area polygon", "Erasing polygon", "Validation data points")) {
      helpText("You cannot change the object type for a", y$obj.text)

    } else {
      radioButtons("pretty_plot_addobj_update_thing", tags$h5("Object type:"),
                   choices = list("Point(s)" = 1, "Polygon(s)" = 2),
                   selected = y$obj.type)
    }

    #----------------------------------------
  } else if (z == 2) {
    val.curr <- ifelse(y$pre.sdm, 1, 2)
    radioButtons("pretty_plot_addobj_update_thing", tags$h5("Draw object before or after SDM:"),
                 choices = list("Before" = 1, "After" = 2), selected = val.curr)

    #----------------------------------------
  } else if (z == 3) {
    val.curr <- is.na(y$col.ptfill)
    input.lab <- ifelse(
      y$obj_type == 1,
      "Make points transparent",
      "Make polygon fill color transparent"
    )
    checkboxInput("pretty_plot_addobj_update_thing", input.lab, value = val.curr)

    #----------------------------------------
  } else if (z == 4) {
    if (is.na(y$col.ptfill)) {
      helpText("You must uncheck the checkbox ")
    } else {

    }

    #----------------------------------------
  } else if (z == 5) {
    # if (input$pretty_plot_addobj_which == 4 & input$pretty_plot_addobj_type == 1) {
    #   NULL
    #
    # } else {
    #   input.lab <- ifelse(
    #     input$pretty_plot_addobj_which == 3,
    #     "Make absence points transparent", "Make polygon border(s) transparent"
    #   )
    #
    #   checkboxInput("pretty_plot_addobj_color_absbordercheck", input.lab, value = FALSE)
    # }
    #
    #
    #
    # val.curr <- is.na(y$col.ptfill)
    # input.lab <- ifelse(
    #   y$obj_type == 1,
    #   "Make points transparent",
    #   "Make polygon fill color transparent"
    # )
    # checkboxInput("pretty_plot_addobj_color_ptfillcheck", input.lab, value = val.curr)

    #----------------------------------------
  } else if (z == 6) {

    #----------------------------------------
  } else if (z == 7) {

    #----------------------------------------
  } else if (z == 8) {

    #----------------------------------------
  } else {
    helpText("Didn't get anything")
  }
})


###############################################################################
output$pretty_plot_addobj_update_table_out <- renderTable({
  x <- req(input$pretty_plot_addobj_table_out_rows_selected)
  y <- req(val.pretty.addobj.update())

  params.names <- names(list(
    "Object type" = 1, "Draw object before or after SDM" = 2,
    "Make polygon fill color transparent" = 3,
    "Polygon fill color" = 4,
    "Make polygon border(s) transparent" = 5,
    "Polygon border color" = 6,
    "Line type of polygon border(s)" = 7,
    "Line width of polygon border(s)" = 8
  ))

  names.pchlty1 <- names(list(
    "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
    "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
    "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
    "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
    "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
    "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
    "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
  ))
  names.pchlty2 <- names(list(
    "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, "Long dash" = 5,
    "Dot-long dash" = 6
  ))


  params.vals <- c(
    ifelse(y$obj.type == 1, "Point(s)", "Polygon(s)"),
    ifelse(y$pre.sdm, "Before", "After"),
    as.character(is.na(y$col.ptfill)),
    as.character(y$col.ptfill),
    as.character(is.na(y$col.absborder)),
    as.character(y$col.absborder),
    ifelse(y$obj.type == 1, names.pchlty1[y$pchlty], names.pchlty2[y$pchlty]),
    as.character(y$cexlwd)
  )

  data.frame(
    "Name" = params.names, "Value" = params.vals,
    stringsAsFactors = FALSE
  )
})


###############################################################################
observeEvent(input$pretty_plot_addobj_update_execute, {
  z <- req(input$pretty_plot_addobj_update_which)
  y <- val.pretty.addobj.update()

  if (z == 1) {


  } else if (z == 2) {
    y$pre.sdm <- input$pretty_plot_addobj_update_thing == 1


  } else {

  }

  val.pretty.addobj.update(y)
})


###############################################################################
observeEvent(input$pretty_plot_addobj_update_done, {
  removeModal()

  z <- req(input$pretty_plot_addobj_update_which)
  x <- input$pretty_plot_addobj_table_out_rows_selected

  vals$pretty.addobj[[x]] <- val.pretty.addobj.update()
  val.pretty.addobj.update(NULL)
})

###############################################################################
