# Code for modal dialog window for updating params of saved maps

###############################################################################
# Show modal when button is clicked.
observeEvent(input$pretty_update_toplot_show, {
  showModal(toplot_update_modal(
    failed = !isTruthy(input$pretty_update_table_out_rows_selected)
  ))
})


###############################################################################
toplot_update_modal <- function(failed) {
  if (failed) {
    modalDialog(
      tags$strong("Error: You must select a saved map to update",
                  style = "color: red;"),
      tags$br(),
      tags$h5("Click 'Cancel' to close this window and then select a row from the saved map table"),
      footer = tagList(modalButton("Cancel"))
    )

  } else {
    x <- req(input$pretty_update_table_out_rows_selected)
    val.pretty.toplot.update(vals$pretty.params.toplot[[x]])

    choices.list.main <- list(
      "Map coordinate system and range" = 1, "Background color and prediction color scheme" = 2,
      "Legend" = 3, "Title and axis labels" = 4, "Coordinate grid lines and labels" = 5,
      "Additional objects (points or polygons)" = 6, "Map ID" = 7
    )

    modalDialog(
      tags$h4("Saved map parameter update window"),
      tags$h5("Select the parameter you wish to update and change it as desired in the window that appears.",
              "Then click 'Save parameter', and the newly saved parameter will be updated in the table below.",
              "After the table reflects the desired parameter values, click 'Done'."),
      tags$h5("Note that you cannot update the map coordinate system or prediction color scheme of a saved map;",
              "to change these parameters you must create a new map."),
      tags$br(),
      tags$strong(paste("Map ID:", vals$pretty.params.toplot[[x]]$id)),
      fluidRow(
        column(6, selectInput("pretty_toplot_update_which", tags$h5("Choose parameter section"),
                              choices = choices.list.main, selected = 1)),
        column(6, uiOutput("pretty_toplot_update_which_param_uiOut_select"))
      ),
      box(
        width = 12,
        fluidRow(
          column(6, uiOutput("pretty_toplot_update_thing1_uiOut_mult")),
          column(6, uiOutput("pretty_toplot_update_thing2_uiOut_mult"))
        )
      ),
      actionButton("pretty_toplot_update_execute", "Save parameter"),
      uiOutput("pretty_toplot_update_temp_out_text"),
      tags$br(), tags$br(), tags$br(),
      tags$h5("Saved parameters for selected additional object. The color values will be 'NA' if transparent;",
              "otherwise they are displayed as hexadecimals."),
      tableOutput("pretty_toplot_update_table_out"),

      footer = tagList(actionButton("pretty_toplot_update_done", "Done")),
      size = "m" #'size = "l"' for large
    )
  }
}


###############################################################################
# renderUI()'s

#------------------------------------------------------------------------------
### Selection dropdown for specific parameters
output$pretty_toplot_update_which_param_uiOut_select <- renderUI({
  choices.list.names <- req(pretty_toplot_update_table())$Name
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("pretty_toplot_update_which_param",
              tags$h5("Choose parameter to update"),
              choices = choices.list, selected = 1)
})


#------------------------------------------------------------------------------
# renderUI() #1
output$pretty_toplot_update_thing1_uiOut_mult <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))
  z.names <- req(pretty_toplot_update_table())$Name

  #--------------------------------------------------------
  if (z == 1) {
    if (z2 == 1) {
      tags$h5("Cannot update this parameter", style = "color: red;")
    } else {
      z2 <- z2 - 1
      z.names <- z.names[2:5]
      val.curr <- y$plot.lim[z2]
      numericInput("pretty_toplot_update_thing1", tags$h5(z.names[z2]),
                   value = val.curr)
    }

    #------------------------------------------------------
  } else if (z == 2) {
    if (z2 == 1) {
      val.curr <- y$background.color
      input.lab <- "Click to select background color"
      colourpicker::colourInput(
        "pretty_toplot_update_thing1", tags$h5(input.lab),
        showColour = "background", value = val.curr
      )

    } else if (z2 == 2) {
      val.curr <- is.null(y$list.colorscheme$col.na)
      input.lab <- "Make NA predictions transparent"
      checkboxInput("pretty_toplot_update_thing1", input.lab, value = val.curr)

    } else {
      tags$h5("Cannot update this parameter", style = "color: red;")
    }

    #------------------------------------------------------
  } else if (z == 3) {
    y.leg <- y$list.legend
    input.lab <- z.names[z2]

    if (z2 == 1) {
      val.curr <- y.leg$inc
      checkboxInput("pretty_toplot_update_thing1", input.lab, value = val.curr)

    } else if (z2 == 2) {
      val.curr <- ifelse(y.leg$out, 2, 1)
      radioButtons("pretty_toplot_update_thing1", tags$h5(input.lab),
                   choices = list("Inside map frame" = 1, "Outside map frame" = 2), selected = 1)

    } else if (z2 == 3) {
      if (!y.leg$out) {
        choices.list <- choices.list.pos
        val.curr <- which(sapply(list.pos.vals, function(i) identical(i, y.leg$pos)))

      } else { #y.leg$out
        choices.list <- choices.list.posout
        val.curr <- y.leg$out.pos
      }

      selectInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                  choices = choices.list, selected = val.curr)

    } else if (z2 == 4) {
      if (!y.leg$out) {
        tags$h5("N/A: only applies when legend outside map frame",
                style = "color: red;")
      } else {
        val.curr <- y.leg$width
        numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                     value = val.curr, min = 0.1, max = 0.5, step = 0.05)
      }

    } else if (z2 == 5) {
      val.curr <- y.leg$text.size
      numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                   value = val.curr, min = 0.1, step = 0.1)

    } else if (z2 == 6) {
      val.curr <- ifelse(y.leg$border == "black", TRUE, FALSE)
      checkboxInput("pretty_toplot_update_thing1", input.lab, value = val.curr)

    } else {
      tags$h5("Cannot update this parameter", style = "color: red;")
    }



    #------------------------------------------------------
  } else if (z == 4) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 5) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 6) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else { #z == 7
    val.curr <- y$id
    textInput("pretty_toplot_update_thing1", tags$h5("Map ID"),
              value = val.curr)
  }
})


#------------------------------------------------------------------------------
# renderUI() #2
output$pretty_toplot_update_thing2_uiOut_mult <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))

  if (z == 1) {
    req(NULL)

    #------------------------------------------------------
  } else if (z == 2 & z2 == 2) {
    req(input$pretty_toplot_update_thing1)
    req(!input$pretty_toplot_update_thing1)

    val.curr <- y$list.colorscheme$col.na
    input.lab <- "Click to select color of NA predictions"

    colourpicker::colourInput(
      "pretty_toplot_update_thing2", tags$h5(input.lab),
      showColour = "background", value = val.curr
    )

    #------------------------------------------------------
  } else if (z == 3) {
    req(NULL)

    #------------------------------------------------------
  } else if (z == 4) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 5) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 6) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else { #z == 7
    req(NULL)
  }
})


###############################################################################
### Table display current parameters
output$pretty_toplot_update_table_out <- renderTable({
  pretty_toplot_update_table()
})

pretty_toplot_update_table <- reactive({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which

  #--------------------------------------------------------
  if (z == 1) {
    params.names <- c(
      "Coordinate system",
      "Longitude minimum", "Longitude maximum", "Latitude minimum",
      "Latitude maximum"
    )
    params.vals <- c("N/A: cannot update", y$plot.lim)

    #------------------------------------------------------
  } else if (z == 2) {
    params.names <- c(
      "Background color", "NA prediction color", "Prediction color scheme"
    )
    temp <- y$list.colorscheme$col.na
    params.vals <- c(
      y$background.color, ifelse(is.null(temp), "Transparent", temp),
      "N/A: cannot update"
    )

    #------------------------------------------------------
  } else if (z == 3) {
    y.leg <- y$list.legend
    params.names <- c(
      "Include legend", "Place legend:", "Legend position", "Legend width",
      "Legend text size", "Include black frame around legend",
      "Legend labels: number of decimals"
    )
    if (y.leg$inc) {
      param.pos <- ifelse(
        y.leg$out,
        esdm_simple_cap(y.leg$out.pos),
        names(choices.list.pos)[which(sapply(list.pos.vals, function(i) identical(i, y.leg$pos)))]
      )

      params.vals <- c(
        y.leg$inc, ifelse(y.leg$out, "Outside map frame", "Inside map frame"),
        param.pos,
        ifelse(y.leg$out, y.leg$width, "N/A: only applies when legend outside map frame"),
        y.leg$text.size, ifelse(y.leg$border == "black", TRUE, FALSE),
        "N/A: cannot update"
      )

    } else {
      params.vals <- c(y.leg$inc, rep("N/A: no legend", 6))
    }

    #------------------------------------------------------
  } else if (z == 4) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 5) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 6) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else { #z == 7
    params.names <- "Map ID"
    params.vals <- y$id

  }

  data.frame(
    Name = params.names, Values = params.vals, stringsAsFactors = FALSE
  )
})


###############################################################################
# Update reactiveVal within modal
# Modals can't do eventReactive()
observeEvent(input$pretty_toplot_update_execute, {
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))

  if (z == 1 & z2 != 1) {
    y$plot.lim[z2 - 1] <- req(input$pretty_toplot_update_thing1)

    #------------------------------------------------------
  } else if (z == 2) {
    if (z2 == 1) {
      y$background.color <- input$pretty_toplot_update_thing1

    } else if (z2 == 2) {
      if (input$pretty_toplot_update_thing1) {
        y$list.colorscheme$col.na <- NULL
      } else {
        y$list.colorscheme$col.na <- input$pretty_toplot_update_thing2
      }
    }

    #------------------------------------------------------
  } else if (z == 3) {
    y.leg <- y$list.legend
    if (z2 == 1) {
      y.leg$inc <- input$pretty_toplot_update_thing1

    } else if (z2 == 2) {
      if (input$pretty_toplot_update_thing1 == 1) {
        y.leg$out <- FALSE
        if (is.null(y.leg$pos)) y.leg$pos - c("right", "top")
        y.leg$out.pos <- NULL
        y.leg$width <- 1
      } else {
        y.leg$out <- TRUE
        y.leg$pos <- NULL
        if (is.null(y.leg$out.pos)) y.leg$out.pos - "right"
        if (y.leg$width > 0.5) y.leg$width <- 0.2
      }

    } else if (z2 == 3) {
      if (!y.leg$out) {
        y.leg$pos <- list.pos.vals[[as.numeric(input$pretty_toplot_update_thing1)]]
      } else {
        y.leg$out.pos <- input$pretty_toplot_update_thing1
      }

    } else if (z2 == 4) {
      if (y.leg$out) y.leg$width <- input$pretty_toplot_update_thing1


    } else if (z2 == 5) {
      y$text.size <- input$pretty_toplot_update_thing1

    } else if (z2 == 6) {
      y.leg$border <- ifelse(input$pretty_toplot_update_thing1, "black", FALSE)
    }

    y$list.legend <- y.leg

    #------------------------------------------------------
  } else if (z == 4) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 5) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if (z == 6) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else { #z == 7
    y$id <- req(input$pretty_toplot_update_thing1)
  }


  #--------------------------------------------------------
  val.pretty.toplot.update(y)
})


###############################################################################
# Final processing step and close modal
observeEvent(input$pretty_toplot_update_done, {
  removeModal()

  x <- req(input$pretty_update_table_out_rows_selected)

  vals$pretty.params.toplot[[x]] <- val.pretty.toplot.update()
  val.pretty.toplot.update(NULL)
})

###############################################################################
