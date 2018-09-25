# renderUI()'s for updating saved map parameters

###############################################################################
# Additional object-specific widgets

### Selection dropdown for additional objects
output$pretty_toplot_update_which_addobj_uiOut_select <- renderUI({
  if (input$pretty_toplot_update_which == 6) {
    y <- req(val.pretty.toplot.update())
    y.list.addobj <- req(y$list.addobj)

    choices.list <- seq_along(y.list.addobj) %>%
      set_names(sapply(y.list.addobj, function(i) i$obj.text))
    input.lab <- "Choose additional object to update"

    selectInput("pretty_toplot_update_which_addobj", tags$h5(input.lab),
                choices = choices.list, selected = 1)

  } else {
    NULL
  }
})


### Remove additional object
output$pretty_toplot_update_addobj_remove_uiOut_button <- renderUI({
  req(input$pretty_toplot_update_which_addobj,
      input$pretty_toplot_update_which == 6,
      val.pretty.toplot.update()$list.addobj)
  actionButton("pretty_toplot_update_addobj_remove",
               "Remove selected additional object")
})


###############################################################################
### Selection dropdown for specific parameters
output$pretty_toplot_update_which_param_uiOut_select <- renderUI({
  if (input$pretty_toplot_update_which == 6) { #needs to update based off table
    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    isolate({
      y <- req(val.pretty.toplot.update())
      y.addobj <- y$list.addobj[[addobj.which]]
    })

    choices.list.names <- c( #function in '..._addobj_update.R'
      "Object name", pretty_addobj_update_names_func(y.addobj)
    )

  } else {
    isolate(choices.list.names <- req(pretty_toplot_update_table())$Name)
  }

  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("pretty_toplot_update_which_param",
              tags$h5("Choose parameter to update"),
              choices = choices.list, selected = 1)
})


###############################################################################
# renderUI() #0: message.360 is in 'server_5_prettyPlot.R'
output$pretty_toplot_update_message360_uiOut_text <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))

  req(
    (z == 1 & z2 != 1) | (z == 5 & z2 == 2),
    st_bbox(st_transform(y$model.toplot, 4326))[3] > 180
  )

  tags$h5(message.360, tags$br(), style = "color: red;")
})

# renderUI() #0: message
output$pretty_toplot_update_message_uiOut_text <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))
  z.names <- req(pretty_toplot_update_table())$Name
  z.vals <- req(pretty_toplot_update_table())$Value

  if (z == 1 & z2 != 1) {
    temp <- paste(
      "Please ensure that the 'minimum' values remain less than their",
      "respective 'maximum' values.",
      "In addition, note that if you increase the expanse of the map",
      "you will likely need to update the grid line locations"
    )

  } else if (z == 5 & z2 == 2) {
    temp <- paste(
      "Please ensure that the 'Longitude start value' is between",
      "the specified longitude limits:",
      paste(y$map.range[1:2], collapse = " and ")
    )

  }  else if (z == 5 & z2 == 3) {
    temp <- paste(
      "Please ensure that the 'Latitude start value' is between",
      "the specified latitude limits:",
      paste(y$map.range[3:4], collapse = " and ")
    )

  } else {
    req(NULL)
  }

  tags$h5(temp, style = "color: red;")
})


###############################################################################
# renderUI() #1
output$pretty_toplot_update_thing1_uiOut_mult <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))
  z.names <- req(pretty_toplot_update_table())$Name
  z.vals <- req(pretty_toplot_update_table())$Value

  #--------------------------------------------------------
  if (z == 1) {
    if (z2 == 1) {
      tags$h5("Cannot update this parameter", style = "color: red;")
    } else {
      val.curr <- y$map.range[z2 - 1]
      input.lab <- z.names[z2]
      numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
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

    } else if (z2 == 7 ) {
      tags$h5("Cannot update this parameter", style = "color: red;")

    } else if (!y.leg$inc) {
      tags$h5("N/A - legend not included", style = "color: red;")

    } else {
      if (z2 == 2) {
        val.curr <- ifelse(y.leg$out, 2, 1)
        radioButtons("pretty_toplot_update_thing1", tags$h5(input.lab),
                     choices = list("Inside map frame" = 1, "Outside map frame" = 2),
                     selected = val.curr)

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

      }
    }

    #------------------------------------------------------
  } else if (z == 4) {
    if (z2 %in% 1:3) {
      val.curr <- y$list.titlelab[[z2]]
      input.lab <- z.names[[z2]]
      textInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                value = val.curr)

    } else if (z2 %in% 4:5) {
      val.curr <- y$list.titlelab[[z2]]
      input.lab <- z.names[[z2]]
      numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                   value = val.curr, step = 0.1)

    } else {
      val.curr <- y$list.margin[[z2 - 5]]
      input.lab <- z.names[z2]
      numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                   value = val.curr, step = 0.01)
    }

    #------------------------------------------------------
  } else if (z == 5) {
    y.t <- y$list.tick
    input.lab <- z.names[z2]
    val.curr <- z.vals[z2]

    if (z2 == 1 ) {
      checkboxInput("pretty_toplot_update_thing1", input.lab,
                    value = as.logical(val.curr))

    } else if (!y.t$inc) {
      tags$h5("N/A: coordinate grid lines not included", style = "color: red;")

    } else {
      if (z2 == 2) {
        input.lab <- "Longitude grid line start"
        numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                     value = y.t$x.vals[1], step = 5)

      } else if (z2 == 3) {
        input.lab <- "Latitude grid line start"
        numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                     value = y.t$y.vals[1], step = 5)

      } else if (z2 %in% 4:5) {
        max.curr <- ifelse(z2 == 5, 1, NA)
        numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                     value = as.numeric(val.curr), step = 0.1,
                     min = 0, max = max.curr)

      } else if (z2 == 6) {
        colourpicker::colourInput(
          "pretty_toplot_update_thing1", tags$h5(input.lab),
          showColour = "background", value = val.curr
        )

      } else if (z2 == 7) {
        checkboxInput("pretty_toplot_update_thing1", input.lab, value = as.logical(val.curr))

      } else if (y.t$grid.labs.size == 0) {
        tags$h5("N/A: coordinate labels not included", style = "color: red;")

      } else {
        if (z2 == 8) {
          val.curr <- ifelse(y.t$grid.labs.in, 1, 2)
          radioButtons("pretty_toplot_update_thing1", tags$h5(input.lab),
                       choices = list("Inside frame" = 1, "Outside frame" = 2),
                       selected = val.curr)

        } else { #z2 == 9
          numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                       value = as.numeric(val.curr), min = 0.1, step = 0.1)
        }
      }
    }

    #------------------------------------------------------
  } else if (z == 6) {
    input.lab <- z.names[z2]
    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    y.addobj <- y$list.addobj[[addobj.which]]

    if (z2 %in% 1:2) {
      tags$h5("Cannot update this parameter", style = "color: red;")

    } else if (z2 == 3) {
      val.curr <- as.numeric(y.addobj$obj.order)
      selectInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                  choices = list("Draw object behind SDM" = 1,
                                 "Draw object in front of SDM" = 2),
                  selected = val.curr)

    } else if (z2 == 4) {
      val.curr <- is.na(y.addobj$col.ptfill)
      input.lab <- addobj_render_lab(3, y.addobj$obj.which, y.addobj$obj.type)
      checkboxInput("pretty_toplot_update_thing1", input.lab, value = val.curr)

    } else if (z2 == 5) {
      if (input.lab == "N/A") {
        tags$h5("N/A: object type is points", style = "color: red;")
      } else {
        val.curr <- is.na(y.addobj$col.absborder)
        input.lab <- addobj_render_lab(5, y.addobj$obj.which, y.addobj$obj.type)
        checkboxInput("pretty_toplot_update_thing1", input.lab, value = val.curr)
      }

    } else if (z2 == 6) {
      val.curr <- y.addobj$pchlty

      if (y.addobj$obj.type == 1) {
        choices.list <- choices.list.pch
      } else {
        choices.list <- choices.list.lty
      }

      selectizeInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                     choices = choices.list, selected = val.curr,
                     multiple = FALSE)

    } else if (z2 == 7) {
      val.curr <- y.addobj$cexlwd
      numericInput("pretty_toplot_update_thing1", tags$h5(input.lab),
                   value = val.curr, min = 0.1, step = 0.1)
    }

    #------------------------------------------------------
  } else { #z == 7
    val.curr <- y$id
    textInput("pretty_toplot_update_thing1", tags$h5("Map ID"),
              value = val.curr)
  }
})


###############################################################################
# renderUI() #2
output$pretty_toplot_update_thing2_uiOut_mult <- renderUI({
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))

  if (z == 2 & z2 == 2) {
    req(is.logical(input$pretty_toplot_update_thing1))
    req(!input$pretty_toplot_update_thing1)

    val.curr <- y$list.colorscheme$col.na
    input.lab <- "Click to select color of NA predictions"

    colourpicker::colourInput(
      "pretty_toplot_update_thing2", tags$h5(input.lab),
      showColour = "background", value = val.curr
    )

    #------------------------------------------------------
  } else if (z == 5) {
    y.t <- y$list.tick
    if (z2 == 2) {
      val.curr <- y.t$x.vals[2] - y.t$x.vals[1]
      input.lab <- "Longitude grid line interval"
      numericInput("pretty_toplot_update_thing2", tags$h5(input.lab),
                   value = val.curr, step = 5)
    } else if (z2 == 3) {
      val.curr <- y.t$y.vals[2] - y.t$y.vals[1]
      input.lab <- "Latitude grid line interval"
      numericInput("pretty_toplot_update_thing2", tags$h5(input.lab),
                   value = val.curr, step = 5)
    } else {
      NULL
    }

  } else if (z == 6) {
    req(z2 %in% 4:5, is.logical(input$pretty_toplot_update_thing1))
    req(!input$pretty_toplot_update_thing1)

    input.lab <- req(pretty_toplot_update_table())$Name[z2]
    input.lab <- paste("Click to select", tolower(input.lab))
    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    y.addobj <- y$list.addobj[[addobj.which]]

    if (z2 == 4) {
      val.curr <- y.addobj$col.ptfill

      colourpicker::colourInput(
        "pretty_toplot_update_thing2", tags$h5(input.lab),
        showColour = "background", value = val.curr
      )

    } else if (z2 == 5) {
      val.curr <- y.addobj$col.absborder

      colourpicker::colourInput(
        "pretty_toplot_update_thing2", tags$h5(input.lab),
        showColour = "background", value = val.curr
      )

    } else {
      NULL
    }

    #------------------------------------------------------
  } else { #z %in% c(1, 3, 4, 5, 7)
    NULL
  }
})

###############################################################################
