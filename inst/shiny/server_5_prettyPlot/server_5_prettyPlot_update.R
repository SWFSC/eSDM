# Code for modal dialog window for updating params of saved maps

###############################################################################
# Show modal when button is clicked.
observeEvent(input$pretty_update_toplot_show, {
  val.pretty.update.mess(NULL)
  showModal(toplot_update_modal(
    failed = !isTruthy(input$pretty_update_table_out_rows_selected)
  ))
})


### Reset update message when necessary
observe({
  input$tabs
  input$pretty_mapcontrol
  input$pretty_toplot_remove_execute

  val.pretty.update.mess(NULL)
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
      "Map coordinate system and range" = 1,
      "Background color and prediction color scheme" = 2,
      "Legend" = 3, "Title, axis labels, and margins" = 4,
      "Coordinate grid marks and labels" = 5,
      "Additional objects (points or polygons)" = 6, "Map ID" = 7
    )

    modalDialog(
      tags$h4("Saved map parameter update window"),
      tags$h5("Select the parameter you wish to update and change it as desired in the window that appears.",
              "Then click 'Save parameter', and the newly saved parameter will be updated in the table below.",
              "After the table reflects the desired parameter values, click 'Done - save the updated parameters'."),
      tags$h5("Note that you cannot update the map coordinate system or prediction color scheme of a saved map;",
              "to change these parameters you must create a new map."),
      tags$br(),
      tags$strong(paste("Map ID:", vals$pretty.params.toplot[[x]]$id)),
      fluidRow(
        column(
          width = 6,
          selectInput("pretty_toplot_update_which", tags$h5("Choose parameter section"),
                      choices = choices.list.main, selected = 1),
          tags$br(), tags$br(),
          uiOutput("pretty_toplot_update_addobj_remove_uiOut_button")
        ),
        conditionalPanel(
          condition = "output.pretty_toplot_update_addobj_flag",
          column(
            width = 6,
            uiOutput("pretty_toplot_update_which_addobj_uiOut_select"),
            uiOutput("pretty_toplot_update_which_param_uiOut_select")
          )
        )
      ),
      conditionalPanel(
        condition = "output.pretty_toplot_update_addobj_flag == false",
        tags$h5("There are no additional objects for this saved map", style = "color: red;")
      ),
      conditionalPanel(
        condition = "output.pretty_toplot_update_addobj_flag",
        box(
          width = 12,
          uiOutput("pretty_toplot_update_message360_uiOut_text"),
          uiOutput("pretty_toplot_update_message_uiOut_text"),
          fluidRow(
            column(6, uiOutput("pretty_toplot_update_thing1_uiOut_mult"), uiOutput("pretty_toplot_update_thing1_addobj_uiOut_mult")),
            column(6, uiOutput("pretty_toplot_update_thing2_uiOut_mult"))
          )
        ),
        uiOutput("pretty_toplot_update_execute_uiOut_action"),
        uiOutput("pretty_toplot_update_execute_addobj_uiOut_action"),
        uiOutput("pretty_toplot_update_temp_out_text"),
        tags$br(), tags$br(), tags$br(),
        tags$h5("Saved parameters for selected additional object. Any color values will be 'NA' if transparent;",
                "otherwise they are displayed as hexadecimals."),
        tableOutput("pretty_toplot_update_table_out")
      ),

      footer = tagList(
        actionButton("pretty_toplot_update_cancel", "Cancel and discard the updated parameters"),
        actionButton("pretty_toplot_update_done", "Done - save the updated parameters")
      ),
      size = "l" #'size = "l"' for large
    )
  }
}


###############################################################################
# Flag for (if add obj is selected) if selected map has add objects
output$pretty_toplot_update_addobj_flag <- reactive({
  if (isTruthy(input$pretty_toplot_update_which)) {
    isTruthy(val.pretty.toplot.update()$list.addobj) |
      input$pretty_toplot_update_which != 6
  } else {
    TRUE
  }
})
outputOptions(
  output, "pretty_toplot_update_addobj_flag", suspendWhenHidden = FALSE
)

# Reactive for if map includes multiple additional objects
pretty_toplot_update_addobj_len <- reactive({
  req(input$pretty_toplot_update_which == 6)
  length(req(val.pretty.toplot.update()$list.addobj))
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
    # Map coordinate system and range
    params.names <- c(
      "Coordinate system",
      "Longitude minimum", "Longitude maximum", "Latitude minimum",
      "Latitude maximum"
    )
    params.vals <- c("N/A: cannot update", y$map.range)

    #------------------------------------------------------
  } else if (z == 2) {
    # Background color and prediction color scheme
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
    # Legend
    y.leg <- y$list.legend
    params.names <- c(
      "Include legend", "Legend location", "Legend position", "Legend width",
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
      params.vals <- c(y.leg$inc, rep("N/A: legend not included", 6))
    }

    #------------------------------------------------------
  } else if (z == 4) {
    # Title, axis labels, and margins
    params.names <- c(
      "Title", "X-axis label", "Y-axis label", "Title size", "Axis label size",
      "Inner margin - bottom", "Inner margin - left", "Inner margin - top",
      "Inner margin - right", "Outer margin"
    )
    params.vals <- c(
      unname(unlist(y$list.titlelab)), unname(unlist(y$list.margin))
    )

    #------------------------------------------------------
  } else if (z == 5) {
    # Coordinate grid marks and labels
    y.t <- y$list.tick
    params.names <- c(
      "Include coordinate grid marks",
      "Include grid lines", "Include tick marks",
      "Longitude grid mark locations", "Latitude grid mark locations",
      "Grid mark width", "Grid mark transparency (1: solid; 0: transparent)",
      "Coordinate grid mark color", #"Include coordinate labels",
      "Coordinate label location", "Coordinate label size"
    )
    if (y.t$inc) {
      # y.t.labinc <- y.t$grid.labs.size > 0
      params.vals <- c(
        TRUE, y.t$grid.lines, y.t$grid.ticks,
        paste(y.t$x.vals, collapse = ", "), paste(y.t$y.vals, collapse = ", "),
        y.t$grid.lw, y.t$grid.alpha, y.t$grid.col, #y.t.labinc,
        ifelse(y.t$grid.labs.in, "Inside frame", "Outside frame"),
        y.t$grid.labs.size
        # ifelse(y.t.labinc, ifelse(y.t$grid.labs.in, "Inside frame", "Outside frame"),
        #        "N/A: coordinate labels not included"),
        # ifelse(y.t.labinc, y.t$grid.labs.size, "N/A: coordinate labels not included")
      )
    } else {
      params.vals <- c(
        FALSE, rep("N/A: coordinate grid marks not included", length(params.names) - 1)
      )
    }

    #------------------------------------------------------
  } else if (z == 6) {
    # Additional objects (points or polygons)
    if (!isTruthy(y$list.addobj)) {
      return(data.frame(Name = NA, Value = NA))
    }

    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    req(addobj.which <= length(y$list.addobj))
    y.addobj <- y$list.addobj[[addobj.which]]

    params.names <- c( #function in '..._addobj_update.R'
      "Object name", pretty_addobj_update_names_func(y.addobj)
    )
    params.vals <- c(
      y.addobj$obj.text, pretty_addobj_update_vals_func(y.addobj)
    )


    #------------------------------------------------------
  } else { #z == 7
    # Map ID
    params.names <- "Map ID"
    params.vals <- y$id

  }

  data.frame(
    Name = params.names, Value = params.vals, stringsAsFactors = FALSE
  )
})


###############################################################################
# Remove selected additional object
observeEvent(input$pretty_toplot_update_addobj_remove, {
  y <- req(val.pretty.toplot.update())
  addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))

  y$list.addobj <- y$list.addobj[-addobj.which]
  if (length(y$list.addobj) == 0) y$list.addobj <- NULL

  val.pretty.toplot.update(y)
})


###############################################################################
# Update reactiveVal within modal; modals can't do eventReactive()
observeEvent(input$pretty_toplot_update_execute, {
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which
  z2 <- as.numeric(req(input$pretty_toplot_update_which_param))

  #--------------------------------------------------------
  if (z == 1 && z2 != 1) {
    # May need to update grid mark locations
    if (z2 == 2) {
      y$map.range[1] <- req(input$pretty_toplot_update_thing1)
      y$map.range[2] <- req(input$pretty_toplot_update_thing2)

      x.tf <- dplyr::between(y$list.tick$x.vals, y$map.range[1], y$map.range[2])
      y$list.tick$x.vals <- y$list.tick$x.vals[x.tf]

      if (length(y$list.tick$x.vals) == 0) {
        y$list.tick$x.vals <- y$map.range[1:2]
      } else {
        x.interval <- unique(diff(y$list.tick$x.vals))
        y$list.tick$x.vals <- unique(c(
          rev(seq(from = y$list.tick$x.vals[1], to = y$map.range[1], by = -x.interval)),
          seq(from = y$list.tick$x.vals[1], to = y$map.range[2], by = x.interval)
        ))
      }

    } else { #z2 == 3
      y$map.range[3] <- req(input$pretty_toplot_update_thing1)
      y$map.range[4] <- req(input$pretty_toplot_update_thing2)

      y.tf <- dplyr::between(y$list.tick$y.vals, y$map.range[3], y$map.range[4])
      y$list.tick$y.vals <- y$list.tick$y.vals[y.tf]

      if (length(y$list.tick$y.vals) == 0) {
        y$list.tick$y.vals <- y$map.range[1:2]
      } else {
        y.interval <- unique(diff(y$list.tick$y.vals))
        y$list.tick$y.vals <- unique(c(
          rev(seq(from = y$list.tick$y.vals[1], to = y$map.range[3], by = -y.interval)),
          seq(from = y$list.tick$y.vals[1], to = y$map.range[4], by = y.interval)
        ))
      }
    }

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
        if (is.null(y.leg$pos)) y.leg$pos <- c("right", "top")
        y.leg$out.pos <- NULL
        y.leg$width <- 1
      } else {
        y.leg$out <- TRUE
        y.leg$pos <- NULL
        if (is.null(y.leg$out.pos)) y.leg$out.pos <- "right"
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
      y.leg$text.size <- input$pretty_toplot_update_thing1

    } else if (z2 == 6) {
      y.leg$border <- ifelse(input$pretty_toplot_update_thing1, "black", FALSE)
    }

    y$list.legend <- y.leg

    #------------------------------------------------------
  } else if (z == 4) {
    if (z2 %in% 1:5) {
      y$list.titlelab[[z2]] <- input$pretty_toplot_update_thing1
    } else {
      y$list.margin[[z2 - 5]] <- input$pretty_toplot_update_thing1
    }

    #------------------------------------------------------
  } else if (z == 5) {
    y.t <- y$list.tick
    if (z2 == 1) {
      y.t$inc <- input$pretty_toplot_update_thing1

    } else if (z2 == 2) {
      y.t$grid.lines <- input$pretty_toplot_update_thing1

    } else if (z2 == 3) {
      if (!y.t$grid.labs.in)
        y.t$grid.ticks <- input$pretty_toplot_update_thing1

    } else if (z2 == 4) {
      req(dplyr::between(
        input$pretty_toplot_update_thing1, y$map.range[1], y$map.range[2]
      ))
      y.t$x.vals <- seq(
        from = input$pretty_toplot_update_thing1, to = y$map.range[2],
        by = req(input$pretty_toplot_update_thing2)
      )

    } else if (z2 == 5) {
      req(dplyr::between(
        input$pretty_toplot_update_thing1, y$map.range[3], y$map.range[4]
      ))
      y.t$y.vals <- seq(
        from = input$pretty_toplot_update_thing1, to = y$map.range[4],
        by = req(input$pretty_toplot_update_thing2)
      )

    } else if (z2 == 6) {
      y.t$grid.lw <- input$pretty_toplot_update_thing1

    } else if (z2 == 7) {
      y.t$grid.alpha <- input$pretty_toplot_update_thing1

    } else if (z2 == 8) {
      y.t$grid.col <- input$pretty_toplot_update_thing1

    } else if (z2 == 9) {
      y.t$grid.labs.in <- input$pretty_toplot_update_thing1 == 1
      if (y.t$grid.labs.in) y.t$grid.ticks <- FALSE

    } else { #z2 == 10
      if (input$pretty_toplot_update_thing1 > 0)
        y.t$grid.labs.size <- input$pretty_toplot_update_thing1
    }

    y$list.tick <- y.t

    #------------------------------------------------------
  } else if (z == 6) {
    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    req(addobj.which <= pretty_toplot_update_addobj_len())
    y.addobj <- y$list.addobj[[addobj.which]]
    input.lab <- req(pretty_toplot_update_table())$Name[z2]

    if (z2 == 3) {
      y.addobj$obj.order <- input$pretty_toplot_update_thing1

    } else if (z2 == 4) {
      req(is.logical(input$pretty_toplot_update_thing1))
      y.addobj$col.ptfill <- ifelse(
        input$pretty_toplot_update_thing1 | input.lab == "N/A",
        NA, input$pretty_toplot_update_thing2
      )

    } else if (z2 == 5) {
      req(is.logical(input$pretty_toplot_update_thing1))
      y.addobj$col.absborder <- ifelse(
        input$pretty_toplot_update_thing1 | input.lab == "N/A",
        NA, input$pretty_toplot_update_thing2
      )

    } else if (z2 == 6) {
      y.addobj$pchlty <- as.numeric(input$pretty_toplot_update_thing1)

    } else if (z2 == 7) {
      y.addobj$cexlwd <- input$pretty_toplot_update_thing1
    }

    y$list.addobj[[addobj.which]] <- y.addobj

    #------------------------------------------------------
  } else { #z == 7
    y$id <- req(input$pretty_toplot_update_thing1)
  }


  #--------------------------------------------------------

  val.pretty.toplot.update(y)
})

# Update additional object plot order
observeEvent(input$pretty_toplot_update_execute_addobj, {
  y <- req(val.pretty.toplot.update())
  z <- input$pretty_toplot_update_which

  if (z == 6) {
    addobj.which <- as.numeric(req(input$pretty_toplot_update_which_addobj))
    if (addobj.which > pretty_toplot_update_addobj_len()) {
      plot.order <- as.numeric(req(input$pretty_toplot_update_thing1_addobj))
      req(length(plot.order) == length(y$list.addobj))
      y$list.addobj <- y$list.addobj[plot.order]
    }
  }

  val.pretty.toplot.update(y)
})


###############################################################################
### Cancel and discard updated parameters
observeEvent(input$pretty_toplot_update_cancel, {
  removeModal()

  val.pretty.update.mess(list(1, val.pretty.toplot.update()$id))
  val.pretty.toplot.update(NULL)
})

### Done - save updated parameters
observeEvent(input$pretty_toplot_update_done, {
  removeModal()

  x <- req(input$pretty_update_table_out_rows_selected)

  # Update color scheme data breaks and legend labels if necessary
  y <- val.pretty.toplot.update()
  if (identical(y$list.colorscheme$leg.labs[1], "Lowest 60%") | (!y$se.flag)) {
    if (!identical(vals$pretty.params.toplot[[x]]$map.range, y$map.range)) {
      l.cs <- y$list.colorscheme
      temp <- pretty_colorscheme_func(
        y$model.toplot, l.cs$data.name, y$map.range, l.cs$perc,
        length(l.cs$col.pal), leg.perc.esdm, l.cs$leg.round
      )

      y$list.colorscheme[c("data.breaks", "leg.labs")] <- temp
      val.pretty.toplot.update(y)
    }
  }

  val.pretty.update.mess(list(2, val.pretty.toplot.update()$id))
  vals$pretty.params.toplot[[x]] <- val.pretty.toplot.update()
  val.pretty.toplot.update(NULL)
})

###############################################################################
