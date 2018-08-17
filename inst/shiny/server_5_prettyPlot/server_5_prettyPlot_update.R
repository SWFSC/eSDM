# Code for updating parameters of maps in pretty plot to-plot list

###############################################################################
# Clear data table selections whenever map control option is changed
observeEvent(input$pretty_mapcontrol, {
  dataTableProxy("pretty_table_orig_out") %>% selectRows(list())
  dataTableProxy("pretty_table_over_out") %>% selectRows(list())
  dataTableProxy("pretty_table_ens_out") %>% selectRows(list())
  dataTableProxy("pretty_update_table_out") %>% selectRows(list())
})


###############################################################################
###############################################################################
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
                              choices = choices.list.main, selected = 1))
        # column(
        #   width = 6,
        #   uiOutput("pretty_addobj_update_thing_uiOut_mult"),
        #   uiOutput("pretty_addobj_update_thing2_uiOut_mult")
        # )
      ),
      # actionButton("pretty_addobj_update_execute", "Save parameter"),
      # tags$br(), tags$br(), tags$br(),
      # tags$h5("Saved parameters for selected additional object. The color values will be 'NA' if transparent;",
      #         "otherwise they are displayed as hexadecimals."),
      tableOutput("pretty_toplot_update_table_out"),

      footer = tagList(actionButton("pretty_toplot_update_done", "Done")),
      size = "m" #'size = "l"' for large
    )
  }
}


###############################################################################
# renderUI()'s

### Selection dropdown for specific parameters
output$pretty_toplot_update_which_param_uiOut_select <- renderUI({
  input$pretty_toplot_update_which

  selectInput("pretty_toplot_update_which_param",
              tags$h5("Choose parameter to update"),
              choices = list("TODO" = 1), selected = 1)
})


### Table display current parameters
output$pretty_toplot_update_table_out <- renderTable({
  y <- req(val.pretty.toplot.update())

  browser()
  #--------------------------------------------------------
  if (input$pretty_toplot_update_which == 1) {
    params.names <- c(
      "Longitude minimum", "Longitude maximum", "Latitude minimum",
      "Latitude maximum"
    )
    params.vals <- y$plot.lim

    #------------------------------------------------------
  } else if(input$pretty_toplot_update_which == 2) {
    params.names <- c("Background color", "Prediction color scheme")
    params.vals <- c(y$background.color, "N/A: cannot update")

    #------------------------------------------------------
  } else if(input$pretty_toplot_update_which == 3) {
    y.leg <- y$list.legend
    params.names <- c(
      "Include legend",
      "Place legend",
      "Legend position",
      "Legend width",
      "Legend text size",
      "Include black frame around legend",
      "Legend labels: number of decimals"
    )
    if (y.leg$inc) {
      params.vals <- c(
        y.leg$inc,
        ifelse(y.leg$out, "Outside map frame", "Inside map frame"),
        "TODO",
        "TODO",
        y.leg$text.size,
        ifelse(y.leg$border == "black", TRUE, FALSE),
        "N/A: cannot update"
      )

    } else {
      params.vals <- c(y.leg$inc, rep("N/A: no legend", 5))
    }

    #------------------------------------------------------
  } else if(input$pretty_toplot_update_which == 4) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if(input$pretty_toplot_update_which == 5) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else if(input$pretty_toplot_update_which == 6) {
    validate(need(FALSE, "Not ready yet"))

    #------------------------------------------------------
  } else { #input$pretty_toplot_update_which == 7
    params.names <- "Map ID"
    params.vals <- y$id

  }

  data.frame(
    Name = params.names, Values = params.vals, stringsAsFactors = FALSE
  )
})


###############################################################################
# Final processing step and close modal
observeEvent(input$pretty_toplot_update_done, {
  removeModal()

  # z <- req(input$pretty_addobj_update_which)
  # x <- input$pretty_addobj_table_out_rows_selected
  #
  # vals$pretty.addobj[[x]] <- val.pretty.addobj.update()
  # val.pretty.addobj.update(NULL)
})

###############################################################################
