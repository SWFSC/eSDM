# Code for updating parameters of maps in pretty plot to-plot list


###############################################################################
# Clear data table selections whenever map control option is changed
observeEvent(input$pretty_plot_mapcontrol, {
  dataTableProxy("pretty_table_orig_out") %>% selectRows(list())
  dataTableProxy("pretty_table_over_out") %>% selectRows(list())
  dataTableProxy("pretty_table_ens_out") %>% selectRows(list())
  dataTableProxy("pretty_plot_update_table_out") %>% selectRows(list())
})


###############################################################################
###############################################################################
# Code for modal dialog window for updating params of saved maps

###############################################################################
# Show modal when button is clicked.
observeEvent(input$pretty_plot_update_toplot_show, {
  showModal(toplot_update_modal(
    failed = !isTruthy(input$pretty_plot_update_table_out_rows_selected)
  ))
})


###############################################################################
toplot_update_modal <- function(failed) {
  if (failed) {
    modalDialog(
      tags$strong("Error: You must select a row from the table",
                  style = "color: red;"),
      tags$br(),
      tags$h5("Click 'Cancel' to close this window and then select a row from the table"),
      footer = tagList(modalButton("Cancel"))
    )

  } else {
    x <- req(input$pretty_plot_update_table_out_rows_selected)
    val.pretty.toplot.update(vals$pretty.params.toplot[[x]])

    modalDialog(
      tags$h4("Stufff"),
      tags$h5("Select the parameter you wish to update and change it as desired in the window that appears.",
              "Then click 'Save parameter', and the newly saved parameter will be updated in the table below.",
              "After the table reflects the desired parameter values, click 'Done'."),
      tags$br(),
      tags$strong(paste("Map ID:", vals$pretty.params.toplot[[x]]$id)),
      fluidRow(
        column(6, uiOutput("pretty_plot_toplot_update_which_uiOut_select"))
        # column(
        #   width = 6,
        #   uiOutput("pretty_plot_addobj_update_thing_uiOut_mult"),
        #   uiOutput("pretty_plot_addobj_update_thing2_uiOut_mult")
        # )
      ),
      # actionButton("pretty_plot_addobj_update_execute", "Save parameter"),
      # tags$br(), tags$br(), tags$br(),
      # tags$h5("Saved parameters for selected additional object. The color values will be 'NA' if transparent;",
      #         "otherwise they are displayed as hexadecimals."),
      # tableOutput("pretty_plot_addobj_update_table_out"),

      footer = tagList(actionButton("pretty_plot_toplot_update_done", "Done"))
    )
  }
}


###############################################################################
# renderUI()'s

### Main selection dropdown
output$pretty_plot_toplot_update_which_uiOut_select <- renderUI({
  choices.list <- list(
    "Map coordinate system and range" = 1,
    "Background color and prediction color scheme" = 2,
    "Legend" = 3,
    "Title and axis labels" = 4,
    "Coordinate grid lines and labels" = 5,
    "Additional objects (points or polygons)" = 6
  )

  selectInput("pretty_plot_toplot_update_which",
              tags$h5("Choose parameter section"),
              choices = choices.list, selected = 1)
})

### Selection dropdown for specific parameters
output$pretty_plot_toplot_update_which_param_uiOut_select <- renderUI({
  req(input$pretty_plot_toplot_update_which)

  selectInput("pretty_plot_toplot_update_which_param",
              tags$h5("Choose parameter to update"),
              choices = list("TODO" = 1), selected = 1)
})



###############################################################################
# Final processing step and close modal
observeEvent(input$pretty_plot_toplot_update_done, {
  removeModal()

  # z <- req(input$pretty_plot_addobj_update_which)
  # x <- input$pretty_plot_addobj_table_out_rows_selected
  #
  # vals$pretty.addobj[[x]] <- val.pretty.addobj.update()
  # val.pretty.addobj.update(NULL)
})

###############################################################################
