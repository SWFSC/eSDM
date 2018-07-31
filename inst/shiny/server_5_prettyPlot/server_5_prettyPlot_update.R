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
# Collects all widget values when map is added to to-plot list
pretty_plot_update_prep <- reactive({
  list(
    proj.ll     = input$pretty_plot_proj_ll,
    proj.method = input$pretty_plot_proj_method,
    proj.idx    = input$pretty_plot_proj_idx,
    proj.epsg   = input$pretty_plot_proj_epsg,
    range.xmin  = input$pretty_plot_range_xmin,
    range.xmax  = input$pretty_plot_range_xmax,
    range.ymin  = input$pretty_plot_range_ymin,
    range.ymax  = input$pretty_plot_range_ymax,

    color.bg = input$pretty_plot_background_color,
    cs.type  = input$pretty_plot_color_perc,
    cs.pal   = input$pretty_plot_color_palette,
    cs.num   = input$pretty_plot_color_num
  )
})


###############################################################################
# Update widgets to reflect current parameter values
pretty_plot_update <- eventReactive(
  input$pretty_plot_update_table_out_rows_selected,
  {
    validate(
      need(input$pretty_plot_update_table_out_rows_selected,
           "Please select a map from the to-plot list to update parameters")
    )

    # browser()
    x <- vals$pretty.params.update

    updateCheckboxInput(session, "pretty_plot_proj_ll", selected = x$proj.ll)
    updateRadioButtons(session, "pretty_plot_proj_method", selected = x$proj.method)
    updateSelectInput(session, "pretty_plot_proj_idx", selected = x$proj.idx)
    updateNumericInput(session, "pretty_plot_proj_epsg", selected = x$proj.epsg)
    updateNumericInput(session, "pretty_plot_range_xmin", selected = x$range.xmin)
    updateNumericInput(session, "pretty_plot_range_xmax", selected = x$range.xmax)
    updateNumericInput(session, "pretty_plot_range_ymin", selected = x$range.ymin)
    updateNumericInput(session, "pretty_plot_range_ymax", selected = x$range.ymax)

    ""
  }
)


###############################################################################


###############################################################################
