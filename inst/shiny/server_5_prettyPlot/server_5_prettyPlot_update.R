# Code for updating parameters of maps in pretty plot to-plot list


###############################################################################
# Collects all widget values when map is added to to-plot list
pretty_plot_update_prep <- reactive({
  list(

  )
})


###############################################################################
# Update widgets to reflect current parameter values
pretty_plot_update_display <- eventReactive(
  input$pretty_plot_update_table_out_rows_selected,
  {
    req(input$pretty_plot_update_table_out_rows_selected)

    dd
  }
)


###############################################################################


###############################################################################
