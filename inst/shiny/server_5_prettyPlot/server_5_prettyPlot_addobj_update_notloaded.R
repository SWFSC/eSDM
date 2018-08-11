#------------------------------------------------------------------------------
# Update widgets when item is selected from table

###############################################################################
###############################################################################
###############################################################################

observe({
  # input$pretty_plot_addobj_which
  # input$pretty_plot_addobj_type
  input$pretty_plot_addobj_type
  input$pretty_plot_addobj_color_ptfillcheck
  input$pretty_plot_addobj_color_ptfill
  input$pretty_plot_addobj_color_absbordercheck
  input$pretty_plot_addobj_color_absborder
  input$pretty_plot_addobj_pchlty
  input$pretty_plot_addobj_cexlwd
})

#-------------
observeEvent(input$pretty_plot_addobj_table_out_rows_selected, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]

  if (x$obj.text == "Study area polygon") {
    obj.which <- 1
  } else if (x$obj.text == "Erasing polygon") {
    obj.which <- 2
  } else if (x$obj.text == "Validation data points") {
    obj.which <- 3
  } else {
    obj.which <- 4
  }

  updateSelectInput(session, "pretty_plot_addobj_which", selected = obj.which)
})

#------------- not reactive
observeEvent(input$pretty_plot_addobj_table_out_rows_selected, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  updateSelectInput(session, "pretty_plot_addobj_own_type", selected = x$obj.own)
})

#-------------
observeEvent(input$pretty_plot_addobj_which, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  updateRadioButtons(session, "pretty_plot_addobj_type", selected = x$obj.type)})

#-------------not reactive
observeEvent(input$pretty_plot_addobj_table_out_rows_selected, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  updateRadioButtons(session, "pretty_plot_addobj_order", selected = ifelse(x$pre.sdm, 1, 2))
})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  col1.na <- is.na(x$col.ptfill)
  updateCheckboxInput(session, "pretty_plot_addobj_color_ptfillcheck", value = col1.na)
})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  col1 <- ifelse(is.na(x$col.ptfill), "black", x$col.ptfill)
  colourpicker::updateColourInput(session, "pretty_plot_addobj_color_ptfill", value = col1)
})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  col2.na <- is.na(x$col.absborder)
  updateCheckboxInput(session, "pretty_plot_addobj_color_absbordercheck", value = col2.na)
})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  col2 <- ifelse(is.na(x$col.absborder), "black", x$col.absborder)
  colourpicker::updateColourInput(session, "pretty_plot_addobj_color_absborder", value = col2)

})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  updateSelectInput(session, "pretty_plot_addobj_pchlty", selected = x$pchlty)
})

#-------------
observeEvent(input$pretty_plot_addobj_type, {
  req(input$pretty_plot_addobj_table_out_rows_selected)
  x <- vals$pretty.addobj[[input$pretty_plot_addobj_table_out_rows_selected]]
  updateNumericInput(session, "pretty_plot_addobj_cexlwd", value = x$cexlwd)
})


###############################################################################
###############################################################################
###############################################################################
