### Ccode for 'High Quality Maps' tab for adding parameters to reactive values


###############################################################################
# Reactive plotting functions

###########################################################
### Add data to pretty plotreactive variables
pretty_plot_toplot_add <- eventReactive(input$pretty_plot_toplot_add_execute, {
  validate(
    need(pretty_plot_models_idx_count() > 0,
         paste("Error: Please select at least one set of",
               "model predictions to plot"))
  )

  # Get/set plotting variables
  withProgress(message = "Processing specified map parameters", value = 0.5, {
    list.selected <- pretty_plot_models_toplot()

    model.toplot <- suppressMessages(
      st_intersection(list.selected[[3]], pretty_plot_range_poly()[[2]])
    )
    data.name <- switch(
      list.selected[[1]], "Pred", "Pred.overlaid", "Pred.ens"
    )
    plot.lim <- pretty_plot_range_poly()[[1]]
    axes.inc <- input$pretty_plot_tick

    title.ll  <- input$pretty_plot_title
    lab.x <- input$pretty_plot_xlab
    lab.y <- input$pretty_plot_ylab

    title.cex <- input$pretty_plot_title_cex
    lab.cex  <- input$pretty_plot_lab_cex
    # axis.cex <- input$pretty_plot_tick_label_size
    # axis.tcl <- input$pretty_plot_tick_length * -0.5

    # list.background <- list(pretty_plot_range_poly()[[2]],
    #                         input$pretty_plot_background_color)
    # list.colorscheme <- pretty_plot_colorscheme_list()
    # incProgress(0.2)
    # list.addobj <- lapply(vals$pretty.addobj.list, function(i) {
    #   c(obj.sfc = list(st_geometry(st_transform(i$obj, st_crs(model.toplot)))),
    #     i[2:5])
    # })
    # incProgress(0.3)
  })

  # Save plot parameters to reactive values
  # params.list <- list(
  #   model.toplot = model.toplot, data.name = data.name, plot.lim = plot.lim,
  #   axes.inc = axes.inc, title.ll = title.ll, lab.x = lab.x, lab.y = lab.y,
  #   title.cex = title.cex, lab.cex = lab.cex, axis.cex = axis.cex,
  #   axis.tcl = axis.tcl, list.background = list.background,
  #   list.colorscheme = list.colorscheme, #list.scales = list.scales,
  #   list.addobj = list.addobj
  # )

  params.list <- list(
    model.toplot = model.toplot, data.name = data.name, plot.lim = plot.lim,
    axes.inc = axes.inc, title.ll = title.ll, lab.x = lab.x, lab.y = lab.y,
    title.cex = title.cex, lab.cex = lab.cex,
    id = input$pretty_plot_toplot_add_id
  )
  vals$pretty.params.list <- c(vals$pretty.params.list, list(params.list))
  vals$pretty.toplot.idx <- c(
    vals$pretty.toplot.idx, list(pretty_plot_models_idx_list())
  )

  paste0("'", input$pretty_plot_toplot_add_id, "' added to to-plot list")
})


###########################################################
### Table
pretty_plot_toplot_table <- reactive({
  req(vals$pretty.toplot.idx)

  data.frame(
    Predictions = sapply(vals$pretty.toplot.idx, function(i) {
      switch(
        which(!sapply(i, is.null)),
        row.names(table_orig())[i[[1]]],
        row.names(table_overlaid())[i[[2]]],
        row.names(table_ensembles())[i[[3]]]
      )
    }),
    ID = sapply(vals$pretty.params.list, function(i) i$id),
    stringsAsFactors = FALSE
  )
})


###########################################################
### Remove stuff from list
pretty_plot_toplot_remove <- eventReactive(input$pretty_plot_toplot_remove_execute, {
  req(vals$pretty.params.list)

  x <- input$pretty_plot_toplot_table_out_rows_selected
  validate(
    need(x, "Error: Select at least one row from the to-plot list to remove")
  )

  vals$pretty.params.list <- vals$pretty.params.list[-x]
  vals$pretty.toplot.idx <- vals$pretty.toplot.idx[-x]

  if (length(vals$pretty.params.list) == 0) vals$pretty.params.list <- NULL
  if (length(vals$pretty.toplot.idx) == 0) vals$pretty.toplot.idx <- NULL

  ""
})



###############################################################################
