### Code for 'High Quality Maps' tab for adding parameters to reactive values
# maps 'toplot' aka saved maps. 'toplot' used in various names


###############################################################################
# Reactive plotting functions

###########################################################
### Add data to pretty plot reactive variables
pretty_toplot_add <- eventReactive(input$pretty_toplot_add_execute, {
  validate(
    need(pretty_models_idx_count() > 0,
         paste("Error: Please select at least one set of",
               "predictions to map"))
  )

  if (isTruthy(vals$pretty.toplot.idx)) {
    id.used <- pretty_toplot_table()$ID
    validate(
      need(!(input$pretty_toplot_add_id %in% id.used),
           "Error: each map must have a unique ID")
    )
  }

  validate(
    need(input$pretty_range_xmin,
         paste("Error: Please wait until the parameter inputs below",
               "have finished loading"))
  )

  # Get/set plotting variables
  withProgress(message = "Processing map parameters", value = 0.3, {
    #--------------------------------------------------------------------------
    if (input$pretty_addobj) { # to get check earlier
      validate(
        need(vals$pretty.addobj,
             paste("Error: Please either load additional objects or uncheck",
                   "the 'Include addition objects box'"))
      )
    }

    #--------------------------------------------------------------------------
    model.toplot <- pretty_model_toplot()
    range.poly <- pretty_plot_poly_func(pretty_plot_lim(), st_crs(model.toplot))
    model.int <- suppressMessages(st_intersection(model.toplot, range.poly))
    validate(
      need(nrow(model.int) > 0,
           "Error: The specified map range does not contain any predictions")
    )
    incProgress(0.1)

    x.bbox.lon <- round(unname(st_bbox(model.toplot)), 3)
    if (identical(abs(x.bbox.lon[1]), x.bbox.lon[3])) {
      # This is here so incProgress() can be called
      # Don't need check_preview360() becuase check was already done above
      incProgress(0, detail = "Processing predictions that span dateline")
      model.toplot <- preview360(model.toplot)
      incProgress(0, detail = "")
    }
    incProgress(0.2)

    # st_intersection call moved to _plot.R so map range inputs can be updated
    # model.toplot <- suppressMessages(
    #   st_intersection(model.toplot, pretty_range_poly()[[2]])
    # )

    plot.lim <- pretty_plot_lim()
    background.color <- input$pretty_background_color

    list.colorscheme <- pretty_colorscheme_list()
    list.legend      <- pretty_legend_list()
    list.titlelab    <- pretty_titlelab_list()
    list.margin      <- pretty_margin_list()
    list.tick        <- pretty_tick_list()
    incProgress(0.1)

    list.addobj <- if (input$pretty_addobj) pretty_addobj_list() else NULL
    incProgress(0.3)


    #--------------------------------------------------------------------------
    # Save plot parameters to reactive values
    vals$pretty.params.toplot <- c(
      vals$pretty.params.toplot,
      list(list(
        model.toplot = model.toplot, plot.lim = plot.lim,
        background.color = background.color,
        list.titlelab = list.titlelab, list.margin = list.margin,
        list.tick = list.tick,
        list.colorscheme = list.colorscheme, list.legend = list.legend,
        list.addobj = list.addobj,
        id = input$pretty_toplot_add_id
      ))
    )
    vals$pretty.toplot.idx <- c(
      vals$pretty.toplot.idx, list(pretty_models_idx_list())
    )
  })

  paste0("Saved map '", input$pretty_toplot_add_id, "'")
})


###########################################################
### Table
pretty_toplot_table <- reactive({
  validate(
    need(vals$pretty.toplot.idx, "No maps have been saved"),
    errorClass = "validation2"
  )

  data.frame(
    Predictions = sapply(vals$pretty.toplot.idx, function(i) {
      switch(
        which(!sapply(i, is.null)),
        row.names(table_orig())[i[[1]]],
        row.names(table_overlaid())[i[[2]]],
        row.names(table_ensembles())[i[[3]]]
      )
    }),
    ID = sapply(vals$pretty.params.toplot, function(i) i$id),
    stringsAsFactors = FALSE
  )
})


###########################################################
### Remove stuff from list
pretty_toplot_remove <- eventReactive(input$pretty_toplot_remove_execute, {
  req(vals$pretty.params.toplot)

  x <- input$pretty_update_table_out_rows_selected
  validate(
    need(x, "Error: You must select a saved map to remove")
  )

  vals$pretty.params.toplot <- vals$pretty.params.toplot[-x]
  vals$pretty.toplot.idx <- vals$pretty.toplot.idx[-x]

  if (length(vals$pretty.params.toplot) == 0) vals$pretty.params.toplot <- NULL
  if (length(vals$pretty.toplot.idx) == 0) vals$pretty.toplot.idx <- NULL

  ""
})



###############################################################################
