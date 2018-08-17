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

  # Get/set plotting variables
  withProgress(message = "Processing map parameters", value = 0.5, {
    if (input$pretty_addobj) { # to get check earlier
      validate(
        need(vals$pretty.addobj,
             paste("Error: Please either load additional objects or uncheck",
                   "the 'Include addition objects box'"))
      )
    }

    model.toplot <- suppressMessages(
      st_intersection(pretty_model_toplot(), pretty_range_poly()[[2]])
    )
    plot.lim <- pretty_range_poly()[[1]]
    background.color <- input$pretty_background_color
    incProgress(0.1)

    list.colorscheme <- pretty_colorscheme_list()
    list.legend      <- pretty_legend_list()
    list.titlelab    <- pretty_titlelab_list()
    list.tick        <- pretty_tick_list()
    incProgress(0.1)

    if (input$pretty_addobj) {
      addobj.pre.bool  <- pretty_addobj_preflag()
      list.addobj.pre  <- pretty_addobj_list()[addobj.pre.bool]
      incProgress(0.1)
      list.addobj.post <- pretty_addobj_list()[!addobj.pre.bool]
      incProgress(0.2)

    } else {
      list.addobj.pre  <- list()
      list.addobj.post <- list()
      incProgress(0.3)
    }


    # Save plot parameters to reactive values
    vals$pretty.params.toplot <- c(
      vals$pretty.params.toplot,
      list(list(
        model.toplot = model.toplot, plot.lim = plot.lim,
        background.color = background.color,
        list.titlelab = list.titlelab, list.tick = list.tick,
        list.colorscheme = list.colorscheme, list.legend = list.legend,
        list.addobj.pre = list.addobj.pre, list.addobj.post = list.addobj.post,
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
    need(vals$pretty.toplot.idx,
         "No maps have been saved"),
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
# pretty_toplot_remove <- eventReactive(input$pretty_toplot_remove_execute, {
#   req(vals$pretty.params.list)
#
#   x <- input$pretty_toplot_table_out_rows_selected
#   validate(
#     need(x, "Error: Select at least one row from the to-plot list to remove")
#   )
#
#   vals$pretty.params.list <- vals$pretty.params.list[-x]
#   vals$pretty.toplot.idx <- vals$pretty.toplot.idx[-x]
#
#   if (length(vals$pretty.params.list) == 0) vals$pretty.params.list <- NULL
#   if (length(vals$pretty.toplot.idx) == 0) vals$pretty.toplot.idx <- NULL
#
#   ""
# })



###############################################################################
