### Plotting code for 'High Quality Maps' tab
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
# Reactive plotting functions

###########################################################
### Event to update reactive variables
pretty_plot_values_event <- eventReactive(input$pretty_plot_execute, {
  validate(
    need(pretty_plot_models_idx_count() > 0,
         paste("Error: Please select at least one set of",
               "model predictions to plot")) %then%
      need(pretty_plot_models_idx_count() == 1,
           "Error: App can only handle plotting one pretty plot for now")
  )

  # Set plotting variables
  withProgress(message = "Processing specified map parameters", value = 0.5, {
    list.selected <- pretty_plot_models_toplot()

    model.toplot <- suppressMessages(
      st_intersection(list.selected[[3]], pretty_plot_range_poly()[[2]])
    )
    data.name <- switch(list.selected[[1]],
                        "Pred", "Pred.overlaid", "Pred.ens")
    plot.lim <- pretty_plot_range_poly()[[1]]
    axes.inc <- input$pretty_plot_tick

    title.ll  <- input$pretty_plot_title
    lab.x <- input$pretty_plot_xlab
    lab.y <- input$pretty_plot_ylab

    title.cex <- input$pretty_plot_title_cex
    lab.cex  <- input$pretty_plot_lab_cex
    axis.cex <- input$pretty_plot_tick_label_size
    axis.tcl <- input$pretty_plot_tick_length * -0.5

    list.background <- list(pretty_plot_range_poly()[[2]],
                            input$pretty_plot_background_color)
    list.colorscheme <- pretty_plot_colorscheme_list()
    incProgress(0.2)
    list.addobj <- lapply(vals$pretty.addobj.list, function(i) {
      c(obj.sfc = list(st_geometry(st_transform(i$obj, st_crs(model.toplot)))),
        i[2:5])
    })
    incProgress(0.3)
  })

  # Save plot parameters to reactive values
  params.list <- list(
    model.toplot = model.toplot, data.name = data.name, plot.lim = plot.lim,
    axes.inc = axes.inc, title.ll = title.ll, lab.x = lab.x, lab.y = lab.y,
    title.cex = title.cex, lab.cex = lab.cex, axis.cex = axis.cex,
    axis.tcl = axis.tcl, list.background = list.background,
    list.colorscheme = list.colorscheme, #list.scales = list.scales,
    list.addobj = list.addobj
  )

  vals$pretty.params.list <- params.list
  vals$pretty.plotted.idx <- pretty_plot_models_idx_list()

  ""
})

###############################################################################
