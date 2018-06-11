### Plotting code for 'High Quality Maps' tab
# NOTE: plot will briefly 'regenerate if screen width changes, i.e.
# if the scroll down bar appears


###############################################################################
### Non-reactive plotting function for prettyPlot
# called in server_render.R
plot_pretty <- function(model.toplot, data.name, plot.lim,
                        title.ll, lab.x, lab.y,
                        title.cex, lab.cex, axis.cex, axis.tcl,
                        list.background, list.colorscheme, list.sp.layout) {
  l.cs <- list.colorscheme

  if (l.cs$leg.inc) {
    layout(matrix(1:2, ncol = 2), width = c(1, lcm(3.6)))
  }

  # plot(st_geometry(model.toplot))
  plot(
    list.background[[1]], axes = TRUE, border = NA, col = list.background[[2]],
    xlim = plot.lim[1:2], ylim = plot.lim[3:4],
    main = title.ll, cex.main = title.cex,
    cex.lab = lab.cex, cex.axis = axis.cex, tcl = axis.tcl,
    xlab = lab.x, ylab = lab.y,
    key.pos = NULL, reset = FALSE
  )

  # This is separate for the sake of background and plotting x and y axis labels
  plot(
    model.toplot[data.name], add = TRUE, border = NA,
    breaks = l.cs$data.breaks, pal = l.cs$color.palette,
    key.pos = NULL, reset = FALSE
  )

  # Plot legend if necessary
  if (l.cs$leg.inc) {
    col.num <- length(l.cs$color.palette)
    col.pal <- l.cs$color.palette
    leg.labels <- l.cs$labels
    opar <- par(mai = c(1, 0, 0.86, 1.2))
    on.exit(par(opar), add = TRUE)

    graphics::image(0.6, 1:col.num, t(as.matrix(1:col.num)), col = col.pal,
                    axes = FALSE, xlab = "", ylab = "")
    graphics::box(col = "black")
    # temp <- ifelse(models.num == 1, 0.8, 1.3) # not sure why this is necessary
    graphics::axis(4, at = 1:col.num, labels = leg.labels, tick = FALSE,
                   las = 1, cex.axis = 1)
  }
}


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

    # browser()
    model.toplot <- suppressMessages(
      st_intersection(list.selected[[3]], pretty_plot_range_poly()[[2]])
    )
    data.name <- switch(list.selected[[1]],
                        "Pred", "Pred.overlaid", "Pred.ens")
    plot.lim <- pretty_plot_range_poly()[[1]]

    title.ll  <- input$pretty_plot_title
    title.cex <- input$pretty_plot_title_cex
    lab.x <- input$pretty_plot_xlab
    lab.y <- input$pretty_plot_ylab
    lab.cex  <- input$pretty_plot_lab_cex
    axis.cex <- input$pretty_plot_tick_label_size
    axis.tcl <- input$pretty_plot_tick_length * -0.5

    list.background <- list(pretty_plot_range_poly()[[2]],
                            input$pretty_plot_background_color)
    list.colorscheme <- pretty_plot_colorscheme_list()
    # list.scales <- pretty_plot_scales_list()
    incProgress(0.2)
    list.sp.layout <- NULL#pretty_plot_splayout_list()
    incProgress(0.3)
  })

  # Save plot parameters to reactive values
  params.list <- list(
    model.toplot = model.toplot, data.name = data.name, plot.lim = plot.lim,
    title.ll = title.ll, lab.x = lab.x, lab.y = lab.y,
    title.cex = title.cex, lab.cex = lab.cex, axis.cex = axis.cex,
    axis.tcl = axis.tcl, list.background = list.background,
    list.colorscheme = list.colorscheme, #list.scales = list.scales,
    list.sp.layout = list.sp.layout
  )

  vals$pretty.params.list <- params.list
  vals$pretty.plotted.idx <- pretty_plot_models_idx_list()

  ""
})

###############################################################################
