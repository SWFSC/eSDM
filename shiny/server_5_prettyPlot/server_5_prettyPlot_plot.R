### Plotting code for 'High Quality Maps' tab
# NOTE: plot will briefly 'regenerate if screen width changes, i.e. 
# if the scroll down bar appears


###############################################################################
### Non-reactive plotting function for prettyPlot
plot.pretty.ll <- function(spdf.ll, data.name, plot.lim, title.ll, 
                           axis.xlab, axis.ylab, title.cex, axis.cex, 
                           list.colorscheme, list.scales, list.sp.layout)
{
  # Variables from server_funcs+vars.R used: col.ramp, breaks, labels.lab, labels.at
  l.cs <- list.colorscheme
  
  #####################################
  ## Do needed calculations - hopefully none
  
  
  #####################################
  ## Generate spplot
  spplot(spdf.ll, zcol = data.name, 
         col = NA, at = l.cs$data.breaks, col.regions = l.cs$color.palette, 
         xlim = plot.lim[1:2], ylim = plot.lim[3:4], 
         main = list(label = title.ll, cex = title.cex), 
         xlab = list(label = axis.xlab, cex = axis.cex), 
         ylab = list(label = axis.ylab, cex = axis.cex), 
         colorkey = l.cs$list.colorkey, 
         scales = list.scales, 
         sp.layout = list.sp.layout
  )
  
  # sp.layout = list(world.layer, states.layer, states.lab.layer)
}



# spplot(x, zcol = "Pred.overlaid", 
#        scales = list(draw = TRUE, alternating = c(1, 2), tck = c(1, 1),  
#                      x = list(at = -122:-118, cex = c(1,2,3,4,5))))
# 


###############################################################################
# Reactive plotting functions

###########################################################
### Reactive function to generate plot from reactiveValues
pretty_plot_generate <- reactive({
  req(length(vals$pretty.params.list) > 0)

  p.list <- vals$pretty.params.list
  
  plot.pretty.ll(p.list$spdf.ll, p.list$data.name, p.list$plot.lim, 
                 p.list$title.ll, p.list$axis.xlab, p.list$axis.ylab, 
                 p.list$title.cex, p.list$axis.cex, 
                 p.list$list.colorscheme, p.list$list.scales, 
                 p.list$list.sp.layout)
})


###########################################################
### Event to update reactive variables
pretty_plot_values_event <- eventReactive(input$pretty_plot_execute, {
  validate(
    need(pretty_plot_models_idx_count() > 0, 
         "Please select at least one set of model predictions to plot") %then%
      need(pretty_plot_models_idx_count() == 1, 
           "Can only handle plotting one pretty plot for now")
  )
  
  # Set plotting variables
  withProgress(message = "Processing specified map parameters", value = 0.6, {
    list.selected <- pretty_plot_models_toplot()
    
    spdf.ll <- intersect(list.selected[[3]], pretty_plot_range_poly()[[2]])
    data.name <- switch(list.selected[[1]], 
                        "Pred", "Pred.overlaid", "Pred.ens")
    plot.lim <- pretty_plot_range_poly()[[1]]
    
    title.ll  <- input$pretty_plot_title
    title.cex <- input$pretty_plot_title_cex
    axis.xlab <- input$pretty_plot_xlab
    axis.ylab <- input$pretty_plot_ylab
    axis.cex  <- input$pretty_plot_lab_cex
    
    list.colorscheme <- pretty_plot_colorscheme_list()
    list.scales <- pretty_plot_scales_list()
    incProgress(0.2)
    list.sp.layout <- pretty_plot_splayout_list()
    incProgress(0.2)
  })
  
  # Save plot parameters to reactive values
  params.list <- list(spdf.ll = spdf.ll, data.name = data.name, 
                      plot.lim = plot.lim, title.ll = title.ll, 
                      axis.xlab = axis.xlab, axis.ylab = axis.ylab, 
                      title.cex = title.cex, axis.cex = axis.cex, 
                      list.colorscheme = list.colorscheme, 
                      list.scales = list.scales, 
                      list.sp.layout = list.sp.layout)
  
  vals$pretty.params.list <- params.list
  vals$pretty.plotted.idx <- pretty_plot_models_idx_list()
  
  return("")
})

###############################################################################
