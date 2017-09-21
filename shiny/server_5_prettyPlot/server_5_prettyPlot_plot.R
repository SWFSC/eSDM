### Plotting code for 'High Quality Maps' tab
# NOTE: plot will 'regenerate if screen width changes, i.e. 
# if the scroll down bar appears


###############################################################################
### Non-reactive plotting function for prettyPlot
plot.pretty.ll <- function(spdf.ll, data.name, plot.lim, title.ll, 
                           axis.xlab, axis.ylab, title.cex, axis.cex, 
                           list.colorscheme, tick) {
  # Variables from server_funcs+vars.R used: col.ramp, breaks, labels.lab, labels.at
  l.cs <- list.colorscheme

  #####################################
  ## Do needed calculations - hopefully none
  
  
  #####################################
  ## Generate spplot
  spplot(spdf.ll, zcol = data.name, col = NA, 
         at = l.cs$data.breaks, col.regions = l.cs$col.ramp.pretty, 
         xlim = plot.lim[1:2], ylim = plot.lim[3:4], 
         main = list(label = title.ll, cex = title.cex), 
         xlab = list(label = axis.xlab, cex = axis.cex), 
         ylab = list(label = axis.ylab, cex = axis.cex), 
         colorkey = l.cs$list.colorkey, 
         
         scales = list(draw = tick, alternating = 1, tck = c(1,0),
                       x = list(at = c(-130,-125,-120), labels = parse(text = paste(c(130,125,120), "*degree*W", sep = ""))),
                       y = list(at = c(30,35,40,45,50), labels = parse(text = paste(c(30,35,40,45,50), "*degree*N", sep = ""))))
  )
  
  # sp.layout = list(world.layer, states.layer, states.lab.layer)
}


###############################################################################
# Reactive plotting functions

###########################################################
# Plotting steps

### Reactive function to generate plot from reactiveValues
pretty_plot_generate <- reactive({
  req(length(vals$pretty.params.list) > 0)

  p.list <- vals$pretty.params.list
  
  plot.pretty.ll(p.list$spdf.ll, p.list$data.name, p.list$plot.lim, 
                 p.list$title.ll, p.list$axis.xlab, p.list$axis.ylab, 
                 p.list$title.cex, p.list$axis.cex, 
                 p.list$list.colorscheme, 
                 p.list$tick)
})


### Event to update reactive variables
pretty_plot_values_event <- eventReactive(input$pretty_plot_execute, {
  validate(
    need(pretty_plot_xyz_count() > 0, 
         "Please select at least one set of model predictions to plot") %then%
      need(pretty_plot_xyz_count() == 1, 
           "Can only handle plotting one pretty plot for now")
  )
  
  list.selected <- pretty_model_toplot()

  # Set plotting variables
  spdf.ll <- list.selected[[3]]
  data.name <- switch(list.selected[[1]], "Pred", "Pred.overlaid", "Pred.ens")
  plot.lim <- c(input$pretty_plot_range_xmin, input$pretty_plot_range_xmax, 
                input$pretty_plot_range_ymin, input$pretty_plot_range_ymax)
  
  title.ll  <- input$pretty_plot_title
  title.cex <- input$pretty_plot_title_cex
  axis.xlab <- input$pretty_plot_xlab
  axis.ylab <- input$pretty_plot_ylab
  axis.cex  <- input$pretty_plot_lab_cex
  
  list.colorscheme <- pretty_plot_colorscheme()
  
  tick <- input$pretty_plot_tick
  # Determine degree*W/E and degree*N/S
  
  # Save plot parameters to reactive values
  params.list <- list(spdf.ll = spdf.ll, data.name = data.name, 
                      plot.lim = plot.lim, title.ll = title.ll, 
                      axis.xlab = axis.xlab, axis.ylab = axis.ylab, 
                      title.cex = title.cex, axis.cex = axis.cex, 
                      list.colorscheme = list.colorscheme, 
                      tick = tick)
  vals$pretty.params.list <- params.list
  
  return("")
})


###########################################################
### Generate list of variables that control color scheme of predictions
pretty_plot_colorscheme <- reactive({
  list.selected <- pretty_model_toplot()
  perc <- input$pretty_plot_perc
  leg.include <- input$pretty_plot_legend
  leg.pos <- as.numeric(input$pretty_plot_legend_pos)
  
  spdf.ll <- list.selected[[3]]
  data.name <- switch(list.selected[[1]], "Pred", "Pred.overlaid", "Pred.ens")
  spdf.data <- spdf.ll@data[,data.name]
  
  ## Set colorkey inputs
  # Things that will need to be changed to user-controlled
  col.ramp.pretty <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0", 
                       "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")
  leg.breaks.pretty <- seq(1, 0, -0.1)
  data.col.num <- 10
  
  # Things that are (mostly) user-controlled
  if (perc) { 
    data.breaks <- breaks.calc(spdf.data)
    labels.lab.pretty <- rev(c("Lowest 60%", "35 - 40%", "30 - 35%", 
                               "25 - 30%", "20 - 25%", "15 - 20%", "10 - 15%", 
                               "5 - 10%", "2 - 5%", "Highest 2%"))
    labels.at.pretty <- seq(0.95, 0.05, -0.1)
  } else {
    data.breaks <- seq(min(spdf.data, na.rm = T), max(spdf.data, na.rm = T), 
                       length.out = (data.col.num + 1))
    labels.lab.pretty <- as.character(data.breaks)
    labels.at.pretty <- seq(1, 0, -0.1)
  }
  
  ## Argument for 'colorkey' in spplot
  if (leg.include) {
    leg.pos.pretty <- switch(leg.pos, "right", "bottom", "left", "top")
    list.colorkey <- list(space = leg.pos.pretty, col = col.ramp.pretty, 
                          at = leg.breaks.pretty,
                          labels = list(labels = labels.lab.pretty, 
                                        at = labels.at.pretty))
  } else {
    list.colorkey <- NULL  
  }
  
  ## Return list
  list(data.breaks = data.breaks, col.ramp.pretty = col.ramp.pretty, 
       list.colorkey = list.colorkey)
})

###############################################################################
