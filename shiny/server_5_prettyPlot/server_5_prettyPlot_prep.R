### Functions that return data used in server_5_prettyPlot_plot.R
### Get model(s) to plot, generate lists with plotting information


###############################################################################
# Get set(s) of predictions to plot

### Process selected rows from tables of predictions
# Return list of [which tables have selected rows, a 3 element list of the...
# ...rows selected in those tables, a 3 element list of selected spdfs]
# '3 element lists' correspond to the 3 tables
pretty_plot_model_toplot_list <- reactive({
  xyz.null <- pretty_plot_xyz_null()
  xyz.count <- pretty_plot_xyz_count()
  
  req(xyz.count == 1)
  
  validate(
    need(xyz.count == 1, "Pretty plots not ready for multiplots")
  )
  
  table.idx <- which(!xyz.null)
  model.idx.list <- pretty_plot_xyz_list()
  
  models.list.orig <- vals$models.ll[model.idx.list[[1]]]
  models.list.over <- vals$overlaid.models[model.idx.list[[2]]]
  models.list.ens <- vals$ensemble.models[model.idx.list[[3]]]
  
  models.all <- list(models.list.orig, models.list.over, models.list.ens)
  models.all.ll <- lapply(models.all, function(j.list) {
    lapply(j.list, function(j) {
      if (identical(crs(j), crs.ll)) { 
        j
      } else {
        spTransform(j, crs.ll)
      }
    })
  })
  
  list(table.idx, model.idx.list, models.all.ll)
})

### Return list of: (table idx, pred idx, currently selected model predictions)
# Use this function because currently there should only be one model
pretty_plot_model_toplot <- reactive({
  vals <- pretty_plot_model_toplot_list()
  
  table.which <- vals[[1]]
  model.which <- vals[[2]][[table.which]]
  model.spdf <- vals[[3]][[table.which]][[1]]
  
  list(table.which, model.which, model.spdf)
})


###############################################################################
### Compile plot limits, and create a boundary box with whcih to clip model
pretty_plot_range_poly <- reactive({
  plot.lim <- c(input$pretty_plot_range_xmin, input$pretty_plot_range_xmax, 
                input$pretty_plot_range_ymin, input$pretty_plot_range_ymax)

  poly.x <- c(plot.lim[1], plot.lim[1], plot.lim[2], plot.lim[2], plot.lim[1])
  poly.y <- c(plot.lim[3], plot.lim[4], plot.lim[4], plot.lim[3], plot.lim[3])
  
  plot.lim.poly <- SpatialPolygons(list(
    Polygons(list(Polygon(cbind(poly.x, poly.y))), ID = "range")
  ))
  crs(plot.lim.poly) <- crs.ll ## TODO update to plot projected maps
  
  list(plot.lim, plot.lim.poly)
})


###############################################################################
### Generate 'colorscheme' list, aka list of vars that control color scheme
pretty_plot_colorscheme_list <- reactive({
  ### Get reactive elements
  list.selected <- pretty_plot_model_toplot()
  
  perc <- input$pretty_plot_perc
  color.scheme.which <- input$pretty_plot_colorscheme
  color.scheme.num <- input$pretty_plot_colorscheme_num
  leg.include <- input$pretty_plot_legend
  leg.pos <- as.numeric(input$pretty_plot_legend_pos)
  
  spdf.ll <- list.selected[[3]]
  data.name <- switch(list.selected[[1]], "Pred", "Pred.overlaid", "Pred.ens")
  spdf.data <- spdf.ll@data[,data.name]
  
  
  ##### Things that are (mostly) user-controlled
  ### Temp check:
  if (perc) {
    validate(
      need(color.scheme.num == 10, 
           "10 is only current 'Number of colors' implemented when percentage is being plotted- to be fixed soon")
    )
  }
  
  ### Set colorkey inputs
  if (color.scheme.which == 1) {
    validate(
      need(color.scheme.num == 10, 
           "10 is only current 'Number of colors' implemented for \"default blue to red\" color scheme")
    )
    col.ramp.pretty <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0", 
                         "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")
  }
  else if (color.scheme.which == 2) col.ramp.pretty <- rev(RColorBrewer::brewer.pal(color.scheme.num, "Spectral"))
  else if (color.scheme.which == 3) {
    validate(
      need(color.scheme.num <= 9, 
           "RColorBrewer: YlGnBu palette has a max of 9 colors")
    )
    col.ramp.pretty <- rev(RColorBrewer::brewer.pal(color.scheme.num, "YlGnBu"))
  }
  else if (color.scheme.which == 4) col.ramp.pretty <- viridis::viridis(color.scheme.num)
  else if (color.scheme.which == 5) col.ramp.pretty <- viridis::inferno(color.scheme.num)
  else validate(need(FALSE, "Error with selecting color scheme"))
  # x <- dichromat::colorschemes$"DarkRedtoBlue.12"
  
  
  ### Plot percentages vs values
  if (perc) { 
    leg.breaks.pretty <- seq(1, 0, -0.1)
    data.col.num <- 10
    data.breaks <- breaks.calc(spdf.data)
    labels.lab.pretty <- rev(c("Lowest 60%", "35 - 40%", "30 - 35%", 
                               "25 - 30%", "20 - 25%", "15 - 20%", "10 - 15%", 
                               "5 - 10%", "2 - 5%", "Highest 2%"))
    labels.at.pretty <- seq(0.95, 0.05, -0.1)
  } else {
    leg.breaks.pretty <- seq(1, 0, (-1 / color.scheme.num))
    data.col.num <- color.scheme.num
    data.breaks <- seq(min(spdf.data, na.rm = TRUE), 
                       max(spdf.data, na.rm = TRUE), 
                       length.out = (data.col.num + 1))
    labels.lab.pretty <- as.character(data.breaks)
    labels.at.pretty <- seq(0.95, 0.05, (-1 / color.scheme.num))
  }
  
  ### Argument for 'colorkey' in spplot
  if (leg.include) {
    leg.pos.pretty <- switch(leg.pos, "right", "bottom", "left", "top")
    list.colorkey <- list(space = leg.pos.pretty, col = col.ramp.pretty, 
                          at = leg.breaks.pretty,
                          labels = list(labels = labels.lab.pretty, 
                                        at = labels.at.pretty))
  } else {
    list.colorkey <- NULL  
  }
  
  ### Return list
  list(data.breaks = data.breaks, col.ramp.pretty = col.ramp.pretty, 
       list.colorkey = list.colorkey)
})


###############################################################################
### Generate 'scales' list for spplot, aka list of tick/tick label details
pretty_plot_scales_list <- reactive({
  ### If ticks not plotted, done. Else, store other variables
  if (!input$pretty_plot_tick) {
    list.scales <- list(draw = FALSE)
    
  } else {
    scales.tck <- c(input$pretty_plot_tick_length, 0) # tick length
    list.scales <- list(draw = TRUE, tck = scales.tck)
    
    ## If applicable: get manually entered tick locations
    ## Else: use defaults by not providing any x.at or y.at arguments
    if (input$pretty_plot_tick_manual == 2) {
      # Read in tick locations
      x.at <- unlist(strsplit(input$pretty_plot_tick_manual_lon, ", "))
      y.at <- unlist(strsplit(input$pretty_plot_tick_manual_lat, ", "))
      
      # Convert tick locations to numbers
      x.at <- suppressWarnings(as.numeric(x.at))
      y.at <- suppressWarnings(as.numeric(y.at))
      validate(
        need(!anyNA(x.at), 
             paste("Please ensure that the 'Longitude tick locations'", 
                   "entry is valid")), 
        need(!anyNA(y.at), 
             paste("Please ensure that the 'Latitude tick locations'", 
                   "entry is valid"))
      )
      
      # Sort tick locations and check that they are all within map range
      x.at <- sort(x.at)
      y.at <- sort(y.at)
      plot.lim <- pretty_plot_range_poly()[[1]]
      
      validate(
        need(all(findInterval(x.at, plot.lim[1:2]) == 1), 
             paste("Not all 'Longitude tick locations' are within", 
                   "the provided map limits")), 
        need(all(findInterval(y.at, plot.lim[3:4]) == 1), 
             paste("Not all 'Latitude tick locations' are within", 
                   "the provided map limits"))
      )
      
      list.scales <- c(list.scales, list(x = list(at = x.at), 
                                         y = list(at = y.at)))
    }
    
    ## Store tick label details or lack thereof (no labels)
    if (input$pretty_plot_tick_label) {
      tick.lab.size <- c(input$pretty_plot_tick_label_size, 0)
      list.scales <- c(list.scales, list(alternating = 1, cex = tick.lab.size))
      
    } else {
      list.scales <- c(list.scales, list(alternating = 0))
      
    }
  }
  
  list.scales
})

###############################################################################
