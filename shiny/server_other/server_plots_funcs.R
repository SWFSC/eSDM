## Non-reactive plotting functions


###############################################################################
### General function for lat/long spplot
preview.ll <- function(spdf.ll, data.name, title.ll, perc, 
                       axis.cex, main.cex) {
  ### Get extent of plot and where axis labels should go and create scales list
  scales.list <- list(draw = TRUE, alternating = 1, tck = c(1, 0), 
                      cex = axis.cex)
  
  x.extent <- c(extent(spdf.ll)@xmin, extent(spdf.ll)@xmax)
  y.extent <- c(extent(spdf.ll)@ymin, extent(spdf.ll)@ymax)
  
  # browser()
  if (diff(x.extent) > 10) {
    x.list <- list(at = seq(mround(x.extent[1], 5, floor.use = TRUE), 
                            mround(x.extent[2], 5, ceiling.use = TRUE), 
                            by = 5))
    
    scales.list <- c(scales.list, x = list(x.list))
  }
  
  if (diff(y.extent) > 10) {
    y.list <- list(at = seq(mround(y.extent[1], 5, floor.use = TRUE), 
                            mround(y.extent[2], 5, ceiling.use = TRUE), 
                            by = 5))
    
    scales.list <- c(scales.list, y = list(y.list))
  }
  
  ### Generate plot with densities color-coded by percentages or values
  if(perc == 1) {
    b.model <- suppressWarnings(breaks.calc(spdf.ll@data[,data.name]))
    
    spplot(spdf.ll, zcol = data.name, 
           col = NA, col.regions = col.ramp, at = b.model, 
           main = list(label = title.ll, cex = main.cex),
           colorkey = list(space = "right", col = col.ramp, at = breaks,
                           labels = list(labels = labels.lab,
                                         at = labels.at),
                           width = 1, axis.text = list(cex = axis.cex)),
           scales = scales.list)
    
  } else {
    data.vec <- spdf.ll@data[data.name]
    b.model <- seq(from = min(data.vec, na.rm = TRUE), 
                   to = max(data.vec, na.rm = TRUE), 
                   length.out = 11)
    
    spplot(spdf.ll, zcol = data.name, 
           col = NA, col.regions = col.ramp, at = b.model, 
           main = list(label = title.ll, cex = main.cex),
           colorkey = list(space = "right", col = col.ramp, at = breaks,
                           labels = list(labels = rev(round(b.model, 5)),
                                         at = labels.at), 
                           width = 1, axis.text = list(cex = axis.cex)), 
           scales = scales.list)
  }
}


###############################################################################
# Multiplot functions that return gtables

### Produce gtable for single- or multi-plot for display
plot.multi.display <- function(list.data.display) {
  models.num <- length(list.data.display$models.toplot)
  
  # Set variables with plot parameters
  plot.ncol <- case_when(
    models.num == 1 ~ 1, 
    models.num == 2 ~ 2, 
    models.num == 3 ~ 3, 
    models.num == 4 ~ 2, 
    models.num <= 9 ~ 3, 
    TRUE ~ ceiling(sqrt(models.num))
  )
  plot.nrow <- case_when(
    models.num <= 3 ~ 1, 
    models.num <= 6 ~ 2, 
    models.num <= 9 ~ 3, 
    TRUE ~ ceiling(sqrt(models.num))
  )
  
  axis.cex.curr <- case_when(
    models.num == 1 ~ 1.2, 
    models.num <= 3 ~ 0.9, 
    TRUE ~ 0.6
  )
  main.cex.curr <- case_when(
    models.num == 1 ~ 1.6, 
    TRUE ~ 1.0
  )
  
  x.tick.num <- 5
  y.tick.num <- 5
  
  
  # Generate gtable object of plot(s)
  list.spplots <- mapply(function(model.toplot.curr, plot.title.curr) {
    preview.ll(model.toplot.curr, list.data.display$data.name, 
               plot.title.curr, list.data.display$perc.num, 
               axis.cex.curr, main.cex.curr)
  }, list.data.display$models.toplot, list.data.display$plot.titles, 
  SIMPLIFY = FALSE)
  
  arrangeGrob(grobs = list.spplots, nrow = plot.nrow, ncol = plot.ncol)
}


###########################################################
### Produce gtable for single- or multi-plot for download
plot.multi.download <- function(list.data.download) {
  models.num <- length(list.data.download$models.toplot)
  
  # Set variables with plot parameters
  plot.ncol <- case_when(
    models.num == 1 ~ 1, 
    models.num <= 4 ~ 2, 
    models.num <= 9 ~ 3, 
    TRUE ~ ceiling(sqrt(models.num))
  )
  plot.nrow <- case_when(
    models.num <= 2 ~ 1, 
    models.num <= 6 ~ 2, 
    models.num <= 9 ~ 3, 
    TRUE ~ ceiling(sqrt(models.num))
  )
  
  axis.cex.curr <- case_when(
    models.num == 1 ~ 0.8, 
    models.num <= 4 ~ 0.3, 
    TRUE ~ 0.2
  )
  main.cex.curr <- case_when(
    models.num == 1 ~ 1.0, 
    models.num == 2 ~ 0.5, 
    TRUE ~ 0.4
  )
  
  x.tick.num <- 5
  y.tick.num <- 5
  
  # Generate gtable object of plot(s)
  list.spplots <- mapply(function(model.toplot.curr, plot.title.curr) {
    preview.ll(model.toplot.curr, list.data.download$data.name, 
               plot.title.curr, list.data.download$perc.num, 
               axis.cex.curr, main.cex.curr)
  }, list.data.download$models.toplot, list.data.download$plot.titles, 
  SIMPLIFY = FALSE)
  
  arrangeGrob(grobs = list.spplots, nrow = plot.nrow, ncol = plot.ncol)
}

###############################################################################
