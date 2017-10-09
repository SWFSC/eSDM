## Non-reactive plotting functions


###############################################################################
### General function for lat/long spplot
plot.ll <- function(spdf.ll, data.name, title.ll, perc, axis.cex, main.cex, 
                    x.tick, y.tick) {
  # Get extent of plot and where axis labels should go
  # x.extent <- c(extent(spdf.ll)@xmin, extent(spdf.ll)@xmax)
  # y.extent <- c(extent(spdf.ll)@ymin, extent(spdf.ll)@ymax)
  # x.at <- mround(seq(x.extent[1], x.extent[2], by = x.tick), x.tick)
  # y.at <- mround(seq(y.extent[1], y.extent[2], by = y.tick), y.tick)
  
  # Generate plot with densities color-coded by percentages
  if(perc == 1) {
    b.model <- suppressWarnings(breaks.calc(spdf.ll@data[,data.name]))
    
    spplot(spdf.ll, zcol = data.name, col = NA, col.regions = col.ramp,
           at = b.model, main = list(label = title.ll, cex = main.cex),
           colorkey = list(space = "right", col = col.ramp, at = breaks,
                           labels = list(labels = labels.lab,
                                         at = labels.at),
                           width = 1, axis.text = list(cex = axis.cex)),
           scales = list(draw = TRUE, alternating = 1, tck = c(1, 0), 
                         # x = list(at = x.at), y = list(at = y.at), 
                         cex = axis.cex)
    )
  }
  # Generate plot with densities color-coded by values
  else {
    spplot(spdf.ll, zcol = data.name, 
           main = list(label = title.ll, cex = main.cex),
           colorkey = list(space = "right", #col = col.ramp, at = breaks,
                           width = 1, axis.text = list(cex = axis.cex)), 
           scales = list(draw = TRUE, alternating = 1, tck = c(1, 0), 
                         # x = list(at = x.at), y = list(at = y.at), 
                         cex = axis.cex)
    )
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
    plot.ll(model.toplot.curr, list.data.display$data.name, 
            plot.title.curr, list.data.display$perc.num, 
            axis.cex.curr, main.cex.curr, 
            x.tick.num, x.tick.num)
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
    plot.ll(model.toplot.curr, list.data.download$data.name, 
            plot.title.curr, list.data.download$perc.num, 
            axis.cex.curr, main.cex.curr, 
            x.tick.num, y.tick.num)
  }, list.data.download$models.toplot, list.data.download$plot.titles, 
  SIMPLIFY = FALSE)
  
  arrangeGrob(grobs = list.spplots, nrow = plot.nrow, ncol = plot.ncol)
}

###############################################################################
