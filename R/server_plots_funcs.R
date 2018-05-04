## Non-reactive plotting functions


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
  
  # TODO make this smart?
  x.tick.num <- 5
  y.tick.num <- 5
  
  
  # # Generate gtable object of plot(s)
  # list.sfplots <- mapply(function(model.toplot.curr, plot.title.curr) {
  #   preview.ll(model.toplot.curr, list.data.display$data.name, 
  #              plot.title.curr, list.data.display$perc.num, 
  #              axis.cex.curr, main.cex.curr)
  # }, list.data.display$models.toplot, list.data.display$plot.titles, 
  # SIMPLIFY = FALSE)
  # 
  # arrangeGrob(grobs = list.sfplots, nrow = plot.nrow, ncol = plot.ncol)
  
  # browser()
  
  # # Layout prep
  # if (input$model_preview_legend) {
  #   mat.num <- c(1:list.data.display$models.num, 
  #                rep(1 + list.data.display$models.num, plot.nrow))
  #   lay.w <- c(rep((0.95 / plot.ncol), plot.ncol), 0.05)
  #   layout(matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol + 1), 
  #          width = lay.w)
  # } else {
  #   mat.num <- 1:list.data.display$models.num
  #   layout(matrix(mat.num, nrow = plot.nrow, ncol = plot.ncol))
  # }
  # 
  # # Plot SDM previews
  # for (i in 1:list.data.display$models.num) {
  #   preview.ll(list.data.display$models.toplot[[i]],
  #              list.data.display$data.name,
  #              list.data.display$plot.titles[[i]],
  #              list.data.display$perc.num,
  #              axis.cex.curr, main.cex.curr)
  # }
  # 
  # # Plot legend
  # if (input$model_preview_legend) {
  #   .image_scale(1:11, col = col.ramp, key.pos = 4, key.length = 1, key.width = 0.5, 
  #                at = seq(0.5, 10.5, by = 1), labels = rev(labels.lab))
  #   # plot.new()
  #   # legend("top", legend = rev(labels.lab), col = col.ramp, pch = 16, cex = 2)
  # }
  # 
  # # Reset layout
  # layout(1)
  
  mapview(list.data.display$models.toplot[[1]], zcol = "Pred")
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
  layout(matrix(1:list.data.download$models.num, nrow = plot.nrow,
                ncol = plot.ncol, byrow = FALSE))
  for (i in 1:list.data.download$models.num) {
    preview.ll(list.data.download$models.toplot[[i]],
               list.data.download$data.name,
               list.data.download$plot.titles[[i]],
               list.data.download$perc.num,
               axis.cex.curr, main.cex.curr)
  }
  layout(1)
}


###############################################################################
### General function for lat/long spplot
preview.ll <- function(sf.ll, data.name, title.ll, perc, 
                       axis.cex, main.cex) {
  ### Prep: 
  data.vec <- st_set_geometry(sf.ll, NULL)[, data.name]
  
  ### Generate plot with densities color-coded by percentages or values
  if(perc == 1) {
    b.model <- breaks.calc(data.vec)
    # b.model[1] <- b.model[1] - 0.1 # so that left.open = FALSE
    # temp <- findInterval(data.vec, b.model, 
    #                      rightmost.closed = TRUE, left.open = FALSE)
    # 
    # plot(st_geometry(sf.ll), axes = TRUE, border = NA, 
    #      col = col.ramp[temp],  
    #      main = title.ll, cex.main = main.cex, cex.axis = axis.cex)
    # legend("bottomleft", legend = labels.lab, col = rev(col.ramp), 
    #        pch = 15, pt.cex = 2, bty = "n", cex = 0.8)
    
    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.ramp, 
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex, 
         key.pos = NULL, reset = FALSE)
    #graticule = st_crs(sf.ll),
  } else {
    b.model <- seq(from = min(data.vec, na.rm = TRUE), 
                   to = max(data.vec, na.rm = TRUE),
                   length.out = 11)
    # temp <- findInterval(data.vec, b.model, 
    #                      rightmost.closed = TRUE, left.open = FALSE)
    # 
    # plot(st_geometry(sf.ll), axes = TRUE, border = NA, 
    #      col = col.ramp[temp],  
    #      main = title.ll, cex.main = main.cex, cex.axis = axis.cex)
    # 
    # legend("bottomleft", legend = labels.lab, col = rev(col.ramp), 
    #        pch = 15, pt.cex = 2, bty = "n")
    
    plot(sf.ll[data.name], axes = TRUE, border = NA,
         breaks = b.model, pal = col.ramp, 
         main = title.ll, cex.main = main.cex, cex.axis = axis.cex, 
         key.pos = NULL, reset = FALSE)
  }
}

###############################################################################
