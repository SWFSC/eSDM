# Download plots from 'High Quality Maps' tab


###############################################################################
# Download handler - High Quality Maps
output$pretty_download_execute <- downloadHandler(
  filename = function() {
    download_plot_ext(
      input$pretty_download_format, input$pretty_download_name
    )
  },

  content = function(file) {
    withProgress(message = "Downloading high quality map", value = 0.4, {
      #----------------------------------------------------
      plot.which  <- input$pretty_toplot_table_out_rows_selected
      plot.nrow   <- input$pretty_nrow
      plot.ncol   <- input$pretty_ncol
      plot.width  <- input$pretty_width_inch
      plot.height <- input$pretty_height_inch

      req(
        plot.which, (plot.nrow * plot.ncol) >= length(plot.which),
        plot.width > 0, plot.height > 0
      )

      plot.res <- ifelse(input$pretty_download_res == "1", 300, 72)
      plot.format <- input$pretty_download_format
      incProgress(0.2)


      #----------------------------------------------------
      dims.vec <- c(
        nrow = plot.nrow, ncol = plot.ncol, width = plot.width,
        height = plot.height
      )

      tmap.todownload <- plot_pretty_top(
        dims.vec, vals$pretty.toplot.idx[plot.which],
        vals$pretty.params.toplot[plot.which]
      )


      #----------------------------------------------------
      esdm.tmap_options.orig <- tmap_options()$show.messages
      tmap_options(show.messages = FALSE)

      tmap_save(
        tmap.todownload, file, dpi = plot.res,
        width = plot.width, height = plot.height, units = "in"
      )
      incProgress(0.4)

      tmap_options(show.messages = esdm.tmap_options.orig)
      rm(esdm.tmap_options.orig)

      #----------------------------------------------------
    })
  }
)


###############################################################################
# renderUI()'s

### Filename
output$pretty_download_name_uiOut_text <- renderUI({
  req(vals$pretty.params.toplot)

  maps.selected <- input$pretty_toplot_table_out_rows_selected

  res.txt <- ifelse(input$pretty_download_res == 1, "300ppi", "72ppi")

  if (length(maps.selected) == 1) {
    req(maps.selected <= length(vals$pretty.params.toplot))
    id.txt <- paste(
      unlist(strsplit(vals$pretty.params.toplot[[maps.selected]]$id, " ")),
      collapse = "_"
    )
    f.val <- paste0("eSDM_", id.txt, res.txt)

  } else {
    f.val <- paste0("eSDM_map_", res.txt)
  }

  input.lab <- "Filename (without file extension)"
  textInput("pretty_download_name", tags$h5(input.lab), value = f.val)
})

### Download button
output$pretty_download_execute_uiOut_download <- renderUI({
  req(vals$pretty.params.toplot)

  plot.which  <- input$pretty_toplot_table_out_rows_selected
  plot.nrow   <- input$pretty_nrow
  plot.ncol   <- input$pretty_ncol
  plot.width  <- input$pretty_width_inch
  plot.height <- input$pretty_height_inch

  validate(
    need(plot.which,
         "You must select at least one saved map to download"),
    errorClass = "validation2"
  )

  validate(
    need(inherits(plot.nrow, "integer") && inherits(plot.ncol, "integer"),
         paste("'Number of rows' and 'Number of columns'",
               "must be whole numbers")),
    need(isTruthy(plot.width) && isTruthy(plot.height) &&
           is.numeric(plot.width) && is.numeric(plot.height),
         paste("'Plot width (in)' and 'Plot height (in)'",
               "must be numbers")),
    errorClass = "validation2"
  )

  validate(
    need((plot.nrow * plot.ncol) >= length(plot.which),
         paste("'Number of rows' * 'Number of columns' must be",
               "greater than or equal to the number of items",
               "selected from the to-plot list to plot")),
    need(plot.width > 0 && plot.height > 0,
         paste("'Plot width (in)' and 'Plot height (in)' must both",
               "be greater than 0")),
    errorClass = "validation2"
  )

  downloadButton("pretty_download_execute", "Download map(s)")
})

###############################################################################
