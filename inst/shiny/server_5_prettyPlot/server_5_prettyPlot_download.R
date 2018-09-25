# Download plots from 'High Quality Maps' tab


###############################################################################
# Download handler - High Quality Maps
output$pretty_download_execute <- downloadHandler(
  filename = function() input$pretty_download_name,

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
      # Check that file extension is as expected
      ext.curr <- switch(
        as.numeric(input$pretty_download_format), ".jpg", ".pdf", ".png"
      )
      req(substr_right(file, 4) == ext.curr)


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
  file.ext <- switch(
    input$pretty_download_format,
    "1" = ".jpg", "2" = ".pdf", "3" = ".png"
  )

  if (length(maps.selected) == 1) {
    req(maps.selected <= length(vals$pretty.params.toplot$id))
    id.txt <- paste(
      unlist(strsplit(vals$pretty.params.toplot[[maps.selected]]$id, " ")),
      collapse = "_"
    )
    f.val <- paste0("eSDM_", id.txt, res.txt, file.ext)

  } else {
    f.val <- paste0("eSDM_map_", res.txt, file.ext)
  }

  textInput("pretty_download_name", tags$h5("File name"), value = f.val)
})

### Button
output$pretty_download_execute_uiOut_download <- renderUI({
  req(vals$pretty.params.toplot)

  row.sel.len <- length(input$pretty_toplot_table_out_rows_selected)
  plot.nrow <- input$pretty_nrow
  plot.ncol <- input$pretty_ncol

  validate(
    need(row.sel.len > 0,
         "Select at least one saved map to download")
    %then%
      need(inherits(plot.nrow, "integer") & inherits(plot.ncol, "integer"),
           paste("'Number of rows' and 'Number of columns'",
                 "must be whole numbers to download a map"))
    %then%
      need((plot.nrow * plot.ncol) >= row.sel.len,
           paste("'Number of rows' * 'Number of columns' must be",
                 "greater than or equal to the number of selected map(s)")),
    errorClass = "validation2"
  )

  downloadButton("pretty_download_execute", "Download map(s)")
})

###############################################################################
