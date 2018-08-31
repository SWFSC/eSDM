###############################################################################
### Load Models tab preview download
output$model_download_preview_execute <- downloadHandler(
  filename = function() input$model_download_preview_name,

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      models.idx <- as.numeric(input$models_loaded_table_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$models.ll) > 0, models.num > 0)

      models.toplot <- vals$models.ll[models.idx]
      stopifnot(models.num == length(models.toplot))

      plot.titles <- paste("Original", models.idx)
      perc.num <- as.numeric(input$model_download_preview_perc)
      if (perc.num == 1) {
        pal.download <- pal.esdm
      } else {
        pal.download <- pal.esdm.alt
      }

      x <- session$clientData$output_model_preview_plot_width
      y <- session$clientData$output_model_preview_plot_height

      if (input$model_download_preview_dim == 2) {
        req(x, y)
        plot.dims <- multiplot_inapp(models.num)
        pf.dim1 <- x / 96 * 1.5
        pf.dim2 <- y / 96 * 1.5

      } else {
        plot.dims <- multiplot_download(models.num)
        pf.dim1 <- pf.dim2 <- 8
      }


      #-------------------------------------------------------------
      plot.res <- ifelse(input$model_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$model_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$model_download_preview_format)


      #-------------------------------------------------------------
      if (plot.format == 1) {
        jpeg(file, width = pf.dim1, height = pf.dim2, units = 'in', res = plot.res,
             quality = 150)
        multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = pf.dim1, height = pf.dim2, units = "in", res = plot.res)
        multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
### Create Ensemble Predictions tab preview download
output$ens_download_preview_execute <- downloadHandler(
  filename = function() input$ens_download_preview_name,

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$ensemble.models) > 0, models.num > 0)

      models.toplot <- vals$ensemble.models[models.idx]
      stopifnot(models.num == length(models.toplot))

      plot.titles <- paste("Ensemble", models.idx)
      perc.num <- as.numeric(input$ens_download_preview_perc)
      if (perc.num == 1) {
        pal.download <- pal.esdm
      } else {
        pal.download <- pal.esdm.alt
      }

      x <- session$clientData$output_ens_preview_plot_width
      y <- session$clientData$output_ens_preview_plot_height

      if (input$ens_download_preview_dim == 2) {
        req(x, y)
        plot.dims <- multiplot_inapp(models.num)
        pf.dim1 <- x / 96 * 1.5
        pf.dim2 <- y / 96 * 1.5

      } else {
        plot.dims <- multiplot_download(models.num)
        pf.dim1 <- pf.dim2 <- 8
      }


      #-------------------------------------------------------------
      plot.res <- ifelse(input$ens_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$ens_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$ens_download_preview_format)


      #-------------------------------------------------------------
      if (plot.format == 1) {
        jpeg(file, width = pf.dim1, height = pf.dim2, units = 'in', res = plot.res,
             quality = 150)
        multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = pf.dim1, height = pf.dim2, units = "in", res = plot.res)
        multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10]
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
